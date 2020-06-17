(define-library (loki runtime)
(import (scheme base))
(import (scheme write))
(import (scheme eval))
(import (loki compat))
(import (loki util))
(import (loki shared))
(export ex:map-while
        ex:make-library
        ex:library-name
        ex:library-envs
        ex:library-exports
        ex:library-imports
        ex:library-builds
        ex:library-syntax-defs
        ex:library-bound-vars
        ex:library-forms
        ex:library-build
        ex:library-visited?
        ex:library-invoked?
        ex:library-visited?-set!
        ex:library-invoked?-set!
        ex:import-libraries-for
        ex:import-libraries-for-run
        ex:import-library
        ex:register-library!
        ex:invoke-library!
        ex:lookup-library
        ex:lookup-library/false
        ex:runtime-add-primitive
        ex:runtime-eval)
(begin
;;; 
;;; Runtime include file:
;;; Contains the minimal set of binding necessary
;;; for running a fully expanded program.
;;;

(define (util:filter p? lst)
  (if (null? lst)
      '()
      (if (p? (car lst))
          (cons (car lst)
                (util:filter p? (cdr lst)))
          (util:filter p? (cdr lst)))))

(define (ex:map-while f lst k)
  (cond ((null? lst) (k '() '()))
        ((pair? lst)
         (let ((head (f (car lst))))
           (if head
               (ex:map-while f
                          (cdr lst)
                          (lambda (answer rest)
                            (k (cons head answer)
                               rest)))
               (k '() lst))))
        (else  (k '() lst))))

(define-record-type <library>
    (make-library-record name envs exports imports builds syntax-defs bound-vars forms build visited? invoked?)
    ex:library?
    (name        ex:library-name)
    (envs        ex:library-envs)
    (exports     ex:library-exports)
    (imports     ex:library-imports)
    (builds      ex:library-builds)
    (syntax-defs ex:library-syntax-defs)
    (bound-vars  ex:library-bound-vars)
    (forms       ex:library-forms)
    (build       ex:library-build)
    (visited?    ex:library-visited? ex:library-visited?-set!)
    (invoked?    ex:library-invoked? ex:library-invoked?-set!))

(define (ex:make-library name envs exports imports builds syntax-defs bound-vars forms build)
  (make-library-record name envs exports imports builds syntax-defs bound-vars forms build #f #f))

(define ex:imported '())
(define (import-library name build phase importer run-or-expand)
  (if (not (member (cons name (cons phase run-or-expand)) ex:imported))
      (let ((library (ex:lookup-library name)))
        (or (not build)
            (eq? build (ex:library-build library))
            (let () 
              (display build) (newline)
              (display (ex:library-build library)) (newline)
            (assertion-violation 
             'import "Client was expanded against a different build of this library" name)))
        (ex:import-libraries-for (ex:library-imports library) 
                          (ex:library-builds library)
                          phase
                          importer 
                          run-or-expand)
        (importer library phase ex:imported)
        (set! ex:imported (cons (cons name (cons phase run-or-expand)) ex:imported)))))

(define (importer library phase imported)
  (if (and (= phase 0)
    (not (ex:library-invoked? library)))
    (begin 
      (ex:invoke-library! library)
      (ex:library-invoked?-set! library #t))))

(define (ex:import-libraries-for imports builds phase importer run-or-expand)
  (for-each (lambda (import build)
              (let ((name   (car import))
                    (levels (cdr import)))
                (for-each (lambda (level)
                            (import-library name build (+ phase level) importer run-or-expand))
                          levels)))
            imports
            builds)
  (values))
(define (ex:import-libraries-for-run imports builds phase)
  (ex:import-libraries-for imports 
                           builds
                           phase 
                           importer
                           'run))

(define (ex:import-library name)
    (let ((library (ex:lookup-library name)))
      (ex:import-libraries-for-run (ex:library-imports library) (ex:library-builds library) 0)
      (import-library (ex:library-name library) (ex:library-build library) 0 importer 'run)))

(define table '())
(define ex:register-library! 
    (lambda (library)
      (set! table (cons library table))
      (set! ex:imported (util:filter (lambda (entry)
                                  (not (equal? (ex:library-name library) 
                                               (car entry))))
                                ex:imported))))

(define ex:invoke-library!
    (lambda (library)
        (ex:runtime-eval `(begin
                      ,@(map (lambda (var) `(define ,var '(if #f #f))) (ex:library-bound-vars library))
                      ,@(ex:library-forms library)))))

(define ex:lookup-library 
    (lambda (name)
      (let ((library (ex:lookup-library/false name)))
        (if library
            library
            (assertion-violation 'lookup-library "Library not loaded" name)))))

(define ex:lookup-library/false
    (lambda (name)
      (let ((library (find (lambda (l) (equal? name (ex:library-name l))) table)))
        (if library
            library
            #f))))

(define runtime-env #f)

(define (runtime-env-init!)
  (set! runtime-env 
    (environment
     '(scheme case-lambda)
     '(scheme char)
     '(scheme complex)
     '(scheme cxr)
     '(scheme eval)
     '(scheme file)
     '(scheme inexact)
     '(scheme lazy)
     '(scheme load)
     '(scheme process-context)
     '(scheme read)
     '(scheme repl)
     '(scheme time)
     '(scheme write)
     '(scheme base)
     )))

(define (ex:runtime-add-primitive name value)
  (ex:runtime-eval `(define ,name ,value)))

(define (ex:runtime-eval e)
  (if (not runtime-env) (runtime-env-init!))
  (eval e runtime-env))

;; Register the required runtime primitives
(ex:runtime-add-primitive 'void (if #f #f))
(ex:runtime-add-primitive 'ex:map-while ex:map-while)
(ex:runtime-add-primitive 'ex:import-library ex:import-library)
(ex:runtime-add-primitive 'real-vector? vector?)


(ex:runtime-add-primitive '%add        +)
(ex:runtime-add-primitive '%sub        -)
(ex:runtime-add-primitive '%mul        *)
(ex:runtime-add-primitive '%div        /)
(ex:runtime-add-primitive '%lt         <)
(ex:runtime-add-primitive '%lte       <=)
(ex:runtime-add-primitive '%number-eq  =)
(ex:runtime-add-primitive '%gt         >)
(ex:runtime-add-primitive '%gte       >=)

(ex:runtime-add-primitive '%cons      cons)
(ex:runtime-add-primitive '%car       car)
(ex:runtime-add-primitive '%cdr       cdr)

;; Only instantiate part of the bootstrap library 
;; that would be needed for invocation at runtime.

(ex:register-library! 
 (ex:make-library
  '(core primitive-macros)
  ;; envs
  '()
  ;; exports
  '()
  ;; imported-libraries
  '()
  ;; builds
  '()
  ;; syntax-defs
  '()
  ;; bound-vars
  '()
  ;; forms
  '()
  ;; build
  'system))
))
