(define-library (loki runtime)
(import (scheme base))
(import (scheme file))
(import (scheme write))
(import (scheme eval))
(import (loki compat))
(import (loki util))
(export ex:unspecified
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

(define ex:unspecified (if #f #f))

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
(define (ex:import-libraries-for imports builds phase importer run-or-expand)
  (define (import-libraries imports builds phase)
    (for-each (lambda (import build)
                (let ((name   (car import))
                      (levels (cdr import)))
                  (for-each (lambda (level)
                              (import-library name build (+ phase level)))
                            levels)))
              imports
              builds)
    (values))
  (define (import-library name build phase)
    (if (not (member (cons name (cons phase run-or-expand)) ex:imported))
        (let ((library (ex:lookup-library name)))
          (or (not build)
              (eq? build (ex:library-build library))
              (let () 
                (display build) (newline)
                (display (ex:library-build library)) (newline)
              (assertion-violation 
               'import "Client was expanded against a different build of this library" name)))
          (import-libraries (ex:library-imports library) 
                            (ex:library-builds library)
                            phase)
          (importer library phase ex:imported)
          (set! ex:imported (cons (cons name (cons phase run-or-expand)) ex:imported)))))
  (import-libraries imports builds phase))

(define (ex:import-libraries-for-run imports builds phase)
  (ex:import-libraries-for imports 
                           builds
                           phase 
                           (lambda (library phase imported)
                             (if (and (= phase 0)
                                      (not (ex:library-invoked? library)))
                                 (begin 
                                   (ex:invoke-library! library)
                                   (ex:library-invoked?-set! library #t))))
                           'run))

(define (ex:import-library name)
    (let ((library (ex:lookup-library name)))
      (ex:import-libraries-for-run (ex:library-imports library) (ex:library-builds library) 0)))

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
                      ,@(map (lambda (var) `(define ,var ,ex:undefined)) (ex:library-bound-vars library))
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
    (environment '(scheme r5rs) '(loki compat) '(loki runtime) '(loki expander))))

(define (ex:runtime-eval e) 
  (if (not runtime-env) (runtime-env-init!))
  (eval e runtime-env))

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
