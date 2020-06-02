(define-library (loki runtime)
(import (scheme r5rs))
(import (loki compat))
(import (loki util))
(export ex:load-hook-set!
        ex:unspecified
        ex:make-library
        ex:library-name
        ex:library-envs
        ex:library-exports
        ex:library-imports
        ex:library-builds
        ex:library-visiter
        ex:library-invoker
        ex:library-build
        ex:library-visited?
        ex:library-invoked?
        ex:library-visited?-set!
        ex:library-invoked?-set!
        ex:import-libraries-for
        ex:import-libraries-for-run
        ex:register-library!
        ex:lookup-library)
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

; TODO - set this at runtime to something that crashes
(define load-hook #f)
(define (ex:load-hook-set! hook)
  (set! load-hook hook))

(define ex:unspecified (if #f #f))

(define (ex:make-library name envs exports imports builds visiter invoker build)
  (list name envs exports imports builds visiter invoker build #f #f))

(define (ex:library-name     lib) (car lib))
(define (ex:library-envs     lib) (cadr lib))
(define (ex:library-exports  lib) (caddr lib))
(define (ex:library-imports  lib) (cadddr lib))
(define (ex:library-builds   lib) (car (cddddr lib)))
(define (ex:library-visiter  lib) (car (cdr (cddddr lib))))
(define (ex:library-invoker  lib) (car (cdr (cdr (cddddr lib)))))
(define (ex:library-build    lib) (car (cdr (cdr (cdr (cddddr lib))))))
(define (ex:library-visited? lib) (car (cdr (cdr (cdr (cdr (cddddr lib)))))))
(define (ex:library-invoked? lib) (car (cdr (cdr (cdr (cdr (cdr (cddddr lib))))))))

(define (ex:library-visited?-set! lib b) (set-car! (cdr (cdr (cdr (cdr (cddddr lib))))) b))
(define (ex:library-invoked?-set! lib b) (set-car! (cdr (cdr (cdr (cdr (cdr (cddddr lib)))))) b))

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
                                   ((ex:library-invoker library))
                                   (ex:library-invoked?-set! library #t))))
                           'run))

(define table '())
(define ex:register-library! 
    (lambda (library)
      (set! table (cons library table))
      (set! ex:imported (util:filter (lambda (entry)
                                  (not (equal? (ex:library-name library) 
                                               (car entry))))
                                ex:imported))))
(define ex:lookup-library 
    (lambda (name)
      (let ((library (assoc name table)))
        (if library
            library
            (let ((filename (library-name->filename name)))
              (if filename
                  (begin
                    (load-hook filename)
                    (ex:lookup-library name))
                  (assertion-violation 'lookup-library "Library not loaded" name)))))))

;; Only instantiate part of the bootstrap library 
;; that would be needed for invocation at runtime.

(ex:register-library! 
 (let ((error (lambda () 
                (assertion-violation 
                 'runtime.scm
                 "Attempt to use runtime instance of (core primitive-macros) for expansion.  Make sure expander.scm is loaded after runtime.scm."))))
   (ex:make-library
    '(core primitive-macros)
    ;; envs
    error
    ;; exports
    '()
    ;; imported-libraries
    '()
    ;; builds
    '()
    ;; visiter
    error
    ;; invoker
    (lambda () (values))
    ;; build
    'system)))
))
