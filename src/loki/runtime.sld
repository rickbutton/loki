(define-library (loki runtime)
(import (scheme base))
(import (scheme file))
(import (scheme write))
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
        ex:library-syntax-defs
        ex:library-bound-vars
        ex:library-forms
        ex:library-build
        ex:library-visited?
        ex:library-invoked?
        ex:library-visited?-set!
        ex:library-invoked?-set!
        ex:make-program
        ex:import-libraries-for
        ex:import-libraries-for-run
        ex:import-main-library
        ex:register-library!
        ex:invoke-library!
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

(define-record-type <program>
    (ex:make-program name imports builds phase)
    program?
    (name program-name)
    (imports program-imports)
    (builds program-builds)
    (phase program-phase))

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

(define (ex:import-main-library name)
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
        (compat-eval `(begin
                      ,@(map (lambda (var) `(define ,var ,ex:undefined)) (ex:library-bound-vars library))
                      ,@(ex:library-forms library)))))

(define (library-name-part->string p)
    (if (symbol? p) (symbol->string p)
                    (number->string p)))

(define (library-name->filename name)
    (find file-exists?
        (map (lambda (dir)
            (string-append 
                (string-join (cons dir (reverse (map library-name-part->string name))) "/") ".sld"))
            ex:library-dirs)))

(define ex:lookup-library 
    (lambda (name)
      (let ((library (find (lambda (l) (equal? name (ex:library-name l))) table)))
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
