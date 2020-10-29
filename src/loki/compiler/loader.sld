(define-library (loki compiler loader)
(import (scheme base))
(import (loki core reader))
(import (loki compiler util))
(import (loki compiler environment))
(import (loki compiler macro))
(import (loki compiler runtime))
(import (srfi 1))
(export *module-dirs*
        make-module
        module-name
        module-envs
        module-exports
        module-imports
        module-builds
        module-syntax-defs
        module-forms
        module-build
        module-visited?
        module-invoked?
        module-visited?-set!
        module-invoked?-set!
        import-libraries-for
        import-libraries-for-run
        import-module
        register-module!
        invoke-module!
        lookup-module
        lookup-module/false
        current-builds
        import-libraries-for-expand
        evaluate-macro)
(begin

(define *module-dirs* '("src"))

(define-record-type <module>
    (make-module-record name envs exports imports builds syntax-defs forms build visited? invoked?)
    module?
    (name        module-name)
    (envs        module-envs)
    (exports     module-exports)
    (imports     module-imports)
    (builds      module-builds)
    (syntax-defs module-syntax-defs)
    (forms       module-forms)
    (build       module-build)
    (visited?    module-visited? module-visited?-set!)
    (invoked?    module-invoked? module-invoked?-set!))
(define (make-module name envs exports imports builds syntax-defs forms build)
  (make-module-record name envs exports imports builds syntax-defs forms build #f #f))

(define imported '())
(define (import-module* name build phase importer run-or-expand)
  (if (not (member (cons name (cons phase run-or-expand)) imported))
      (let ((module (lookup-module name)))
        (or (not build)
            (eq? build (module-build module))
            (let () 
              (display build) (newline)
              (display (module-build module)) (newline)
            (error 
             "Import failed: client was expanded against a different build of this module" name)))
        (import-libraries-for (module-imports module) 
                          (module-builds module)
                          phase
                          importer 
                          run-or-expand)
        (let ((result (importer module phase imported)))
          (set! imported (cons (cons name (cons phase run-or-expand)) imported))
          result))))

(define (importer module phase imported)
  (if (and (= phase 0)
    (not (module-invoked? module)))
    (let ((result (invoke-module! module)))
      (module-invoked?-set! module #t)
      result)))

(define (import-libraries-for imports builds phase importer run-or-expand)
  (for-each (lambda (import build)
              (let ((name   (car import))
                    (levels (cdr import)))
                (for-each (lambda (level)
                            (import-module* name build (+ phase level) importer run-or-expand))
                          levels)))
            imports
            builds)
  #f)
(define (import-libraries-for-run imports builds phase)
  (import-libraries-for imports 
                         builds
                         phase 
                         importer
                         'run))

(define (import-module name)
    (let ((module (lookup-module name)))
      (import-libraries-for-run (module-imports module) (module-builds module) 0)
      (import-module* (module-name module) (module-build module) 0 importer 'run)))

(define table '())
(define register-module! 
    (lambda (module)
      (set! table (cons module table))
      (set! imported (filter (lambda (entry)
                                  (not (equal? (module-name module) 
                                               (car entry))))
                                imported))))

(define invoke-module!
    (lambda (module)
        (runtime-run-program (module-forms module))))

(define lookup-module 
    (lambda (name)
      (let ((module (lookup-module/false name)))
        (if module
            module
            (error "module lookup failed, module not loaded" name)))))

(define lookup-module/false
    (lambda (name)
      (let ((module (find (lambda (l) (equal? name (module-name l))) table)))
        (if module
            module
            #f))))


(define (current-builds imported-libraries)
  (map (lambda (lib-entry)
         (module-build (lookup-module (car lib-entry))))
       imported-libraries))

;; Register macros in module
(define (visit-module! module)
  (for-all (lambda (def)
             (let ((name (car def)) (macro (cdr def)))
                (if (macro? macro)
                  (register-macro! name macro)
                  (register-macro! name (make-transformer (evaluate-macro macro))))))
           (module-syntax-defs module)))

(define (import-libraries-for-expand imports builds phase)
  (import-libraries-for
   imports
   builds
   phase
   (lambda (module phase imported)
     (if (and (>= phase 0)
              (not (module-visited? module)))
         (begin
           (load-reified-env-table (module-envs module))
           (visit-module! module)
           (module-visited?-set! module #t)))
     (if (and (>= phase 1)
              (not (module-invoked? module)))
         (begin 
           (invoke-module! module)
           (module-invoked?-set! module #t))))
   'expand))

(define (evaluate-macro module)
  (register-module! module)
  (module-visited?-set! module #f)
  (module-invoked?-set! module #f)
  (import-module (module-name module)))

;; Only instantiate part of the bootstrap module 
;; that would be needed for invocation at runtime.

(register-module! 
 (make-module
  '(loki core primitive-macros)
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
  ;; forms
  '()
  ;; build
  'system))

))
