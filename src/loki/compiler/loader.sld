(define-library (loki compiler loader)
  (import (scheme base))
  (import (scheme cxr))
  (import (scheme file))
  (import (scheme write))
  (import (loki match))
  (import (loki path))
  (import (loki fs))
  (import (loki profile))
  (import (loki core reader))
  (import (loki core syntax))
  (import (loki compiler util))
  (import (loki compiler environment))
  (import (loki compiler intrinsics))
  (import (loki compiler expander))
  (import (loki compiler macro))
  (import (loki compiler runtime))
  (import (loki compiler binding))
  (import (loki compiler lang core))
  (import (loki util))
  (import (srfi 1))
  (import (srfi 69))
  (export
   load-module
   load-module-from-cache
   serialize-module
   deserialize-module)
  (begin
   
   (define *module-dirs* '("src"))

   (define (serialize-export e) (cons (car e) (serialize-binding (cdr e))))
   (define (deserialize-export e) (cons (car e) (deserialize-binding (cdr e))))
   (define (serialize-import e)
     (match e
       ((name module-ref binding)
        `(,name ,module-ref ,(serialize-binding binding)))))
   (define (deserialize-import e)
     (match e
       ((name module-ref binding)
        `(,name ,module-ref ,(deserialize-binding binding)))))
   (define (serialize-syntax-def e)
     (cons (car e) (serialize-module (cdr e))))
   (define (deserialize-syntax-def e)
     (cons (car e) (deserialize-module (cdr e))))
   (define (serialize-module m)
     `(module ,(core::module-name m)
       ,(serialize-reified-env-table (core::module-envs m))
       ,(map serialize-export (core::module-exports m))
       ,(map serialize-import (core::module-imports m))
       ,(core::module-imported-libraries m)
       ,(core::module-builds m)
       ,(map serialize-syntax-def (core::module-syntax-defs m))
       ,(core::module->scheme m)
       ,(core::module-build m)))
   (define (deserialize-module m)
     (match m
       (('module name
         env-table
         exports
         imports
         imported-libraries
         builds
         syntax-defs
         forms
         build)
        (core::module name
                      (deserialize-reified-env-table env-table)
                      (map deserialize-export exports)
                      (map deserialize-import imports)
                      imported-libraries
                      builds
                      (map deserialize-syntax-def syntax-defs)
                      (map core::deserialize forms)
                      build))))
      
   
   (define (module-name->path module-name)
     (let ((name (map core::module-name-part->string module-name)))
       (let ((options (map (lambda (dir)
                             (let ((absolute (path-join (current-working-path) dir)))
                               (path-with-suffix (apply path-join absolute name) "sld")))
                           *module-dirs*)))
         (or (find (lambda (path) (file-exists? (path->string path))) options)
             (syntax-violation #f (string-append "File not found for module: " (write-to-string name)) name)))))
   
   (define *phase-table* (make-hash-table))
   (define (make-module-instance-table) (make-hash-table))
   
   (define (phase-table-set! name phase-data)
     (unless (hash-table-exists? *phase-table* name)
       (hash-table-set! *phase-table* name (make-module-instance-table)))
     (hash-table-set! (hash-table-ref *phase-table* name) phase-data #t))
   (define (phase-table-get name phase-data)
     (and (hash-table-exists? *phase-table* name)
          (hash-table-exists? (hash-table-ref *phase-table* name) phase-data)))
   (define (phase-table-delete! name phase-data)
     (if (hash-table-exists? *phase-table* name)
         (begin
          (hash-table-delete! (hash-table-ref *phase-table* name) phase-data)
          (if (= (hash-table-size *phase-table*) 0)
              (hash-table-delete! *phase-table* name)))
       #f))
   (define (phase-table-clear! name)
     (hash-table-delete! *phase-table* name))
   
   (define (import-module* name build phase importer run-or-expand)
     (let ((phase-data (cons phase run-or-expand)))
       (if (not (phase-table-get name phase-data))
           (let ((module (lookup-module name)))
             (or (not build)
                 (eq? build (core::module-build module))
                 (let ()
                   (display build) (newline)
                   (display (core::module-build module)) (newline)
                   (error
                    "Import failed: client was expanded against a different build of this module" name)))
             (import-modules-for (core::module-imported-libraries module)
                                   (core::module-builds module)
                                   phase
                                   importer
                                   run-or-expand)
             (let ((result (importer module phase)))
               (phase-table-set! name phase-data)
               result)))))
   
   (define (run-importer module phase)
     (if (and (= phase 0)
              (not (core::module-invoked? module)))
         (let ((result (invoke-module! module)))
           (core::module-invoked?-set! module #t)
           result)))
   
   (define (import-modules-for imports builds phase importer run-or-expand)
     (for-each (lambda (import build)
                 (let ((name   (car import))
                       (levels (cdr import)))
                   (for-each (lambda (level)
                               (import-module* name build (+ phase level) importer run-or-expand))
                             levels)))
               imports
               builds)
     #f)
   (define (import-modules-for-run imports builds phase)
     (import-modules-for imports
                           builds
                           phase
                           run-importer
                           'run))
 
  (define (import-modules-for-expand imports phase)
     (import-modules-for
      imports
      (map not imports)
      phase
      (lambda (module phase)
        (if (and (>= phase 0)
                 (not (core::module-visited? module)))
            (begin
             (load-reified-env-table (core::module-envs module))
             (visit-module! module)
             (core::module-visited?-set! module #t)))
        (if (and (>= phase 1)
                 (not (core::module-invoked? module)))
            (begin
             (invoke-module! module)
             (core::module-invoked?-set! module #t))))
      'expand))
   
   (define (import-module name)
     (let ((module (lookup-module name)))
       (import-modules-for-run (core::module-imported-libraries module) (core::module-builds module) 0)
       (import-module* (core::module-name module) (core::module-build module) 0 run-importer 'run)))

  (define eval-template
    (make-identifier 'eval-template
                     '()
                     '()
                     0
                     '()
                     (make-source "<eval>" 1 0)))

   (define (loki-environment . import-specs)
     (make-expander-environment with-default-loader import-specs))

   (define (loki-eval exp env)
     (let* ((exp (datum->syntax eval-template exp))
            (import-specs (environment->import-specs env))
            (module (expand-module (list exp)
                                   with-default-loader
                                   import-specs
                                   #t))
            (imports (core::module-imports module))
            (exports (core::module-exports module)))
       (register-module! module)
       (imports->environment! env imports)
       (for-each (lambda (export)
                   (environment-name-set! env (car export) (binding-module (cdr export))))
                 exports)
       (import-module (core::module-name module))))
   
   (define *module-table* (make-hash-table))
   (define register-module!
     (lambda (module)
       (hash-table-set! *module-table* (core::module-name module) module)
       (phase-table-clear! (core::module-name module))))
   
   (define invoke-module!
     (lambda (module)
       (runtime-run-module module)))
   
   (define lookup-module
     (lambda (name)
       (let ((module (lookup-module/false name)))
         (if module
             module
           (error "module lookup failed, module not loaded" name)))))
   
   (define (lookup-module/false name)
     (hash-table-ref/default *module-table* name #f))
   
   ;; Register macros in module
   (define (visit-module! module)
     (for-all (lambda (def)
                (let ((name (car def)) (macro (cdr def)))
                  (if (macro? macro)
                      (register-macro! name macro)
                    (register-macro! name (make-transformer (load-macro-module macro))))))
              (core::module-syntax-defs module)))

  (define (load-module name)
     (or
      (lookup-module/false name)
      (load-module-from-cache (module-name->path name))))

  (define (load-macro-module module)
     (register-module! module)
     (core::module-visited?-set! module #f)
     (core::module-invoked?-set! module #f)
     (import-module (core::module-name module)))
   
   (define (load-module-from-cache path)
     (let* ((path (wrap-path path))
            (path-string (path->string path))
            (cache-path (path->string (path-with-suffix path "so"))))
       (unless (file-exists? path-string)
         (error "file not found" path-string))
       (if (and (file-exists? cache-path)
                (>= (file-mtime cache-path)
                    (file-mtime path-string)))
           (let* ((foo (debug "before deserialize"))
                  (module (deserialize-module (syntax->datum (car (read-file cache-path #f)))))
                  (imported-libraries (core::module-imported-libraries module)))
             (for-each (lambda (imported-module) (load-module (car imported-module)))
                       imported-libraries)
             (register-module! module)
             (import-module (core::module-name module))
             (debug "deserialized module from" cache-path)
             module)
         (let* ((module (expand-module (read-file path #f)
                                       with-default-loader '() #f))
                (serialized (serialize-module module)))
           (register-module! module)
           (import-module (core::module-name module))
           (debug "caching module to" cache-path)
           (with-output-to-file cache-path (lambda () (pretty-print serialized)))
           module))))

   (define (with-default-loader thunk)
     (thunk load-module
            load-macro-module
            import-modules-for-expand))
      
   ;; Only instantiate part of the bootstrap module
   ;; that would be needed for invocation at runtime.
   
   (register-module!
    (core::module
     '(loki core primitive-macros)
     ;; envs
     #f
     ;; exports
     '()
     ;; imports
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
   
    ;;===================================================================
    ;;
    ;; Bootstrap module containing intrinsics defined in (loki compiler intrisics)
    ;;
    ;;===================================================================
   (register-module!
    (core::module
     '(loki core intrinsics)
     ;; envs
     #f
     ;; exports
     (map (lambda (intrinsic)
            (cons intrinsic (make-binding 'variable intrinsic '(0) '(loki core intrinsics))))
          compiler-intrinsics)
     ;; imports
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
   
    ;;===================================================================
    ;;
    ;; Bootstrap module containing macros defined in (loki compiler expander)
    ;;
    ;;===================================================================
   (register-module!
    (core::module
     '(loki core primitive-macros)
     ;; envs
     #f
     ;; exports
     (map (lambda (mapping)
            (cons (car mapping) (make-binding 'macro (car mapping) '(0) '(loki core primitive-macros))))
          primitive-macro-mapping)
     ;; imports
     '()
     ;; imported-libraries
     '()
     ;; builds
     '()
     ;; syntax-defs
     (map (lambda (mapping)
            (cons (car mapping) (make-expander (cdr mapping))))
          primitive-macro-mapping)
     ;; forms
     '()
     ;; build
     'system))
   
   
   (runtime-add-primitive 'ex:load load-module-from-cache)
   (runtime-add-primitive 'ex:eval loki-eval)
   (runtime-add-primitive 'ex:environment loki-environment)
   
   ))
