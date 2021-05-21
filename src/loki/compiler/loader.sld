(define-library (loki compiler loader)
  (import (scheme base))
  (import (scheme cxr))
  (import (scheme file))
  (import (scheme write))
  (import (loki match))
  (import (loki path))
  (import (loki core reader))
  (import (loki core syntax))
  (import (loki compiler util))
  (import (loki compiler environment))
  (import (loki compiler macro))
  (import (loki compiler runtime))
  (import (loki compiler binding))
  (import (loki compiler lang core))
  (import (loki util))
  (import (srfi 1))
  (import (srfi 69))
  (export 
          import-libraries-for
          import-libraries-for-run
          import-module
          register-module!
          invoke-module!
          lookup-module
          lookup-module/false
          current-builds
          import-libraries-for-expand
          evaluate-macro
          read-module-path
          read-file
          read-relative-include
          module-name->path
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
       ,(map core::serialize (core::module-forms m))
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
   
   (define (resolve-include-path id path)
     (let* ((source (id-source id))
            (root (make-path (if source (source-file source) ""))))
           (path-join (path-parent root) path)))
   
   (define (read-file-from-reader reader)
     (let f ((x (read-annotated reader)))
          (if (and (annotation? x) (eof-object? (annotation-expression x)))
              '()
              (cons x (f (read-annotated reader))))))
   
   (define (read-file fn fold-case?)
     (let* ((path (wrap-path fn))
            (str (path->string path))
            (p (open-input-file str))
            (reader (make-reader p str)))
           (reader-fold-case?-set! reader fold-case?)
           (let ((content (read-file-from-reader reader)))
                content)))
   
   (define (read-relative-include id fn fold-case?)
     (read-file (resolve-include-path id fn) fold-case?))
   
   (define (read-module-path fn thunk)
     (let* ((content (read-file fn #f))
            (output (thunk content))) output))
   
   (define (module-name-part->string p)
     (if (symbol? p) (symbol->string p)
         (number->string p)))
   (define (module-name->path module-name)
     (let ((name (map module-name-part->string module-name)))
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
                   (import-libraries-for (core::module-imported-libraries module)
                                         (core::module-builds module)
                                         phase
                                         importer
                                         run-or-expand)
                   (let ((result (importer module phase)))
                        (phase-table-set! name phase-data)
                        result)))))
   
   (define (importer module phase)
     (if (and (= phase 0)
              (not (core::module-invoked? module)))
         (let ((result (invoke-module! module)))
              (core::module-invoked?-set! module #t)
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
          (import-libraries-for-run (core::module-imported-libraries module) (core::module-builds module) 0)
          (import-module* (core::module-name module) (core::module-build module) 0 importer 'run)))
   
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
   
   
   (define (current-builds imported-libraries)
     (map (lambda (lib-entry)
                  (core::module-build (lookup-module (car lib-entry))))
          imported-libraries))
   
   ;; Register macros in module
   (define (visit-module! module)
     (for-all (lambda (def)
                      (let ((name (car def)) (macro (cdr def)))
                           (if (macro? macro)
                               (register-macro! name macro)
                               (register-macro! name (make-transformer (evaluate-macro macro))))))
              (core::module-syntax-defs module)))
   
   (define (import-libraries-for-expand imports builds phase)
     (import-libraries-for
      imports
      builds
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
   
   (define (evaluate-macro module)
     (register-module! module)
     (core::module-visited?-set! module #f)
     (core::module-invoked?-set! module #f)
     (import-module (core::module-name module)))
   
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
   
   ))
