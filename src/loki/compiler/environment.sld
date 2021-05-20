(define-library (loki compiler environment)
(import (scheme base))
(import (scheme cxr))
(import (srfi 1))
(import (only (srfi 69) hash-by-identity))
(import (srfi 128))
(import (srfi 146 hash))
(import (loki core syntax))
(import (loki compiler util))
(import (loki compiler binding))
(import (loki util))
(export with-reified-env-table
        load-reified-env-table
        environment->import-specs
        make-environment
        imports->environment!
        environment?
        environment-name-set!
        make-unit-env
        make-null-env
        env-extend
        env-extend!
        duplicate?
        env-lookup
        env-reflect
        binding-lookup
        serialize-reified-env-table
        deserialize-reified-env-table)
(begin

;; maps <symbolic key> of reflected environment to actual <environment>
(define *identity-comparator* (make-comparator #t eq? #f hash-by-identity))
(define *env-to-key* (hashmap *identity-comparator*))
(define *key-to-env* (hashmap *identity-comparator*))

(define (env-table-get key) (hashmap-ref/default *key-to-env* key #f))
(define (env-table-set! env)
  (or (hashmap-ref/default *env-to-key* env #f)
      (let ((key (generate-guid 'env)))
        (set! *env-to-key* (hashmap-set *env-to-key* env key))
        (set! *key-to-env* (hashmap-set *key-to-env* key env))
        key)))
(define (env-table-load! key-to-env)
  (hashmap-for-each
    (lambda (key env)
      (set! *env-to-key* (hashmap-set *env-to-key* env key))
      (set! *key-to-env* (hashmap-set *key-to-env* key env)))
    key-to-env))
; TODO, compress this data
(define (env-table-reify key-to-env)
  (hashmap-difference *key-to-env* key-to-env))


(define (with-reified-env-table thunk)
  (let ((env-to-key *env-to-key*)
        (key-to-env *key-to-env*))
    (define (reify-env-table)
      (env-table-reify key-to-env))
    (thunk reify-env-table)))

(define (load-reified-env-table key-to-env)
  (if key-to-env (env-table-load! key-to-env)))

(define (serialize-frame frame)
  (hashmap-map->list (lambda (name binding) (cons name (serialize-binding binding)))
                     frame))
(define (deserialize-frame frame)
  (fold (lambda (entry frame)
    (hashmap-set frame (car entry) (deserialize-binding (cdr entry))))
    (make-null-frame)
    frame))
(define (serialize-env env)
  (map serialize-frame env))
(define (deserialize-env env) (map deserialize-frame env))
(define (serialize-reified-env-table key-to-env)
  (if key-to-env
      (hashmap-map->list (lambda (name env) (cons name (map serialize-env env)))
                         key-to-env)
      #f))
(define (deserialize-reified-env-table key-to-env)
  (if key-to-env
      (fold (lambda (entry key-to-env) (hashmap-set key-to-env (car entry) (map deserialize-env (cdr entry))))
                  (hashmap *identity-comparator*)
                  key-to-env)
      #f))


(define environment-template
  (make-identifier 'environment-template
                   '()
                   '()
                   0
                   '()
                   (make-source "<environment>" 1 0)))

(define (imports->environment-scope imports)
  (let ((alist (map (lambda (import) (cons (car import) (cadr import))) imports)))
    (alist->hashmap (make-default-comparator) alist)))
(define (imports->environment-scope-update! environment imports)
  (let ((alist (map (lambda (import) (cons (car import) (cadr import))) imports)))
    (alist->hashmap! (environment-scope environment) alist)))

(define (environment->import-specs environment)
  (hashmap-map->list
    (lambda (name module-ref)
      (if module-ref
          (datum->syntax environment-template `(import (only ,module-ref ,name)))
          (datum->syntax environment-template `(import (only (loki core primitives) ,name)))))
    (environment-scope environment)))

(define-record-type <environment>
  (make-environment-record scope)
  environment?
  (scope environment-scope environment-scope-set!))

(define (make-environment imports) (make-environment-record (imports->environment-scope imports)))
(define (imports->environment! environment imports)
  (environment-scope-set! environment (imports->environment-scope imports)))
(define (environment-name-set! environment name module-ref)
  (environment-scope-set! environment (hashmap-set! (environment-scope environment) name module-ref))
  environment)


;;=========================================================================
;;
;; Environments:
;;
;;=========================================================================

;; Frames can be added, or the leftmost frame can be destructively
;; updated in the case of binding constructs such as bodies where
;; definitions are incrementally discovered.

(define (make-null-env) '())
(define (make-unit-env) (env-extend '() (make-null-env)))
(define (make-null-frame) (hashmap (make-default-comparator)))

(define (mappings->frame mappings)
  (list (alist->hashmap! (make-null-frame) mappings)))
(define (mappings->frame! frame mappings)
  (set-car! frame (alist->hashmap! (car frame) mappings)))

;; Adds a new frame containing mappings to env.
(define (env-extend mappings env)
  (cons (mappings->frame mappings) env))
;; Destructively extends the leftmost frame in env.
(define (env-extend! mappings env)
  (let ((frame (car env)))
    (mappings->frame! frame mappings)))

;; Is id already bound in leftmost frame?
(define (duplicate? id env)
  (let ((frame (car env)))
    (frame-lookup (cons (id-name id) (id-colors id))
                  frame)))

(define (frame-lookup key frame)
  (hashmap-ref (car frame) key (lambda () #f) (lambda (value) (cons key value))))

(define (env-lookup key env)
  (and (pair? env)
       (or (let* ((frame (car env))
                  (probe (frame-lookup key frame)))
             (and probe
                  (or (cdr probe)
                      (syntax-violation
                       #f "Out of context reference to identifier" (car key)))))
           (env-lookup key (cdr env)))))

;; Looks up binding first in provided environment and
;; then in attached transformer environments.
;; Toplevel forward references are treated specially.
;; Returns <binding> | #f if unbound.

(define (binding-lookup id env)
  (let ((name (id-name id)))
    (let loop ((env    env)
               (envs   (id-transformer-envs id))
               (colors (id-colors id)))
      (or (env-lookup (cons name colors) env)
          (and (pair? envs)
               (loop (env-reify (car envs))
                     (cdr envs)
                     (cdr colors)))))))


;; Returns a single-symbol <key> representing an
;; environment that can be included in object code.
(define (env-reflect env)
  (env-table-set! env))

;; The inverse of the above.
(define (env-reify key-or-env)
  (if (symbol? key-or-env)
      (env-table-get key-or-env)
      key-or-env))

; TODO - make compress, uncompress work for fancier frames
(define (compress env-table)
  env-table
  #;(let ((frame-table '())
        (count 0))
    (for-each (lambda (entry)
                (for-each (lambda (frame)
                            (if (not (assq frame frame-table))
                                (begin
                                  (set! frame-table (cons (cons frame count) frame-table))
                                  (set! count (+ 1 count)))))
                          (cdr entry)))
              env-table)
    (cons (map (lambda (env-entry)
                 (cons (car env-entry)
                       (map (lambda (frame)
                              (cdr (assq frame frame-table)))
                            (cdr env-entry))))
               env-table)
          (map (lambda (frame-entry)
                 (cons (cdr frame-entry)
                       (list (map (lambda (mapping)
                                    (cons (car mapping)
                                          (let ((binding (cdr mapping)))
                                            (case (binding-type binding)
                                              ;; Pattern variable bindings can never be
                                              ;; used in client, so don't waste space.
                                              ;; Should really do the same with all local
                                              ;; bindings, but there are usually much less
                                              ;; of them, so don't bother for now.
                                              ((pattern-variable) #f) ; +++
                                              (else binding)))))
                                  (caar frame-entry)))))
               frame-table))))

(define (uncompress compressed-env-table)
  compressed-env-table
  #;(if (null? compressed-env-table) '()
    (map (lambda (env-entry)
           (cons (car env-entry)
                 (map (lambda (frame-abbrev)
                        (cdr (assv frame-abbrev (cdr compressed-env-table))))
                      (cdr env-entry))))
         (car compressed-env-table))))



))
