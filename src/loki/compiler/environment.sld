(define-library (loki compiler environment)
(import (scheme base))
(import (srfi 128))
(import (srfi 146 hash))
(import (loki reader))
(import (loki compiler util))
(export with-reified-env-table
        load-reified-env-table
        make-environment
        environment?
        environment-import-specs
        environment-toplevel-color
        make-unit-env
        make-null-env
        env-extend
        env-extend!
        duplicate?
        env-lookup
        env-reflect
        binding-lookup)
(begin

;; maps <symbolic key> of reflected environment to actual <environment>
(define *env-table*        '())

(define (with-reified-env-table thunk)
  (let ((table *env-table*))
    (define (reify-env-table)
      (compress (drop-tail *env-table* table)))
    (thunk reify-env-table)))

(define (load-reified-env-table envs)
  (set! *env-table* (append (uncompress envs) *env-table*)))

(define-record-type <environment>
  (make-environment import-specs toplevel-color)
  environment?
  (import-specs environment-import-specs)
  (toplevel-color environment-toplevel-color))

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
  (cond ((and (not (null? *env-table*))      ; +++
              (eq? env (cdar *env-table*)))  ; +++
         (caar *env-table*))                 ; +++
        (else
         (let ((key (generate-guid 'env)))
           (set! *env-table*
                 (cons (cons key env)
                       *env-table*))
           key))))


;; The inverse of the above.
(define (env-reify key-or-env)
  (if (symbol? key-or-env)
      (cdr (assq key-or-env *env-table*))
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
