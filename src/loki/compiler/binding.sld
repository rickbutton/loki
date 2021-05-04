(define-library (loki compiler binding)
(import (scheme base))
(import (only (srfi 69) hash-by-identity))
(import (srfi 128))
(import (srfi 146 hash))
(import (loki core syntax))
(import (loki compiler util))
(export make-binding
        binding?
        binding-type
        binding-name
        binding-levels
        binding-module
        binding=?
        empty-binding-metadata
        dimension-attrs
        default-attrs
        bound-identifier=?)
(begin

;;=========================================================================
;;
;; Bindings:
;;
;;=========================================================================

;; <binding> ::= (variable         <binding-name> (<level> ...) <attrs { mutable? }>  <module-name>)
;;            |  (macro            <binding-name> (<level> ...) <attrs>  <module-name>)
;;            |  (pattern-variable <binding-name> (<level> ...) <attrs { dimension }> <module-name>)
;;            |  #f  (out of context binding from another module)
;; <mutable> ::= #t | #f
;; <dimension> ::= 0 | 1 | 2 | ...
;; <binding-name> ::= <symbol> uniquely identifying binding.
;; <binding-name> is used for free-identifier=? comparison.
;; For variable and pattern variable bindings, it is the same
;; as the symbol emitted for the binding in the object code.
;; For macro bindings, it is the key for looking up the transformer
;; in the global macro table.

(define-record-type <binding>
  (make-binding type name levels module)
  binding?
  (type binding-type)
  (name binding-name)
  (levels binding-levels)
  (module binding-module))

(define-syntax record-property-equal?
  (syntax-rules ()
    ((_ get a b)
     (equal? (get a) (get b)))))

(define (binding=? x y)
  (and (record-property-equal? binding-type x y)
       (record-property-equal? binding-name x y)
       (record-property-equal? binding-levels x y)
       (record-property-equal? binding-module x y)))

(define *binding-comparator* (make-comparator binding? binding=? #f hash-by-identity))
(define (empty-binding-metadata) (hashmap *binding-comparator*))

(define (empty-attrs) (hashmap (make-default-comparator)))
(define (dimension-attrs dimension) (hashmap-set (empty-attrs) 'dimension dimension))
(define (default-attrs sequence-counter lambda-color)
  (hashmap-set (empty-attrs) 'sequence-counter sequence-counter 'lambda-color lambda-color))

(define (bound-identifier=? x y)
  (check x identifier? 'bound-identifier=?)
  (check y identifier? 'bound-identifier=?)
  (and (eq? (id-name x)
            (id-name y))
       (equal? (id-colors x)
               (id-colors y))))



))
