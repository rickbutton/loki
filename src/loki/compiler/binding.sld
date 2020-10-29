(define-library (loki compiler binding)
(import (scheme base))
(import (srfi 128))
(import (srfi 146 hash))
(import (loki core syntax))
(import (loki compiler util))
(export make-binding
        binding?
        binding-type
        binding-name
        binding-levels
        binding-attrs
        binding-module
        binding-attr
        binding-attr!
        binding-attr-set!
        binding-mutable?
        binding-mutable-set!
        binding-dimension
        binding-sequence-counter
        binding-lambda-color
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
  (make-binding type name levels attrs module)
  binding?
  (type binding-type)
  (name binding-name)
  (levels binding-levels)
  (attrs binding-attrs binding-attrs-set!)
  (module binding-module))

(define (binding-attr binding name)
  (hashmap-ref (binding-attrs binding) name (lambda () #f)))
(define (binding-attr! binding name)
  (hashmap-ref (binding-attrs binding) name (lambda () (error "binding doesn't have required attr" binding name))))
(define (binding-attr-set! binding name value)
  (binding-attrs-set! binding (hashmap-set (binding-attrs binding) name value)))

(define (binding-mutable? binding) (binding-attr binding 'mutable?))
(define (binding-mutable-set! binding mutable?) (binding-attr-set! binding 'mutable? mutable?))
(define (binding-dimension binding) (binding-attr! binding 'dimension))
(define (binding-sequence-counter binding) (binding-attr! binding 'sequence-counter))
(define (binding-lambda-color binding) (binding-attr! binding 'lambda-color))

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
