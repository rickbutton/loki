(define-library (loki compiler binding)
(import (scheme base))
(import (srfi 128))
(import (srfi 146 hash))
(export make-binding-record
        binding?
        binding-type
        binding-name
        binding-levels
        binding-attrs
        binding-library
        binding-attr
        binding-attr!
        binding-attr-set!
        empty-attrs)
(begin

(define-record-type <binding>
  (make-binding-record type name levels attrs library)
  binding?
  (type binding-type)
  (name binding-name)
  (levels binding-levels)
  (attrs binding-attrs binding-attrs-set!)
  (library binding-library))
(define (binding-attr binding name)
  (hashmap-ref (binding-attrs binding) name (lambda () #f)))
(define (binding-attr! binding name)
  (hashmap-ref (binding-attrs binding) name (lambda () (error "binding doesn't have required attr" binding name))))
(define (binding-attr-set! binding name value)
  (binding-attrs-set! binding (hashmap-set (binding-attrs binding) name value)))
(define (empty-attrs) (hashmap (make-default-comparator)))


))
