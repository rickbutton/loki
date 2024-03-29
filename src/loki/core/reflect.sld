(define-library (loki core reflect)
  (import (scheme base))
  (import (srfi 1))
  (export is-a? slot-ref)
  (cond-expand
   (gauche
    (import (only (gauche base) is-a?))
    (import (rename (gauche base) (slot-ref gauche:slot-ref)
                    (slot-bound? gauche:slot-bound?)
                    (slot-set! gauche:slot-set!)))
    (begin
     (define (slot-ref type value slot)
       (let ((slot (if (number? slot)
                       (list-ref (map slot-definition-name (class-slots type)) slot)
                     slot)))
         (if (gauche:slot-bound? value slot)
             (gauche:slot-ref value slot)
           #f)))
     #;(define (slot-set! type value slot)
     (gauche:slot-set! value))))
  (loki
   (import (loki core records))
   (begin
    (define (is-a? value type)
      (and (record? value)
           (eq? (record-type value) type)))
    (define (slot-ref type value slot)
      (if (number? slot)
          (record-ref value (+ slot 1))
        ((record-accessor type 'slot) value)))))))
