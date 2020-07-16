(define-library (loki message)
(import (scheme base))
(import (scheme process-context))
(import (scheme case-lambda))
(import (loki writer))
(export make-loki-error
        make-loki-warning
        make-loki-message
        loki-message?
        loki-message->type
        loki-message->source
        loki-message->message)
(begin

(define-record-type <loki-message>
  (make-loki-message type source message obj)
  loki-message?
  (type loki-message->type)
  (source loki-message->source)
  (message loki-message->message)
  (obj loki-message->obj))

(type-printer-set! <loki-message> 
  (lambda (x out) 
    (let ((type (loki-message->type x)) (source (loki-message->source x))
          (message (loki-message->message x)))
      (display type out)
      (display " at " out)
      (display (or source "<unknown>") out)
      (display ": " out)
      (display message out)
      (display "\n" out))))

(define make-loki-error
  (case-lambda
    ((source message)
      (make-loki-error source message #f))
    ((source message obj)
      (make-loki-message 'error source message obj))))

(define make-loki-warning
  (case-lambda
    ((source message)
      (make-loki-warning source message #f))
    ((source message obj)
      (make-loki-message 'warning source message obj))))

))
