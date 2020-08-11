(define-library (loki message)
(import (scheme base))
(import (scheme process-context))
(import (scheme case-lambda))
(import (loki printer))
(cond-expand
  (gauche (import (gauche base))
          (import (scheme write))))
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

(define (write-loki-message x writer out)
  (let ((type (loki-message->type x)) (source (loki-message->source x))
        (message (loki-message->message x)))
    (writer type)
    (write-string " at " out)
    (if source
      (writer source)
      (write-string "<unknown>" out))
    (write-string ": " out)
    (write-string message out)
    (write-string "\n" out)))

(type-printer-set! <loki-message> write-loki-message)

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
