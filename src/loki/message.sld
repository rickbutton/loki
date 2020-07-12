(define-library (loki message)
(import (scheme base))
(import (scheme process-context))
(import (scheme case-lambda))
(import (loki writer))
(export raise-loki-error
        raise-loki-warning
        make-loki-message
        loki-message?
        loki-message->type
        loki-message->source
        loki-message->message
        with-loki-error-handler)
(begin

(define-record-type <loki-message>
  (make-loki-message type source message obj)
  loki-message?
  (type loki-message->type)
  (source loki-message->source)
  (message loki-message->message)
  (obj loki-message->obj))

(define raise-loki-error
  (case-lambda
    ((source message)
      (raise-loki-error source message #f))
    ((source message obj)
      (raise (make-loki-message 'error source message obj)))))

(define raise-loki-warning
  (case-lambda
    ((source message)
      (raise-loki-warning source message #f))
    ((source message obj)
      (raise (make-loki-message 'warning source message obj)))))

(define (handle-loki-message e)
  (let ((type (loki-message->type e)) (source (loki-message->source e))
        (message (loki-message->message e)))
    (display type)
    (display " at ")
    (if source
        (display source)
        (display "<unknown>"))
    (display ": ")
    (display message)
    (display "\n")
    (unless (eq? type 'continuable) (raise e))))

(define (handle-unexpected-error e)
  (display "unexpected error: ")
  (raise e))

(define (handle-error e)
        (if (loki-message? e)
            (handle-loki-message e)
            (handle-unexpected-error e)))


(define (with-loki-error-handler proc)
  (with-exception-handler handle-error proc))

))
