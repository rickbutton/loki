(define-library (core loki-message)
(cond-expand
  (loki
    (import (core primitives))
    (import (core let))
    (import (core bool))
    (import (core io))
    (import (core records))
    (import (core exception))
    (import (core process))
    (import (core dynamic)))
  (chibi
    (import (scheme base))
    (import (scheme process-context))
    (import (scheme write))))
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
  (make-loki-message type source message)
  loki-message?
  (type loki-message->type)
  (source loki-message->source)
  (message loki-message->message))

(define (raise-loki-error source message)
  (raise (make-loki-message 'error source message)))

(define (raise-loki-warning source message)
  (raise (make-loki-message 'warning source message)))

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
    (raise e)
    (if (eq? type 'error) (raise e))))

(define (handle-unexpected-error e)
  (display "unexpected error: ")
  (display e) (raise e))

(define (handle-error e)
        (if (loki-message? e)
            (handle-loki-message e)
            (handle-unexpected-error e)))


(define (with-loki-error-handler proc)
  (with-exception-handler handle-error proc))

))
