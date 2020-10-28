(define-library (scheme read)
  (import (loki core primitives))
  (import (only (loki core reader) make-reader read-datum))
  (import (loki core io))
  (import (loki core case-lambda))
  (export read)
  (begin
    (define read
      (case-lambda
        (() (read-datum (make-reader (current-input-port) "<read>")))
        ((port) (read-datum (make-reader port "<read>")))))))


