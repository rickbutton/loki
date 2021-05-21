(define-library (loki error)
  (import (for (loki core exception) run expand))
  (export exception? exception-type exception-message exception-irritants))
