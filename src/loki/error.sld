(define-library (loki error)
(import (for (core exception) run expand))
(export exception? exception-type exception-message exception-irritants))
