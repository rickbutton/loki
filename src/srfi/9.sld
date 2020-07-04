(define-library (srfi 9)
(cond-expand
  (chibi (import (scheme base)))
  (loki (import (core records))))
(export define-record-type))
