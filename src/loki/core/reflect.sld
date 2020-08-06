(define-library (loki core reflect)
(cond-expand
  (chibi
    (import (chibi ast)))
  (loki
    (import (rename (core intrinsics) (%procedure-name-set! procedure-name-set!)))))
(export procedure-name-set!))
