(define-library (loki core fs)
(cond-expand
  (loki
    (import (rename (core intrinsics) (%current-directory current-directory))))
  (gauche (import (file util))))
(export current-directory))
