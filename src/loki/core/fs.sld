(define-library (loki core fs)
(cond-expand
  (chibi
    (import (scheme base))
    (import (chibi filesystem)))
  (loki
    (import (rename (core intrinsics) (%current-directory current-directory)))))
(export current-directory))
