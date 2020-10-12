(define-library (loki core values)
  (export (rename %values values)
          (rename %call-with-values call-with-values))
  (import (loki core intrinsics)))
