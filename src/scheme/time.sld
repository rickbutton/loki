(define-library (scheme time)
    (import (loki core intrinsics))
    (export (rename %current-jiffy current-jiffy)
            (rename %current-second current-second)
            (rename %jiffies-per-second jiffies-per-second)))

