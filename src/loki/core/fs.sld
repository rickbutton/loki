(define-library (loki core fs)
(cond-expand
  (loki
    (import (rename (loki core intrinsics) (%current-directory current-directory) (%file-mtime file-mtime)))
    (import (loki core cond-expand))
    (import (loki core primitives)))
  (gauche (import (gauche base))
          (import (file util))))
(export current-directory
        file-mtime)
(begin

(cond-expand
  (gauche 
    (define (file-mtime path)
      (sys-stat->mtime (sys-stat path)))))))
