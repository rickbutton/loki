(define-library (loki core define-missing)
  (import (loki core primitives))
  (import (loki core intrinsics))
  (import (loki core exception))
  (import (for (loki core syntax-rules) expand))
  (export define-missing)
  (begin
   (define-syntax define-missing
     (syntax-rules ()
                   ((_ name)
                    (define (name . args)
                      (raise "not implemented")))
                   ((_ name names ...)
                    (begin
                     (define-missing name)
                     (define-missing names ...)))))))

