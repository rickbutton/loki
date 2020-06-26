(define-library (core bool)
(import (core primitives))
(import (core intrinsics))
(export not boolean?)
(begin
(define (not x) (if x #f #t))
(define (boolean? x) (if (eq? x #t) #t (eq? x #f)))
))
