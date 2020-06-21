(define-library (core bool)
(import (core primitives))
(import (core intrinsics))
(export not boolean? boolean=?)
(begin

(define (not x) (if x #f #t))
(define (boolean? x) (if (eq? x #t) #t (eq? x #f)))

(define (boolean=? x y . o)
  (if (not (boolean? x))
    (error "not a boolean" x)
    (and (eq? x y) (if (pair? o) (apply boolean=? y o) #t))))
))
