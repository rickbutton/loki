(define-library (core math)
(import (core primitives))
(import (core derived))
(import (core apply))
(import (core let))
(import (core list))
(import (core bool))
(import (core number))
(import (core case-lambda))
(import (core cond-expand))
(import (core define-missing))
(import (rename (core intrinsics) (%number?    number?)
                                  (%finite?    finite?)
                                  (%infinite?  infinite?)
                                  (%nan?       nan?)
                                  (%floor      floor)
                                  (%ceiling    ceiling)
                                  (%truncate   truncate)
                                  (%round      round)
                                  (%sqrt       sqrt)
                                  (%expt       expt)
                                  ))
(export number? complex? real? rational? integer?
        exact? inexact? exact inexact exact-integer?
        finite? infinite? nan? even? odd? max min
        floor/ floor-quotient floor-remainder modulo
        truncate/ truncate-quotient quotient truncate-remainder remainder 
        rationalize exp log sin cos tan asin acos atan 
        exact-integer-sqrt expt make-rectangular make-polar
        real-part imag-part magnitude angle gcd lcm
        numerator denominator floor ceiling truncate round sqrt square)
(begin

(define complex? number?)
(define real? number?)
(define (integer? x)
  (= x (truncate x)))

(define (rational? x)
  (and (real? x)
       (= x x)
       (if (or (> x 1) (< x -1))
           (not (= x (/ x 2)))
           (<= -1 x 1))))

; TODO - lol
(define (exact? x) #t)
(define (inexact? x) #f)
(define (exact x) x)
(define (inexact x) x)

(define (exact-integer? x) (and (integer? x) (exact? x)))

(define (even? n) (= (remainder n 2) 0))
(define (odd? n) (not (= (remainder n 2) 0)))

(define (max x . rest)
  (define (max2 hi ls)
    (if (null? ls)
        hi
        (max2 (if (> (car ls) hi)
                  (car ls)
                  hi)
              (cdr ls))))
  (max2 x rest))

(define (min x . rest)
  (define (min2 lo ls)
    (if (null? ls)
        lo
        (min2 (if (< (car ls) lo)
                  (car ls)
                  lo)
              (cdr ls))))
  (min2 x rest))

(define-missing floor/
                floor-quotient
                floor-remainder modulo
                truncate/
                truncate-quotient quotient
                truncate-remainder remainder
                rationalize
                exp log sin cos tan asin acos atan 
                exact-integer-sqrt
                make-rectangular make-polar)


(define (numerator x) x)
(define (denominator x) 1.0)

(define (square x) (%mul x x))

(define (real-part x) x)
(define (imag-part x) 0)

(define magnitude abs)
(define (angle z) (if (< z 0) 3.141592653589793 0))

(define (gcd2 a b)
  (if (= b 0)
      (abs a)
      (gcd b (remainder a b))))

(define (gcd . args)
  (if (null? args)
      0
      (let lp ((x (car args)) (ls (cdr args)))
        (if (null? ls) x (lp (gcd2 x (car ls)) (cdr ls))))))

(define (lcm2 a b)
  (abs (quotient (* a b) (gcd a b))))

(define (lcm . args)
  (if (null? args)
      1
      (let lp ((x (car args)) (ls (cdr args)))
        (if (null? ls) x (lp (lcm2 x (car ls)) (cdr ls))))))

))