(define-library (core number)
(import (core primitives))
(import (core apply))
(import (core intrinsics))
(import (core case-lambda))
(export + * - / < <= = > >= zero? positive? negative? abs
        bitwise-not bitwise-and bitwise-ior bitwise-xor
        arithmetic-shift bit-count integer-length)
(begin

(define +
  (case-lambda
    (() 0)
    ((a) a)
    ((a b) (%add a b))
    ((a b . rest) (apply + (%add a b) rest))))

(define *
  (case-lambda
    (() 1)
    ((a) a)
    ((a b) (%mul a b))
    ((a b . rest) (apply * (%mul a b) rest))))

(define -
  (case-lambda
    ((a) (%sub 0 a))
    ((a b) (%sub a b))
    ((a b . rest) (apply - (%sub a b) rest))))

(define /
  (case-lambda
    ((a) (%div 1 a))
    ((a b) (%div a b))
    ((a b . rest) (apply / (%div a b) rest))))

(define <
  (case-lambda
    ((a b) (%lt a b))
    ((a b . rest) (and (%lt a b) (apply < b rest)))))

(define <=
  (case-lambda
    ((a b) (%lte a b))
    ((a b . rest) (and (%lte a b) (apply <= b rest)))))

(define =
  (case-lambda
    ((a b) (%number-eq a b))
    ((a b . rest) (and (%number-eq a b) (apply = b rest)))))

(define >
  (case-lambda
    ((a b) (%gt a b))
    ((a b . rest) (and (%gt a b) (apply > b rest)))))

(define >=
  (case-lambda
    ((a b) (%gte a b))
    ((a b . rest) (and (%gte a b) (apply >= b rest)))))


(define (zero? x) (= x 0))
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))

(define (abs x) (if (< x 0) (- x) x))

(define (bitwise-not i)
  (%bit-not i))

(define bitwise-and
  (case-lambda
    (() -1)
    ((a b) (%bit-and a b))
    ((a b . rest) (%bit-and a (apply bitwise-and b rest)))))

(define bitwise-ior
  (case-lambda
    (() 0)
    ((a b) (%bit-ior a b))
    ((a b . rest) (%bit-ior a (apply bitwise-ior b rest)))))

(define bitwise-xor
  (case-lambda
    (() 0)
    ((a b) (%bit-xor a b))
    ((a b . rest) (%bit-xor a (apply bitwise-xor b rest)))))

(define (arithmetic-shift i count) (%bit-shift i count))
(define (bit-count i) (%bit-count i))
(define (integer-length i) (%bit-length i))

))
