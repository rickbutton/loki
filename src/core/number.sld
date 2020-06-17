(define-library (core number)
(import (core primitives))
(import (core control))
(import (core intrinsics))
(import (primitives %add %sub %mul %div
                    %lt %lte %number-eq %gt %gte))
(export + * - / < <= = > >=)
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
))
