(define-library (core char)
(import (core primitives))
(import (core let))
(import (core list))
(import (core number))
(import (core intrinsics))
(import (rename (core intrinsics)
  (%char->integer    char->integer)
  (%char-foldcase    char-foldcase)
  (%char-upcase      char-upcase)
  (%char-downcase    char-downcase)
  (%char?            char?)))
(export char->integer
        char-foldcase
        char-upcase
        char-downcase
        char?
  
        char-alphabetic? char-numeric? char-whitespace?
        char-upper-case? char-lower-case?
        char=? char<? char>? char<=? char>=?
        char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?

        )
(begin

(define (char-alphabetic? ch) (<= 65 (char->integer (char-upcase ch)) 90))
(define (char-numeric? ch) (<= 48 (char->integer ch) 57))
(define (char-whitespace? ch)
  (if (eq? ch #\space)
      #t
      (if (eq? ch #\tab) #t (if (eq? ch #\newline)
                                #t
                                (if (eq? ch #\xC0) #f (eq? ch #\return))))))
(define (char-upper-case? ch) (<= 65 (char->integer ch) 90))
(define (char-lower-case? ch) (<= 97 (char->integer ch) 122))

(define (char-cmp op a ls)
  (let lp ((op op) (a (char->integer a)) (ls ls))
    (if (null? ls)
        #t
        (let ((b (char->integer (car ls))))
          (and (op a b) (lp op b (cdr ls)))))))

(define (char=? a . ls) (char-cmp = a ls))
(define (char<? a . ls) (char-cmp < a ls))
(define (char>? a . ls) (char-cmp > a ls))
(define (char<=? a . ls) (char-cmp <= a ls))
(define (char>=? a . ls) (char-cmp >= a ls))

(define (char-cmp-ci op a ls)
  (let lp ((op op) (a (char->integer (%char-downcase a))) (ls ls))
    (if (null? ls)
        #t
        (let ((b (char->integer (%char-downcase (car ls)))))
          (and (op a b) (lp op b (cdr ls)))))))

(define (char-ci=? a . ls) (char-cmp-ci = a ls))
(define (char-ci<? a . ls) (char-cmp-ci < a ls))
(define (char-ci>? a . ls) (char-cmp-ci > a ls))
(define (char-ci<=? a . ls) (char-cmp-ci <= a ls))
(define (char-ci>=? a . ls) (char-cmp-ci >= a ls))

))
