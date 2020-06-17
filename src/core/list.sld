(define-library (core list)
(import (core primitives))
(import (core let))
(import (core control))
(import (core number))
(import (core intrinsics))
(export make-list list-copy
        member memv memq
        assoc assv assq)
(begin

(define (make-list n . o)
  (let ((default (if (pair? o) (car o))))
    (let lp ((n n) (res '()))
      (if (<= n 0) res (lp (- n 1) (cons default res))))))

(define (list-copy ls)
  (let lp ((ls ls) (res '()))
    (if (pair? ls)
        (lp (cdr ls) (cons (car ls) res))
        (append (reverse res) ls))))

(define (member obj ls . o)
  (let ((eq (if (pair? o) (car o) equal?)))
    (let lp ((ls ls))
      (and (pair? ls) (if (eq obj (car ls)) ls (lp (cdr ls)))))))
(define (memv obj ls) (member obj ls eqv?))
(define (memq obj ls) (member obj ls eq?))

(define (assoc obj ls . o)
  (let ((eq (if (pair? o) (car o) equal?)))
    (let assoc ((ls ls))
      (if (null? ls)
          #f
          (if (eq obj (caar ls))
              (car ls)
              (assoc (cdr ls)))))))
(define (assv obj ls) (assoc obj ls eqv?))
(define (assq obj ls) (assoc obj ls eq?))

))
