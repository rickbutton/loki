(define make-add (lambda (x) (lambda (y) (add x y))))
(define add-to-12 (make-add 12))

(add-to-12 45)