;(define make (lambda (x) (lambda (y) (add x y))))

;((make 10) 1)

(define test (lambda (x y) (add x y)))
(define + (lambda (x y) (test x y)))

(+ 1 5)