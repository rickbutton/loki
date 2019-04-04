(define make (lambda (x) (lambda (y) (add x y))))

(define test (make 10))
(define test2 (make 20))

(cons 1 (test2 2))