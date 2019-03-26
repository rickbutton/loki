(define make (lambda () (lambda (x y) (sub x y))))
(define - (make))

(- 2 10)