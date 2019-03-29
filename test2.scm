;(define make (lambda (x) (lambda (y) (add x y))))

;((make 10) 1)

(let ((num 10) (make (lambda (x) (lambda (y) (add x y)))))
    ((make 10) 1))