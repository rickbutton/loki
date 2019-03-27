(define test (lambda (x) (lambda (y) (sub x y))))

((test 10) 2)