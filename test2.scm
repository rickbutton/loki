(define (+ a b c) (add a b c))

(define (test x)
    (let ((y (add x 2)) (z (add x 3)))
        (+ x y z)))

(test 5)