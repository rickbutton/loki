(define-library 
    (p04_scheme2cps.test)
    (import (scheme base))
    (import (scheme write))
    (import (unit))
    (import (shared))
    (import (util))
    (import (srfi 159))
    (import (p04_scheme2cps))
    (export test_p04_scheme2cps)
(begin

(define (test-compile actual expected)
    (test-equal (show #f actual) expected (p04_scheme2cps actual)))

(define (test_p04_scheme2cps) 
    (test-group "p04_scheme2cps"
        (test-compile 1 '(constant 1 (return)))
        (test-compile #t '(constant #t (return)))
        (test-compile #\a '(constant #\a (return)))
        (test-compile '() '(constant () (return)))

        (let ((marked (make-variable 'test 'bound)))
            (test-compile marked `(refer ,marked (return))))
        (test-compile '(1 . 2) '(constant 1 (constant 2 (pair (return)))))

        
    
        (define (mb v) (make-variable v 'bound))
        (define (mi n) (make-intrinsic n))
        (let ((x (mb '$v1_x))
              (y (mb '$v2_y))
              (add (mi '%%prim%add))
              (mul (make-variable '$v4_mul 'free)))

            (test-compile `(,add 1 2)
            `(constant 1 (constant 2 (intrinsic %%prim%add (return)))))

            (test-compile `(let ((,x 1) (,y 2)) (,add ,x ,y))
                `(constant 1 (slot (store ,x 
                 (constant 2 (slot (store ,y
                 (refer ,x (refer ,y (intrinsic %%prim%add (return)))))))))))

            (test-compile `(begin (define ,x 1) (,mul ,x 2))
                `(constant 1 (slot (store ,x
                    (refer ,mul (refer ,x (constant 2 (apply 2 (return)))))))))

            (test-compile `(begin 1 2)
                          '(constant 1 (constant 2 (return))))

            (test-compile `(lambda (,x) (,add ,x 1))
                          `(close (,x) (refer ,x (constant 1 
                                (intrinsic %%prim%add (return)))) (return))))
    ))
        
))
