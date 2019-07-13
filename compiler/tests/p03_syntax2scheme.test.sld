(define-library 
    (p03_syntax2scheme.test)
    (import (scheme base))
    (import (scheme write))
    (import (unit))
    (import (shared))
    (import (util))
    (import (srfi 159))
    (import (p02_attrs))
    (import (p03_syntax2scheme))
    (export test_p03_syntax2scheme)
(begin

(define (compile scheme) (p03_syntax2scheme (p02_attrs (scheme->mock-syntax scheme))))
(define (test-compile actual expected)
    (let ((out (compile actual)))
        (test-equal (show #f expected) expected out)))

(define (test_p03_syntax2scheme) 
    (test-group "p03_syntax2scheme"
        (test-compile 1 1)
        (test-compile #t #t)

        (test-compile 
            '(begin
                (define test (lambda () 123))
                (test))
            '(begin
                (define $v1_test (lambda () 123))
                ($v1_test)))
    ))
        
))