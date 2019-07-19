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
            `(begin
                (define ,(make-variable '$v1_test 'bound) 
                    (lambda () 123))
                (,(make-variable '$v1_test 'bound))))

        (test-compile 
            '(begin
                (define define (lambda () 123))
                (define))
            `(begin
                (define ,(make-variable '$v1_define 'bound) 
                    (lambda () 123))
                (,(make-variable '$v1_define 'bound))))

        (test-compile 
            '(begin
                (define a (lambda () 123))
                (define b (lambda () 123))
                (set! a 456)
                (set! b 456)
                (a b))
            `(begin
                (define ,(make-variable '$v1_a 'bound) (lambda () 123))
                (define ,(make-variable '$v2_b 'bound) (lambda () 123))
                (set! ,(make-variable '$v1_a 'bound) 456)
                (set! ,(make-variable '$v2_b 'bound) 456)
                (,(make-variable '$v1_a 'bound) 
                 ,(make-variable '$v2_b 'bound))))

        (test-compile 
            '(begin
                (define test (lambda (xyz) 
                    (set! test 456)
                    (%%prim%add xyz test)))
                (test 123))
            `(begin
                (define ,(make-variable '$v1_test 'bound) 
                    (lambda (,(make-variable '$v2_xyz 'bound))
                        (set! ,(make-variable '$v1_test 'free) 456)
                        (,(make-intrinsic '%%prim%add)
                         ,(make-variable '$v2_xyz 'bound) 
                         ,(make-variable '$v1_test 'free))))
                (,(make-variable '$v1_test 'bound) 123)))
    ))
        
))
