(define-library 
    (unit)
    (import (scheme base))
    (import (scheme write))
    (import (srfi 159))
    (import (srfi 159 color))
    (export test-assert test-equal test-eqv make-test-with-predicate test-error test-group)
(begin

(define (display-test-group name)
    (display "\n")
    (display (show #f (as-underline name)))
    (display "\n"))

(define (fail-error! e)
    (display (show #f (as-red "fail! ")))
    (display "exception caught: ")
    (display e)
    (display "\n"))

(define (fail-assert! name)
    (display (show #f (as-red "fail! ")))
    (display " assertion failed: ")
    (display name)
    (display "\n"))

(define (fail-equal! name expected actual)
    (display (show #f (as-red "fail! ")))
    (display (show #f (as-bold name)))
    (display " expected \n")
    (display " ")
    (display (show #f (as-cyan (written expected))))
    (display "\n")
    (display " received \n")
    (display " ")
    (display (show #f (as-cyan (written actual))))
    (display "\n"))

(define (fail-no-error! name)
    (display (show #f (as-red "fail! ")))
    (display "expected exception but did not catch one: ")
    (display name)
    (display "\n"))

(define (pass! name)
    (display (show #f (as-green "pass! ")))
    (display name)
    (display "\n"))

(define-syntax fail-if-error
    (syntax-rules () ((fail-if-error expr)
        (call/cc (lambda (k)
            (with-exception-handler
                (lambda (e) 
                    (fail-error! e)
                    (k))
                (lambda () expr)))))))

(define-syntax test-assert
    (syntax-rules () ((test-assert name val)
        (fail-if-error (if val (pass! name) (fail-assert! name))))))

(define-syntax test-equal
    (syntax-rules () ((test-equal name expected actual)
        (fail-if-error
            (if (equal? expected actual) (pass! name) (fail-equal! name expected actual))))))

(define-syntax test-eqv
    (syntax-rules () ((test-eqv name expected actual)
        (fail-if-error
            (if (eqv? expected actual) (pass! name) (fail-equal! name expected actual))))))

(define-syntax test-with-predicate
    (syntax-rules () ((test-with-predicate name expected actual predicate)
        (fail-if-error
            (if (predicate expected actual) (pass! name) (fail-equal! name expected actual))))))
(define-syntax make-test-with-predicate
    (syntax-rules () ((make-test-with-predicate predicate)
        (lambda (name expected actual) (test-with-predicate name expected actual predicate)))))

(define-syntax test-error
    (syntax-rules () ((test-error name expr)
        (call/cc (lambda (k)
            (with-exception-handler
                (lambda (e) (pass! name) (k))
                (lambda () 
                    (begin
                        expr
                        (fail-no-error! name)))))))))

(define-syntax test-group
    (syntax-rules ()
        ((test-group name body ...)
            (begin
                (display-test-group name)
                (call/cc (lambda (k)
                    (with-exception-handler
                        (lambda (e) 
                            (fail-error! e)
                            (k))
                        (lambda () body ...))))))))
))
