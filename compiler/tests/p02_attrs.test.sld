(define-library 
    (p02_attrs.test)
    (import (scheme base))
    (import (scheme write))
    (import (unit))
    (import (shared))
    (import (util))
    (import (srfi 159))
    (import (p02_attrs))
    (export test_p02_attrs)
(begin

(define (test-pass scheme)
    (test-assert (show #f scheme) (begin (p02_attrs (scheme->mock-syntax scheme)) #t)))

(define (test-fail scheme)
    (test-error (show #f scheme) (p02_attrs (scheme->mock-syntax scheme))))

(define (get-second-expr-in-begin syntax)
    (cons-syntax->car (cons-syntax->car 
    (cons-syntax->cdr (cons-syntax->cdr syntax)))))
(define (get-third-expr-in-begin syntax)
    (cons-syntax->car (cons-syntax->car 
    (cons-syntax->cdr (cons-syntax->cdr (cons-syntax->cdr syntax))))))
(define (test-was-type getter scheme type)
    (test-equal (show #f scheme) type (syntax-get-attr (getter (p02_attrs (scheme->mock-syntax scheme))) 'type)))
(define (test-was-ref getter scheme) (test-was-type getter scheme 'reference))
(define (test-was-prim getter scheme) (test-was-type getter scheme 'primitive))
(define (test-was-int getter scheme) (test-was-type getter scheme 'intrinsic))

(define primitives-to-test-as-vars 
    '((quote (quote 123))
        (quasiquote (quasiquote 123))
        (lambda (lambda () 123))
        (if (if 123 456 789))
        (if (if 123 456))
        (begin (begin 123))))

(define (test_p02_attrs) 
    (test-group "p02_attrs"
        ; valid quote/quasiquote syntax
        (test-pass '(quote 123))
        (test-pass '(quasiquote 123))
        (test-pass '(quote (123)))
        (test-pass '(quasiquote (123)))
        (test-pass '(quote (123 456)))
        (test-pass '(quasiquote (123 456)))

        ; expect quoted datum to not be validated 
        ; (quote) is not valid, but is a quoted datum
        ; not an expression
        (test-pass '(quote (quote)))
        (test-pass '(quasiquote (quote)))
        (test-pass '(quote (quote 123 456)))
        (test-pass '(quasiquote (quote 123 456)))

        ; expect unquote inside quote to pass
        (test-pass '(quote (unquote)))
        (test-pass '(quote (unquote 123)))
        (test-pass '(quote (unquote 123 456)))
        (test-pass '(quote (unquote-splicing)))
        (test-pass '(quote (unquote-splicing 123)))
        (test-pass '(quote (unquote-splicing 123 456)))

        ; expect unquote inside quasiquote to pass
        (test-pass '(quasiquote (unquote 123)))
        (test-pass '(quasiquote (unquote-splicing 123)))

        ; invalid quote/quasiquote syntax
        (test-fail '(quote))
        (test-fail '(quote 123 456))
        (test-fail '(quasiquote))
        (test-fail '(quasiquote 123 456))

        ; invalid unquote syntax inside quasiquote
        (test-fail '(quasiquote (unquote)))
        (test-fail '(quasiquote (unquote 123 456)))
        (test-fail '(quasiquote (unquote-splicing)))
        (test-fail '(quasiquote (unquote-splicing 123 456)))

        ; invalid unquote syntax when not in quote context should fail
        (test-fail '(unquote))
        (test-fail '(unquote 123 456))
        (test-fail '(unquote-splicing))
        (test-fail '(unquote-splicing 123 456))

        ; valid unquote syntax when not in quote context should fail
        (test-fail '(unquote 123))
        (test-fail '(unquote-splicing 123))

        ; invalid quote syntax inside unquoted quote context should fail
        (test-fail '(quasiquote (123 (unquote (quote 123 456)))))
        (test-fail '(quasiquote (123 (unquote (quasiquote 123 456)))))
        (test-fail '(quasiquote (123 (unquote-splicing (quote 123 456)))))
        (test-fail '(quasiquote (123 (unquote-splicing (quasiquote 123 456)))))

        ; valid set! syntax
        (test-pass '(lambda (x) (set! x 1)))

        ; invalid set! syntax
        (test-fail '(set!))
        (test-fail '(set! x 1))
        (test-fail '(set! 123 1))
        (test-fail '(lambda () (set!)))
        (test-fail '(lambda (x) (set! x)))
        (test-fail '(lambda (x) (set! x 1 2)))

        ; valid if syntax
        (test-pass '(if 1 2 3))
        (test-pass '(if 'anything #t))

        ; invalid if syntax
        (test-fail '(if))
        (test-fail '(if 1))
        (test-fail '(if 1 2 3 4))

        ; valid lambda syntax
        (test-pass '(lambda () 123 456))
        (test-pass '(lambda (x) x (x x)))

        ; lambda frees
        (test-pass '(lambda (x) (lambda (y) x)))

        ; invalid lambda syntax
        (test-fail '(lambda ()))
        (test-fail '(lambda (123)))
        (test-fail '(lambda (#t)))
        (test-fail '(lambda #f))
        (test-fail '(lambda (x x) 123))

        ; attempt to reference variable that is not declared
        (test-fail 'x)
        (test-fail '(x 123))

        ; valid define syntax
        (test-pass '(define name 123))
        (test-pass '(define name name))
        (test-pass '(define name (lambda (x) (x 123))))
        (test-pass '(define (name x y z) (x y z)))
        (test-pass '(define (name x ) name))

        (test-pass '(begin (define name 123) name))
        (test-pass '(begin
            (define name 123)
            (define name2 456)
            (name name2)))
        (test-pass '(begin
            (define name 123)
            (define name 456)))

        ; invalid define syntax
        (test-fail '(define 123 456))
        (test-fail '(define 123))
        (test-fail '(define name))
        (test-fail '(define name 123 456))
        (test-fail '(define (name x)))
        (test-fail '(define (name x x) x))

        ;intrinsics
        (test-pass '%%prim%add)

        (map (lambda (p)
            (test-was-ref get-second-expr-in-begin `(begin
                (define ,(car p) 123)
                ,@(cdr p)))
            (test-was-prim get-second-expr-in-begin `(begin
                (define nope 123)
                ,@(cdr p))))
            primitives-to-test-as-vars)

        (test-was-ref get-second-expr-in-begin '(begin
            (define define 123)
            (define 123)))
        (test-was-prim get-second-expr-in-begin '(begin
            (define nope 123)
            (define name 123)))

        (test-was-ref get-third-expr-in-begin '(begin
            (define set! 123)
            (define test 0)
            (set! test 1)))
        (test-was-prim get-second-expr-in-begin '(begin
            (define test 0)
            (set! test 1)))
    ))

))
