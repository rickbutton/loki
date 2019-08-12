(define-library 
    (p01_tokens2syntax.test)
    (import (scheme base))
    (import (scheme write))
    (import (unit))
    (import (shared))
    (import (util))
    (import (srfi 159))
    (import (p00_string2tokens))
    (import (p01_tokens2syntax))
    (export test_p01_tokens2syntax)
(begin

(define (lex-and-parse str)
    (let* ((tokens (p00_string2tokens (open-input-string str)))
           (syntax (p01_tokens2syntax tokens)))
        syntax))

(define (syntax->scheme syntax)
    (if (pair-syntax? syntax)
        (cons 
            (syntax->scheme (safe-car-syntax syntax))
            (syntax->scheme (safe-cdr-syntax syntax)))
        (syntax->value syntax)))

(define (syntax-equal? a b) (equal? (syntax->scheme a) (syntax->scheme b)))

(define test-syntax-equal (make-test-with-predicate syntax-equal?))
(define (test-parse-equal str expected)
    (test-equal str expected (syntax->scheme (lex-and-parse str))))

(define (test-parse-fail str)
    (test-error str (lex-and-parse str)))

(define (test_p01_tokens2syntax) 
    (test-group "p01_tokens2syntax"
        (test-parse-equal "\"this is a test\"" "this is a test")
        (test-parse-equal "#t" #t)
        (test-parse-equal "#f" #f)
        (test-parse-equal "#\\a" #\a)
        (test-parse-equal "1" 1)
        (test-parse-equal "test" 'test)
        (test-parse-equal "()" '())
        (test-parse-equal "(1)" '(1))
        (test-parse-equal "(1 2 3)" '(1 2 3))
        (test-parse-equal "(test 1 #f \"TEST\" #\\a)" '(test 1 #f "TEST" #\a))
        (test-parse-equal "(1 . 3)" '(1 . 3))
        (test-parse-equal "(1 2 3 . 4)" '(1 2 3 . 4))
        (test-parse-equal "(1 2 3 . 4)" '(1 2 3 . 4))

        (test-parse-fail "(")
        (test-parse-fail ")")
        (test-parse-fail "(.)")
        (test-parse-fail "(1 . )")

        (test-parse-equal "'123" '(quote 123))
        (test-parse-equal "'(123 456)" '(quote (123 456)))
        (test-parse-equal "'(123 '456)" '(quote (123 (quote 456))))
        (test-parse-equal "`(123 456 ,x)" '(quasiquote (123 456 (unquote x))))
        (test-parse-equal "`(123 456 ,@x)" '(quasiquote (123 456 (unquote-splicing x))))

        (test-parse-equal "(x 1 2 3) (y 4 5 6) 7" '(begin (x 1 2 3) (y 4 5 6) 7))
    ))

))
