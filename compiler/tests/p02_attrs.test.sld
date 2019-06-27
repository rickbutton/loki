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
    (test-assert (show #f scheme) (begin (p02_attrs (scheme->syntax scheme)) #t)))

(define (test-fail scheme)
    (test-error (show #f scheme) (p02_attrs (scheme->syntax scheme))))

(define (scheme->syntax scheme)
    (cond
        ((pair? scheme)
            (make-cons-syntax #f (scheme->syntax (car scheme)) 
                                    (scheme->syntax (cdr scheme))))
        ((string? scheme) (make-atom-syntax 'string #f scheme))
        ((boolean? scheme) (make-atom-syntax 'boolean #f scheme))
        ((char? scheme) (make-atom-syntax 'char #f scheme))
        ((number? scheme) (make-atom-syntax 'number #f scheme))
        ((symbol? scheme) (make-atom-syntax 'symbol #f scheme))
        ((null? scheme) (make-atom-syntax 'null #f scheme))
        (else (raise (string-append
            "unknown scheme, can't convert value "
            (show #f scheme)
            "to syntax")))))

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
    ))

))