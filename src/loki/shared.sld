(define-library 
    (loki shared)
    (import (scheme base))
    (import (scheme write))
    (import (srfi 69))
    (import (loki util))
    (import (loki compat))
    (export 
        make-source-location
        source-location->line
        source-location->col
        source-location->string

        make-token
        token?
        token->string
        token->type
        token->value
        token->location
        
        syntax
        syntax?
        syntax->location
        syntax->value
        syntax->attrs
        pair-syntax?

        syntax-get-attr
        syntax-set-attr

        make-variable
        variable?
        variable->value

        make-intrinsic
        intrinsic?
        intrinsic->name
        intrinsic-names
        intrinsic-name?

        make-comment
        comment?
        comment->text

        make-compile-error
        compile-error?
        compile-error->location
        compile-error->message
        raise-location-error
        raise-token-error
        raise-syntax-error
        
        safe-car-syntax
        safe-cdr-syntax
        safe-cadr-syntax
        safe-cddr-syntax
        safe-caddr-syntax
        safe-cdddr-syntax
        safe-cadddr-syntax
        safe-cddddr-syntax
        
        scheme->mock-syntax)
(begin

(define-record-type <source-location>
    (make-source-location path line col offset)
    source-location?
    (path source-location->path)
    (line source-location->line)
    (col  source-location->col)
    (offset source-location->offset))
(define (source-location->string l)
    (string-append 
        (source-location->path l)
        " ["
        (number->string (source-location->line l))
        ":"
        (number->string (source-location->col l))
        "]"))

(define-record-type <token>
    (make-token string type value location)
    token?
    (string token->string)
    (type token->type)
    (value token->value)
    (location token->location))

(define (make-attrs) (make-hash-table))

(define-record-type <syntax>
    (make-syntax-record location value attrs)
    syntax?
    (location syntax->location)
    (value syntax->value)
    (attrs syntax->attrs))
(define (syntax location value)
    (make-syntax-record location value (make-attrs)))
(define (pair-syntax? syntax)
    (and (syntax? syntax) (pair? (syntax->value syntax))))
(type-printer-set! <syntax> 
    (lambda (x out) 
        (display "#'" out)
        (display (syntax->value x) out)))

(define-record-type <variable>
    (make-variable value)
    variable?
    (value variable->value))
(type-printer-set! <variable> 
    (lambda (x out) 
        (display (symbol->string (variable->value x)) out)))

(define-record-type <intrinsic>
    (make-intrinsic name)
    intrinsic?
    (name intrinsic->name))
(define intrinsic-names '($$prim$add
                          $$prim$sub
                          $$prim$car
                          $$prim$cdr
                          $$prim$cons
                          $$prim$concat-string

                          $$prim$make-vector

                          $$prim$le_s))
(define (intrinsic-name? name)
    (contains? intrinsic-names name))
(type-printer-set! <intrinsic> 
    (lambda (x out) 
        (display (string-append 
            "i:" (symbol->string (intrinsic->name x))) out)))


(define-record-type <comment>
    (make-comment text)
    comment?
    (text comment->text))
(type-printer-set! <comment> 
    (lambda (x out) 
        (display (string-append "(;" (comment->text x) ";)") out)))

(define-record-type <compile-error>
    (make-compile-error location message)
    compile-error?
    (location compile-error->location)
    (message compile-error->message))

(define (raise-location-error location message)
        (raise (make-compile-error location message)))
(define (raise-token-error token message)
    (raise-location-error (token->location token) 
        (string-append message " [" (symbol->string (token->type token)) "]")))
(define (raise-syntax-error syntax message)
    (let ((location (syntax->location syntax)))
        (raise-location-error location message)))

(define (syntax-set-attr syntax attr value)
    (let ((attrs (syntax->attrs syntax)))
        (hash-table-set! attrs attr value)))
(define (syntax-get-attr syntax attr)
    (let ((attrs (syntax->attrs syntax)))
        (hash-table-ref/default attrs attr #f)))

(define (safe-car-syntax syntax) 
    (if (syntax? syntax)
        (if (pair? (syntax->value syntax))
            (car (syntax->value syntax)) 
            #f)
        #f))
(define (safe-cdr-syntax syntax) 
    (if (syntax? syntax)
        (if (pair? (syntax->value syntax)) 
            (cdr (syntax->value syntax))
            #f)
        #f))
(define (safe-cadr-syntax syntax) (safe-car-syntax (safe-cdr-syntax syntax)))
(define (safe-cddr-syntax syntax) (safe-cdr-syntax (safe-cdr-syntax syntax)))
(define (safe-caddr-syntax syntax) (safe-car-syntax (safe-cdr-syntax (safe-cdr-syntax syntax))))
(define (safe-cdddr-syntax syntax) (safe-cdr-syntax (safe-cdr-syntax (safe-cdr-syntax syntax))))
(define (safe-cadddr-syntax syntax) (safe-car-syntax (safe-cdr-syntax (safe-cdr-syntax (safe-cdr-syntax syntax)))))
(define (safe-cddddr-syntax syntax) (safe-cdr-syntax (safe-cdr-syntax (safe-cdr-syntax (safe-cdr-syntax syntax)))))

(define (scheme->mock-syntax scheme)
    (cond
        ((equal? scheme (quote '()))
            (syntax #f (list 
                (syntax #f 'quote)
                (syntax #f '()))))
        ((pair? scheme)
            (syntax #f (cons 
                (scheme->mock-syntax (car scheme)) 
                (scheme->mock-syntax (cdr scheme)))))
        ((string? scheme) (syntax #f scheme))
        ((boolean? scheme) (syntax #f scheme))
        ((char? scheme) (syntax #f scheme))
        ((number? scheme) (syntax #f scheme))
        ((symbol? scheme) (syntax #f scheme))
        ((null? scheme) '())
        (else (raise (string-append
            "unknown scheme, can't convert value to syntax")))))
))
