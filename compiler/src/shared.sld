(define-library 
    (shared)
    (import (scheme base))
    (import (scheme write))
    (import (srfi 69))
    (import (srfi 159))
    (import (util))
    (export 
        make-source-location
        source-location->line
        source-location->col

        make-token
        token?
        token->string
        token->type
        token->value
        token->location
        
        make-cons-syntax
        cons-syntax?
        cons-syntax->start
        cons-syntax->car
        cons-syntax->cdr
        cons-syntax->attrs

        make-atom-syntax
        atom-syntax?
        atom-syntax->type
        atom-syntax->token
        atom-syntax->value
        atom-syntax->attrs

        make-variable
        variable?
        variable->value
        variable->binding

        make-intrinsic
        intrinsic?
        intrinsic->name
        intrinsic-names
        intrinsic-name?
        
        syntax->attrs
        syntax-get-attr
        syntax-set-attr
        
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
    (make-source-location line col offset)
    source-location?
    (line source-location->line)
    (col  source-location->col)
    (offset source-location->offset))

(define-record-type <token>
    (make-token string type value location)
    token?
    (string token->string)
    (type token->type)
    (value token->value)
    (location token->location))

(define (make-attrs) (make-hash-table))

(define-record-type <cons-syntax>
    (make-cons-syntax-record start car cdr attrs)
    cons-syntax?
    (start cons-syntax->start)
    (car cons-syntax->car)
    (cdr cons-syntax->cdr)
    (attrs cons-syntax->attrs))
(define (make-cons-syntax start car cdr)
    (make-cons-syntax-record start car cdr (make-attrs)))

(define-record-type <atom-syntax>
    (make-atom-syntax-record type token value attrs)
    atom-syntax?
    (type atom-syntax->type)
    (token atom-syntax->token)
    (value atom-syntax->value)
    (attrs atom-syntax->attrs))
(define (make-atom-syntax type token value)
    (make-atom-syntax-record type token value (make-attrs)))

(define-record-type <variable>
    (make-variable value binding)
    variable?
    (value variable->value)
    (binding variable->binding))

(define-record-type <intrinsic>
    (make-intrinsic name)
    intrinsic?
    (name intrinsic->name))
(define intrinsic-names '(%%prim%add
                          %%prim%sub
                          %%prim%car
                          %%prim%cdr
                          %%prim%cons))
(define (intrinsic-name? name)
    (contains? intrinsic-names name))

(define (syntax->attrs syntax)
    (cond
        ((cons-syntax? syntax) (cons-syntax->attrs syntax))
        ((atom-syntax? syntax) (atom-syntax->attrs syntax))
        (else (raise "unknown syntax type when getting attrs"))))
(define (syntax-set-attr syntax attr value)
    (let ((attrs (syntax->attrs syntax)))
        (hash-table-set! attrs attr value)))
(define (syntax-get-attr syntax attr)
    (let ((attrs (syntax->attrs syntax)))
        (hash-table-ref/default attrs attr #f)))

(define (safe-car-syntax syntax) (if (cons-syntax? syntax) (cons-syntax->car syntax) #f))
(define (safe-cdr-syntax syntax) (if (cons-syntax? syntax) (cons-syntax->cdr syntax) #f))
(define (safe-cadr-syntax syntax) (safe-car-syntax (safe-cdr-syntax syntax)))
(define (safe-cddr-syntax syntax) (safe-cdr-syntax (safe-cdr-syntax syntax)))
(define (safe-caddr-syntax syntax) (safe-car-syntax (safe-cdr-syntax (safe-cdr-syntax syntax))))
(define (safe-cdddr-syntax syntax) (safe-cdr-syntax (safe-cdr-syntax (safe-cdr-syntax syntax))))
(define (safe-cadddr-syntax syntax) (safe-car-syntax (safe-cdr-syntax (safe-cdr-syntax (safe-cdr-syntax syntax)))))
(define (safe-cddddr-syntax syntax) (safe-cdr-syntax (safe-cdr-syntax (safe-cdr-syntax (safe-cdr-syntax syntax)))))

(define (scheme->mock-syntax scheme)
    (cond
        ((pair? scheme)
            (make-cons-syntax #f (scheme->mock-syntax (car scheme)) 
                                    (scheme->mock-syntax (cdr scheme))))
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
))
