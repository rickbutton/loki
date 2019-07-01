(define-library 
    (shared)
    (import (scheme base))
    (import (scheme write))
    (import (srfi 69))
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
        atom-syntax->attrs)
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

))