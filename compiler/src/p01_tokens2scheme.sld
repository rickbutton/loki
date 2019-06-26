(define-library 
    (p01_tokens2scheme)
    (import (scheme base))
    (import (util))
    (import (shared))
    (export 
        p01_tokens2scheme
        make-cons-syntax
        cons-syntax?
        cons-syntax->start
        cons-syntax->end
        cons-syntax->car
        cons-syntax->cdr
        make-atom-syntax
        atom-syntax?
        atom-syntax->type
        atom-syntax->token
        atom-syntax->value)
(begin

; lvector
; lbytevector

; lparen
; rparen
; dot
; string
; boolean
; char
; number
; id
; unquote-splicing
; unquote
; quote
; quasiquote

(define-record-type <cons-syntax>
    (make-cons-syntax start car cdr)
    cons-syntax?
    (start cons-syntax->start)
    (car cons-syntax->car)
    (cdr cons-syntax->cdr))

(define-record-type <atom-syntax>
    (make-atom-syntax type token value)
    atom-syntax?
    (type atom-syntax->type)
    (token atom-syntax->token)
    (value atom-syntax->value))

(define (p01_tokens2scheme tokens)
    (let ((token-list tokens))
        (define (peek-token)
            (if (null? token-list) #f (car token-list)))

        (define (pop-token)
            (if (null? token-list)
                #f
                (begin
                    (let ((token (car token-list)))
                        (set! token-list (cdr token-list))
                        token))))

        (define (parse-next)
            (parse-next-with-token (pop-token)))

        (define (parse-next-with-token token)
            (let* ((type (token->type token)) (value (token->value token)))
                (if token
                    (cond
                        ((equal? type 'lparen) (parse-cons token))
                        ;((equal? type 'lvector) (parse-vector))
                        ;((equal? type 'lbytevector) (parse-bytevector))
                        ((equal? type 'quote) (parse-quote token))
                        ((equal? type 'quasiquote) (parse-quasiquote token))
                        ((equal? type 'unquote) (parse-unquote token))
                        ((equal? type 'unquote-splicing) (parse-unquote-splicing token))
                        ((equal? type 'string) (make-atom-syntax 'string token value))
                        ((equal? type 'boolean) (make-atom-syntax 'boolean token value))
                        ((equal? type 'char) (make-atom-syntax 'char token value))
                        ((equal? type 'number) (make-atom-syntax 'number token value))
                        ((equal? type 'id) (make-atom-syntax 'symbol token value))
                        (else (raise "unexpected token")))
                    '() )))

        (define (make-parse-quote-like name)
            (lambda (token)
                (let ((value-token (pop-token)))
                    (make-cons-syntax
                        (token->location token)
                        (make-atom-syntax 'symbol token name)
                        (make-cons-syntax
                            (token->location value-token)
                            (parse-next-with-token value-token)
                            (make-atom-syntax 'null token '()))))))

        (define parse-quote (make-parse-quote-like 'quote))
        (define parse-quasiquote (make-parse-quote-like 'quasiquote))
        (define parse-unquote (make-parse-quote-like 'unquote))
        (define parse-unquote-splicing (make-parse-quote-like 'unquote-splicing))

        (define (parse-cons lparen-token)
            (let* ((car-token (pop-token)) (car-type (token->type car-token)))
                (if car-token
                    (cond
                        ((equal? car-type 'rparen) (make-atom-syntax 'null car-token '()))
                        ((equal? car-type 'dot)
                            (let* ((cdr-token (pop-token)) (cdr-type (token->type cdr-token))
                                   (rparen-token (pop-token)) (rparen-type (token->type rparen-token)))
                                (if (equal? rparen-type 'rparen)
                                    (parse-next-with-token cdr-token)
                                    (raise "expected rparen after value in dot cdr position"))))
                        (else 
                            (let* ((car-syntax (parse-next-with-token car-token))
                                    (cdr-syntax (parse-cons lparen-token)))
                                (make-cons-syntax 
                                    (token->location lparen-token)
                                    car-syntax
                                    cdr-syntax))))
                    (raise "unexpected end of stream, expected parens"))))
        (parse-next)))
))