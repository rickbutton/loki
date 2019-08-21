; tokens2syntax
; this pass converts a list of tokens into a syntax object.
; if the list of tokens cannot be represented by valid syntax objects, an error is thrown.

; this pass also converts quote/unquote syntax shortcuts into their long-form counterparts. 
; e.g. '123 => (quote 123)
; whether the quote/unquote syntax is valid in-context is not validated, and is checked in a later pass.
(define-library 
    (p01_tokens2syntax)
    (import (scheme base))
    (import (util))
    (import (shared))
    (export 
        p01_tokens2syntax)
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

(define (p01_tokens2syntax tokens)
    (let ((token-list tokens)
          (syntax-list '()))
        (define (peek-token)
            (if (null? token-list) #f (car token-list)))
        (define (pop-token)
            (if (null? token-list)
                #f
                (begin
                    (let ((token (car token-list)))
                        (set! token-list (cdr token-list))
                        token))))
        (define (push-syntax syntax)
            (set! syntax-list (cons syntax syntax-list)))

        (define (parse-next)
            (parse-next-with-token (pop-token)))

        (define (parse-next-with-token token)
            (let* ((type (token->type token)) (value (token->value token)) (location (token->location token)))
                (if token
                    (cond
                        ((equal? type 'lparen) (parse-cons token #t))
                        ;((equal? type 'lvector) (parse-vector))
                        ;((equal? type 'lbytevector) (parse-bytevector))
                        ((equal? type 'quote) (parse-quote token))
                        ((equal? type 'quasiquote) (parse-quasiquote token))
                        ((equal? type 'unquote) (parse-unquote token))
                        ((equal? type 'unquote-splicing) (parse-unquote-splicing token))
                        ((equal? type 'string) (syntax location value))
                        ((equal? type 'boolean) (syntax location value))
                        ((equal? type 'char) (syntax location value))
                        ((equal? type 'number) (syntax location value))
                        ((equal? type 'id) (syntax location value))
                        (else (raise "unexpected token")))
                    '() )))
        
        (define (make-parse-quote-like name)
            (lambda (token)
                (let ((value-token (pop-token)))
                    (syntax (token->location token) (cons
                        (syntax (token->location token) name)
                        (syntax (token->location value-token) (cons
                            (parse-next-with-token value-token)
                            '()
                            )))))))

        (define parse-quote (make-parse-quote-like 'quote))
        (define parse-quasiquote (make-parse-quote-like 'quasiquote))
        (define parse-unquote (make-parse-quote-like 'unquote))
        (define parse-unquote-splicing (make-parse-quote-like 'unquote-splicing))

        (define (parse-cons lparen-token first)
            (let* ((car-token (pop-token)) (car-type (token->type car-token)))
                (if car-token
                    (cond
                        ((equal? car-type 'rparen) 
                            (if first
                                (syntax (token->location car-token) '()))
                                '())
                        ((equal? car-type 'dot)
                            (let* ((cdr-token (pop-token)) (cdr-type (token->type cdr-token))
                                   (rparen-token (pop-token)) (rparen-type (token->type rparen-token)))
                                (if (equal? rparen-type 'rparen)
                                    (parse-next-with-token cdr-token)
                                    (raise "expected rparen after value in dot cdr position"))))
                        (else 
                            (let* ((car-syntax (parse-next-with-token car-token))
                                    (cdr-syntax (parse-cons lparen-token #f)))
                                (syntax (token->location lparen-token) (cons car-syntax cdr-syntax)))))
                    (raise "unexpected end of stream, expected parens"))))

        (define (parse)
            (push-syntax (parse-next))
            (if (peek-token) (parse)))

        (define (syntax-list->begin* syntax-list)
            (if (null? syntax-list) 
                '()
                (syntax #f (cons (car syntax-list) (syntax-list->begin* (cdr syntax-list))))))
            
        (define (syntax-list->begin syntax-list)
            (cond
                ((null? syntax-list) '())
                ((eq? (length syntax-list) 1) (car syntax-list))
                (else (syntax #f (cons
                        (syntax #f 'begin)
                        (syntax-list->begin* syntax-list))))))

        (parse)
        (syntax-list->begin (reverse syntax-list))))
))
