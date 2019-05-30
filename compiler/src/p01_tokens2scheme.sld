(define-library 
    (p01_tokens2scheme.sld)
    (import (scheme base))
    (import (util))
    (import (shared))
    (export 
        p01_tokens2scheme)
(begin

; dot
; unquote-splice
; unquote

; rparen

; lparen
; lvector
; lbytevector
; quote
; quasiquote
; string
; boolean
; char
; number
; id

(define-record-type <cons-syntax>
    (make-cons-syntax paren-token car cdr)
    cons-syntax?
    (paren-token (cons-syntax->paren-token))
    (car (cons-syntax->car))
    (cdr (cons-syntax->cdr)))

(define-record-type <atom-syntax>
    (make-atom-syntax type token value)
    atom-syntax?
    (type (atom-syntax->type))
    (token (atom-syntax->token))
    (value (atom-syntax->value)))

(define (p01_tokens2scheme tokens)
    (let ((token-list (reverse tokens)))
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
                        ((equal? type 'lparen) (parse-cons))
                        ;((equal? type 'lvector) (parse-vector))
                        ;((equal? type 'lbytevector) (parse-bytevector))
                        ;((equal? type 'quote) (parse-quote))
                        ;((equal? type 'quasiquote) (parse-quasiquote))
                        ((equal? type 'string) (make-atom-syntax 'string token value))
                        ((equal? type 'boolean) (make-atom-syntax 'boolean token value))
                        ((equal? type 'char) (make-atom-syntax 'char token value))
                        ((equal? type 'number) (make-atom-syntax 'number token value))
                        ((equal? type 'id) (make-atom-syntax 'symbol token value))
                        (else (raise "unexpected token")))
                    '() )))
        
        (define (parse-cons lparen-token)
            (let* ((car-token (pop-token)) (car-type (token->type car-token)))
                (if car-token
                    (cond
                        ((equal? car-type 'rparen) (make-atom-syntax 'null car-token '()))
                        ((equal? car-type 'dot)
                            (let* ((cdr-token (pop-token)) (cdr-type (token->type cdr-token))
                                   (rparen-token (pop-token)) (rparen-type (token->type rparen-token)))
                                (if (equal? rparen-type 'rparen)
                                    (make-cons-syntax 
                                        (parse-next-with-token car-token)
                                        (parse-next-with-token cdr-token))
                                    (raise "expected rparen after value in dot cdr position"))))
                        (else (make-cons-syntax (parse-next) (parse-cons))))
                    (raise "unexpected end of stream, expected parens"))))
            ))

))