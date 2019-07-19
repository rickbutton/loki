(define-library
    (p03_syntax2scheme)
    (import (scheme base))
    (import (shared))
    (import (util))
    (export p03_syntax2scheme)
(begin

    (define (atom-syntax-type? syntax type)
        (and
            (atom-syntax? syntax)
            (equal? (atom-syntax->type syntax) type)))

    (define (symbol-syntax->scheme syntax)
        (let ((type (syntax-get-attr syntax 'type)) 
              (unique-id (syntax-get-attr syntax 'unique-id))
              (binding (syntax-get-attr syntax 'binding)))
            (cond
                ((equal? type 'reference) (make-variable unique-id binding))
                ((equal? type 'declaration) (make-variable unique-id binding))
                ((equal? type 'primitive) (atom-syntax->value syntax))
                ((equal? type 'intrinsic) 
                    (make-intrinsic (atom-syntax->value syntax)))
                (else (raise "unknown symbol type")))))

    (define (syntax->scheme syntax)
        (if (cons-syntax? syntax)
            (cons
                (syntax->scheme (cons-syntax->car syntax))
                (syntax->scheme (cons-syntax->cdr syntax)))
            (cond
                ((atom-syntax-type? syntax 'symbol) (symbol-syntax->scheme syntax))
                (else (atom-syntax->value syntax)))))

    (define (p03_syntax2scheme syntax) 
        (syntax->scheme syntax))
))
