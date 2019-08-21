(define-library
    (p03_syntax2scheme)
    (import (scheme base))
    (import (shared))
    (import (util))
    (export p03_syntax2scheme)
(begin

    (define (symbol-syntax->scheme syntax)
        (let ((type (syntax-get-attr syntax 'type)) 
              (unique-id (syntax-get-attr syntax 'unique-id)))
            (cond
                ((equal? type 'reference) (make-variable unique-id))
                ((equal? type 'declaration) (make-variable unique-id))
                ((equal? type 'primitive) (syntax->value syntax))
                ((equal? type 'intrinsic) 
                    (make-intrinsic (syntax->value syntax)))
                (else (syntax->value syntax)))))

    (define (syntax->scheme syntax)
        (if (pair-syntax? syntax)
            (cons
                (syntax->scheme (safe-car-syntax syntax))
                (syntax->scheme (safe-cdr-syntax syntax)))
            (cond
                ((null? syntax) '())
                ((symbol? (syntax->value syntax)) (symbol-syntax->scheme syntax))
                (else (syntax->value syntax)))))

    (define (p03_syntax2scheme syntax) 
        (syntax->scheme syntax))
))
