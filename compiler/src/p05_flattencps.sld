(define-library 
(p05_flattencps)
(import (scheme base))
(import (util))
(export p05_flattencps)
(begin

(define (close? x) (and (list? x) (eq? (car x) 'close)))
(define (close->bindings x) (cadr x))
(define (close->body x) (caddr x))
(define (close->next x) (car (cdr (cdr x))))
(define (close->next x) (cadddr x))

(define (flatten-close x)
    (cons `(close ,(close->bindings x) ,(p05_flattencps (close->body x))) (p05_flattencps (close->next x))))

(define (p05_flattencps x)
    (cond
        ((eq? (length x) 1) (cons x '()))
        ((close? x) (flatten-close x))
        (else (cons (all-but-last x) (p05_flattencps (last x))))))))
