(define-library 
(p03_flattencps)
(import (scheme base))
(import (util))
(export p03_flattencps)
(begin

(define (close? x) (and (list? x) (eq? (car x) 'close)))
(define (close->body x) (car (cdr x)))
(define (close->next x) (car (cdr (cdr x))))

(define (flatten-close x)
    (cons `(close ,(p03_flattencps (close->body x))) (p03_flattencps (close->next x))))

(define (p03_flattencps x)
    (cond
        ((eq? (length x) 1) (cons x '()))
        ((close? x) (flatten-close x))
        (else (cons (all-but-last x) (p03_flattencps (last x))))))))