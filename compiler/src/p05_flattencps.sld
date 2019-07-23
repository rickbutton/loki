(define-library 
(p05_flattencps)
(import (scheme base))
(import (util))
(export p05_flattencps)
(begin

(define (close? x) (and (list? x) (eq? (car x) 'close)))
(define (close->bindings x) (cadr x))
(define (close->body x) (caddr x))
(define (close->next x) (cadddr x))

(define (flatten-close x)
    (cons `(close ,(close->bindings x) ,(p05_flattencps (close->body x))) (p05_flattencps (close->next x))))

(define (test? x) (and (list? x) (eq? (car x) 'test)))
(define (test->consequent x) (cadr x))
(define (test->alternate x) (caddr x))
(define (test->next x) (cadddr x))
(define (flatten-test x)
    (cons `(test ,(p05_flattencps (test->consequent x))
                 ,(p05_flattencps (test->alternate x)))
                 (p05_flattencps (test->next x))))

(define (p05_flattencps x)
    (cond
        ((eq? (length x) 1) (cons x '()))
        ((close? x) (flatten-close x))
        ((test? x) (flatten-test x))
        (else (cons (all-but-last x) (p05_flattencps (last x))))))))
