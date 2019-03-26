(define-module (pass97)
    #:export (flatten-cps))

(use-modules (util))

(define (close? x) (and (list? x) (eq? (car x) 'close)))
(define (close->body x) (car (cdr x)))
(define (close->next x) (car (cdr (cdr x))))

(define (flatten-close x)
    (cons `(close ,(flatten-cps (close->body x))) (flatten-cps (close->next x))))

(define (flatten-cps x)
    (cond
        ((eq? (length x) 1) (cons x '()))
        ((close? x) (flatten-close x))
        (else (cons (all-but-last x) (flatten-cps (last x))))))