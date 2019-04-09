(define-library 
    (util)
    (import (scheme base))
    (export 
        caddr
        cadddr
        cdddr
        find
        filter
        fold-left
        fold-right
        list-count
        last
        all-but-last
        index
        contains?
        reduce
        range
        makeid)
(begin

(define (caddr x) (car (cdr (cdr x))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdddr x) (cdr (cdr (cdr x))))

(define (find pred list)
    (if (null? list) #f
        (if (pred (car list))
            (car list)
            (find pred (cdr list)))))

(define filter
    (lambda (pred lst)
    (cond ((null? lst) '())
            ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
            (else (filter pred (cdr lst))))))

(define (fold-left f init seq) 
    (if (null? seq) 
        init 
        (fold-left f 
                    (f (car seq) init) 
                    (cdr seq)))) 

(define (fold-right f init seq) 
    (if (null? seq) 
        init 
        (f (car seq) 
            (fold-right f init (cdr seq))))) 

(define (list-count-internal l count)
        (if (null? l) count
            (list-count-internal (cdr l) (+ count 1))))

(define (list-count l) (list-count-internal l 0))

(define (last l)
    (cond ((null? l) '())
        ((null? (cdr l)) (car l))
        (else (last (cdr l)))))

(define (all-but-last l) (reverse (cdr (reverse l))))

(define (index a b)
    (let ((tail (member a (reverse b))))
        (and tail (length (cdr tail)))))

(define (contains? l i)
    (if (null? l) #f
        (or (eq? (car l) i) (contains? (cdr l) i))))

(define (reduce fn base-value lis)
    (if (null? lis)
        base-value
        (fn (car lis)
            (reduce fn base-value (cdr lis)))))
    
(define (range start end step)
    (reverse (range-reversed '() start end step)))
(define (range-reversed fold-var pos end step)
    (if (if (>= step 0)
            (< pos end)
            (> pos end))
        (range-reversed
        (cons pos fold-var)
        (+ pos step)
        end
        step)
        fold-var))

(define (makeid prefix)
    (let ((count 0))
        (lambda () 
            (set! count (+ 1 count))
            (string->symbol (string-append prefix (number->string count))))))))