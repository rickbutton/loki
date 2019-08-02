(define-library 
    (util)
    (import (scheme base))
    (import (scheme read))
    (import (scheme eval))
    (import (scheme write))
    (import (srfi 159))
    (import (chibi show pretty))
    (export 
        caddr
        cadddr
        cdddr
        unique
        find
        filter
        fold-left
        fold-right
        last
        all-but-last
        index
        contains?
        range
        debug
        string-join
        make-anon-id
        make-named-id)
(begin

(define (caddr x) (car (cdr (cdr x))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdddr x) (cdr (cdr (cdr x))))

(define (unique lst)
  (fold-right (lambda (e a)
        (if (not (member e a))
            (cons e a)
            a))
        '()
         lst))

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

(define-syntax debug
    (syntax-rules ()
        ((debug exp)
            (begin
                (display (show #f (quote exp)))
                (display ": ")
                (display (show #f exp))
                (display "\n")))))

(define (string-join strings delimiter)
    (if (null? strings)
        ""
        (fold-right (lambda (s so-far) (string-append so-far delimiter s))
            (car strings)
            (cdr strings))))

(define (make-anon-id prefix)
    (let ((count 0))
        (lambda () 
            (set! count (+ 1 count))
            (string->symbol (string-append prefix (number->string count))))))

(define (make-named-id prefix)
    (let ((count 0))
        (lambda (name) 
            (set! count (+ 1 count))
            (string->symbol (string-append prefix (number->string count) "_" (symbol->string name))))))))
