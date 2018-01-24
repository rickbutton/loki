(library (src/util)
    (export list-count last index contains? reduce print-help-and-exit push-list-top)
    (import (rnrs) (rnrs mutable-pairs (6)))

    (define (list-count-internal l count)
        (if (null? l) count
            (list-count-internal (cdr l) (+ count 1))))
    (define (list-count l) (list-count-internal l 0))

    (define (last l)
        (cond ((null? l) '())
            ((null? (cdr l)) (car l))
            (else (last (cdr l)))))

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

    (define (print-help-and-exit) 
        (display "arguments: compiler.scm [input.scm] [out.wat]")
        (exit))

    (define (push-list-top l v)
        (let ((lcar (car l)) (lcdr (cdr l)))
            (set-car! l v)
            (set-cdr! l (cons lcar lcdr))
            l))
)