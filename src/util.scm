(define-module (util)
    #:export (fold-left fold-right list-count last all-but-last index contains? reduce range))

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
    
(define (cps->port x port)
    (if (eq? (length x) 1)
        (begin
        (write (list (car x)) port)
        (newline port))
        (begin
            (write (all-but-last x) port)
            (newline port)
            (flatten-cps (last x) port))))

(define (range start end step)
  (reverse
   (range-reversed '() start end step)))
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