(define-library 
    (loki util)
    (import (scheme base))
    (import (scheme read))
    (import (scheme eval))
    (import (scheme write))
    (import (loki compat))
    (export 
        map-vector
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
        pretty-print
        string-join
        make-anon-id
        make-named-id
        fluid-let
        assert
        call-with-string-output-port)
(begin

(define (map-vector fn vec)
    (list->vector (map fn (vector->list vec))))

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

(define (filter p? lst)
  (if (null? lst)
      '()
      (if (p? (car lst))
          (cons (car lst)
                (filter p? (cdr lst)))
          (filter p? (cdr lst)))))

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

(define (debug . args)
    (map (lambda (a)
        (display a)
        (display " ")) args)
    (display "\n\n"))

(define (pretty-print exp)
    (display exp))
    ;(display (show #f (pretty exp))))

(define (make-anon-id prefix)
    (let ((count 0))
        (lambda () 
            (set! count (+ 1 count))
            (string->symbol (string-append prefix (number->string count))))))

(define (make-named-id prefix)
    (let ((count 0))
        (lambda (name) 
            (set! count (+ 1 count))
            (string->symbol (string-append prefix (number->string count) "_" (symbol->string name))))))

(define-syntax fluid-let
      (syntax-rules ()
        ((fluid-let () be ...)
         (begin be ...))
        ((fluid-let ((p0 e0) (p e) ...) be ...)
         (let ((saved p0))
           (set! p0 e0)
           (call-with-values (lambda ()
                               (fluid-let ((p e) ...) be ...))
             (lambda results
               (set! p0 saved)
               (apply values results)))))))

(define (assert e)
    (if e e (raise e)))

(define (call-with-string-output-port proc)
    (define port (open-output-string))
    (proc port)
    (get-output-string port))
))
