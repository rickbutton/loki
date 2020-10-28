(define-library (loki compiler util)
(import (scheme base))
(import (scheme time))
(import (scheme write))
(import (srfi 1))
(import (loki util))
(export for-all
        map-while
        flatten
        dotted-memp
        dotted-map
        dotted-length
        dotted-butlast
        dotted-last
        check-set?
        unionv
        drop-tail
        join
        compose
        generate-guid
        check
        assert
        call-with-string-output-port)
(begin

(define (map-while f lst k)
  (cond ((null? lst) (k '() '()))
        ((pair? lst)
         (let ((head (f (car lst))))
           (if head
               (map-while f
                          (cdr lst)
                          (lambda (answer rest)
                            (k (cons head answer)
                               rest)))
               (k '() lst))))
        (else  (k '() lst))))

(define (flatten l)
  (cond ((null? l) l)
        ((pair? l) (cons (car l)
                         (flatten (cdr l))))
        (else (list l))))

(define (dotted-memp proc ls)
  (cond ((null? ls) #f)
        ((pair? ls) (if (proc (car ls))
                        ls
                        (dotted-memp proc (cdr ls))))
        (else (and (proc ls)
                   ls))))

(define (dotted-map f lst)
  (cond ((null? lst) '())
        ((pair? lst) (cons (f (car lst))
                           (dotted-map f (cdr lst))))
        (else (f lst))))

;; Returns 0 also for non-list a la SRFI-1 protest.

(define (dotted-length dl)
  (cond ((null? dl) 0)
        ((pair? dl) (+ 1 (dotted-length (cdr dl))))
        (else 0)))

(define (dotted-butlast ls n)
  (let recurse ((ls ls)
                (length-left (dotted-length ls)))
    (cond ((< length-left n) (error "dotted-butlast: List too short" ls n))
          ((= length-left n) '())
          (else
           (cons (car ls)
                 (recurse (cdr ls)
                          (- length-left 1)))))))

(define (dotted-last ls n)
  (let recurse ((ls ls)
                (length-left (dotted-length ls)))
    (cond ((< length-left n) (error "dotted-last: List too short" ls n))
          ((= length-left n) ls)
          (else
           (recurse (cdr ls)
                    (- length-left 1))))))

(define (check-set? ls = fail)
  (or (null? ls)
      (if (memp (lambda (x)
                  (= x (car ls)))
                (cdr ls))
          (fail (car ls))
          (check-set? (cdr ls) = fail))))

(define (unionv . sets)
  (cond ((null? sets) '())
        ((null? (car sets))
         (apply unionv (cdr sets)))
        (else
         (let ((rest (apply unionv
                            (cdr (car sets))
                            (cdr sets))))
           (if (memv (car (car sets)) rest)
               rest
               (cons (car (car sets)) rest))))))

(define (drop-tail list tail)
  (cond ((null? list)    '())
        ((eq? list tail) '())
        (else
         (cons (car list)
               (drop-tail (cdr list) tail)))))

(define (join e separator)
  (define (tostring x)
    (cond ((symbol? x)
           (symbol->string x))
          ((number? x)
           (number->string x))
          (else
           (error "join: Invalid argument." e))))
  (if (null? e)
      ""
      (string-append
       (tostring (car e))
       (apply string-append
              (map (lambda (x)
                     (string-append separator (tostring x)))
                   (cdr e))))))

(define (compose f g)
  (lambda (x) (f (g x))))

;; Generate-guid returns a fresh symbol that has a globally
;; unique external representation and is read-write invariant.
;; Your local gensym will probably not satisfy both conditions.
;; Prefix makes it disjoint from all builtins.
;; Uniqueness is important for incremental and separate expansion.

(define guid-prefix "&")
(define (unique-token)
  (number->string (current-jiffy) 32))
(define generate-guid
  (let ((token (unique-token))
        (ticks 0))
    (lambda (symbol)
      (set! ticks (+ ticks 1))
      (string->symbol
       (string-append guid-prefix
                      (symbol->string symbol)
                      "~"
                      token
                      "~"
                      (number->string ticks))))))

(define (check x p? from)
  (or (p? x)
      (syntax-violation from "Invalid argument" x)))

(define-syntax assert
  (syntax-rules ()
    ((assert e)
      (let ((e2 e))
        (if e2 e2 (error "assertion failed" 'e))))))

(define (call-with-string-output-port proc)
    (define port (open-output-string))
    (proc port)
    (get-output-string port))

))
