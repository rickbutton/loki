(define-library (core string)
(import (core primitives))
(import (core apply))
(import (core let))
(import (core control))
(import (core derived))
(import (core list))
(import (core vector))
(import (core number))
(import (core bool))
(import (core math))
(import (core intrinsics))
(import (core char))
(import (core define-missing))
(import (rename (core intrinsics) (%string-set!   string-set!)
                                  (%string-ref    string-ref)
                                  (%make-string   make-string)
                                  (%string-length string-length)
                                  (%string->symbol string->symbol)
                                  (%symbol->string symbol->string)
                                  (%string-downcase string-downcase)
                                  (%string-upcase string-upcase)
                                  (%string-foldcase string-foldcase)))
(export make-string string string-length string-ref string-set!
        string-append string-map string-for-each string-fill!
        string-copy string-copy! string->list list->string
        vector->string string->vector digit-value
        string<=? string<? string>=? string>? string=?
        string-ci<=? string-ci<?
        string-ci=? string-ci>=?
        string-ci>? string-downcase
        string-foldcase string-upcase string->symbol symbol->string
        string->utf8 utf8->string)
(begin

(define (string . args) (list->string args))

(define (list->string ls)
  (let ((str (make-string (length ls) #\0)))
    (let lp ((ls ls) (i 0))
      (if (pair? ls)
          (begin
            (string-set! str i (car ls))
            (lp (cdr ls) (+ i 1)))))
    str))

(define (string->list str . o)
  (let ((start (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (string-length str))))
    (let lp ((i (- end 1)) (res '()))
      (if (< i start) res (lp (- i 1) (cons (string-ref str i) res))))))

(define (string-append . strs)
  (let* ((len (apply + (map string-length strs)))
         (res (make-string len)))
    (let lp ((ls strs) (i 0))
      (if (null? ls)
          res
          (let ((s-len (string-length (car ls))))
            (string-copy! res i (car ls))
            (lp (cdr ls) (+ i s-len)))))))

(define (string-map proc str . los)
  (if (null? los)
      (let lp ((i (string-length str)) (res '()))
        (if (zero? i)
            (list->string res)
            (lp (- i 1) (cons (proc (string-ref str (- i 1))) res))))
      (list->string (apply map proc (map string->list (cons str los))))))

(define (string-for-each proc str . los)
  (if (null? los)
      (let ((len (string-length str)))
        (let lp ((i 0))
          (cond ((< i len)
                 (proc (string-ref str i))
                 (lp (+ i 1))))))
      (apply for-each proc (map string->list (cons str los)))))

(define (string-copy str . o)
  (let* ((start (if (pair? o) (car o) 0))
         (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (string-length str)))
         (res (make-string (- end start))))
    (do ((i 0 (+ i 1)) (j start (+ j 1))) ((>= j end) res)
      (string-set! res i (string-ref str j)))))

(define (string-copy! to at from . o)
  (let* ((start (if (pair? o) (car o) 0))
         (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (string-length from)))
         (limit (min end (+ start (- (string-length to) at)))))
    (if (<= at start)
        (do ((i at (+ i 1)) (j start (+ j 1)))
            ((>= j limit))
          (string-set! to i (string-ref from j)))
        (do ((i (+ at (- end start 1)) (- i 1)) (j (- limit 1) (- j 1)))
            ((< j start))
          (string-set! to i (string-ref from j))))))

(define (string-fill! str ch . o)
  (let ((start (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (string-length str))))
    (let lp ((i (- end 1)))
      (if (>= i start) (begin (string-set! str i ch) (lp (- i 1)))))))

(define (vector->string vec . o)
  (list->string (apply vector->list vec o)))

(define (string->vector vec . o)
  (list->vector (apply string->list vec o)))

(define (digit-value ch)
  (let ((res (- (char->integer ch) (char->integer #\0))))
    (if (<= 0 res 9)
        res
        ch)))

(define string-cursor? number?)
(define string-cursor=? eq?)
(define string-cursor<? <)
(define string-cursor<=? <=)
(define string-cursor>? >)
(define string-cursor>=? >=)
(define (string-index->cursor str i) i)
(define (string-cursor->index str off) off)
(define (string-cursor-offset str off) off)
(define string-size string-length)
(define substring-cursor substring)
(define (string-cursor-start s) 0)
(define string-cursor-end string-length)
(define string-cursor-ref string-ref)
(define (string-cursor-next s i) (+ i 1))
(define (string-cursor-prev s i) (- i 1))

(define (string-cmp-ls op ci? s ls)
  (if (null? ls)
      #t
      (and (op (%string-cmp s (car ls) ci?) 0)
           (string-cmp-ls op ci? (car ls) (cdr ls)))))

(define (string=? s . ls) (string-cmp-ls eq? #f s ls))
(define (string<? s . ls) (string-cmp-ls < #f s ls))
(define (string>? s . ls) (string-cmp-ls > #f s ls))
(define (string<=? s . ls) (string-cmp-ls <= #f s ls))
(define (string>=? s . ls) (string-cmp-ls >= #f s ls))

(define (string-ci=? s . ls) (string-cmp-ls eq? #t s ls))
(define (string-ci<? s . ls) (string-cmp-ls < #t s ls))
(define (string-ci>? s . ls) (string-cmp-ls > #t s ls))
(define (string-ci<=? s . ls) (string-cmp-ls <= #t s ls))
(define (string-ci>=? s . ls) (string-cmp-ls >= #t s ls))

(define-missing string->utf8 utf8->string)

))
