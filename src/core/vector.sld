(define-library (core vector)
(import (core primitives))
(import (core let))
(import (core control))
(import (core derived))
(import (core list))
(import (core number))
(import (core intrinsics))
(import (rename (core intrinsics) (%vector-set!   vector-set!)
                                  (%vector-ref    vector-ref)
                                  (%make-vector   make-vector)
                                  (%vector-length vector-length)
                                  (%bytevector-u8-set! bytevector-u8-set!)
                                  (%bytevector-u8-ref  bytevector-u8-ref)
                                  (%make-bytevector    make-bytevector)
                                  (%bytevector-length  bytevector-length)
                                  (%bytevector? bytevector?)))
(export vector list->vector vector->list
        vector-fill! vector-append vector-map
        vector-for-each vector-copy vector-copy!
        vector-set! vector-ref vector-length make-vector 
        vector->string string->vector
        bytevector bytevector-copy! bytevector-copy
        bytevector-append bytevector-u8-ref bytevector-u8-set!
        make-bytevector bytevector-length bytevector?)
(begin

(define (vector . args) (list->vector args))

(define (list->vector ls)
  (let ((vec (make-vector (length ls) #f)))
    (let lp ((ls ls) (i 0))
      (if (pair? ls)
          (begin
            (vector-set! vec i (car ls))
            (lp (cdr ls) (+ i 1)))))
    vec))

(define (vector->list vec . o)
  (let ((start (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (vector-length vec))))
    (let lp ((i (- end 1)) (res '()))
      (if (< i start) res (lp (- i 1) (cons (vector-ref vec i) res))))))

(define (vector-fill! vec ch . o)
  (let ((start (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (vector-length vec))))
    (let lp ((i (- end 1)))
      (if (>= i start) (begin (vector-set! vec i ch) (lp (- i 1)))))))

(define (vector-append . vecs)
  (let* ((len (apply + (map vector-length vecs)))
         (res (make-vector len)))
    (let lp ((ls vecs) (i 0))
      (if (null? ls)
          res
          (let ((v-len (vector-length (car ls))))
            (vector-copy! res i (car ls))
            (lp (cdr ls) (+ i v-len)))))))

(define (vector-map proc vec . lov)
  (if (null? lov)
      (let lp ((i (vector-length vec)) (res '()))
        (if (zero? i)
            (list->vector res)
            (lp (- i 1) (cons (proc (vector-ref vec (- i 1))) res))))
      (list->vector (apply map proc (map vector->list (cons vec lov))))))

(define (vector-for-each proc vec . lov)
  (if (null? lov)
      (let ((len (vector-length vec)))
        (let lp ((i 0))
          (cond ((< i len)
                 (proc (vector-ref vec i))
                 (lp (+ i 1))))))
      (apply for-each proc (map vector->list (cons vec lov)))))

(define (vector-copy vec . o)
  (let* ((start (if (pair? o) (car o) 0))
         (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (vector-length vec)))
         (res (make-vector (- end start))))
    (do ((i 0 (+ i 1)) (j start (+ j 1))) ((>= j end) res)
      (vector-set! res i (vector-ref vec j)))))

(define (vector-copy! to at from . o)
  (let* ((start (if (pair? o) (car o) 0))
         (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (vector-length from)))
         (limit (min end (+ start (- (vector-length to) at)))))
    (if (<= at start)
        (do ((i at (+ i 1)) (j start (+ j 1)))
            ((>= j limit))
          (vector-set! to i (vector-ref from j)))
        (do ((i (+ at (- end start 1)) (- i 1)) (j (- limit 1) (- j 1)))
            ((< j start))
          (vector-set! to i (vector-ref from j))))))

(define (vector->string vec . o)
  (list->string (apply vector->list vec o)))

(define (string->vector vec . o)
  (list->vector (apply string->list vec o)))

(define (bytevector . args)
  (let* ((len (length args))
         (res (make-bytevector len)))
    (do ((i 0 (+ i 1)) (ls args (cdr ls)))
        ((null? ls) res)
      (bytevector-u8-set! res i (car ls)))))

(define (bytevector-copy! to at from . o)
  (let* ((start (if (pair? o) (car o) 0))
         (end (if (and (pair? o) (pair? (cdr o)))
                  (cadr o)
                  (bytevector-length from)))
         (limit (min end (+ start (- (bytevector-length to) at)))))
    (if (<= at start)
        (do ((i at (+ i 1)) (j start (+ j 1)))
            ((>= j limit))
          (bytevector-u8-set! to i (bytevector-u8-ref from j)))
        (do ((i (+ at (- end start 1)) (- i 1)) (j (- limit 1) (- j 1)))
            ((< j start))
          (bytevector-u8-set! to i (bytevector-u8-ref from j))))))

(define (bytevector-copy from . o)
  (let* ((start (if (pair? o) (car o) 0))
         (end (if (and (pair? o) (pair? (cdr o)))
                  (cadr o)
                  (bytevector-length from)))
         (to (make-bytevector (- end start))))
    (bytevector-copy! to 0 from start end)
    to))


(define (bytevector-append . vecs)
  (let* ((len (apply + (map bytevector-length vecs)))
         (res (make-bytevector len)))
    (let lp ((ls vecs) (i 0))
      (if (null? ls)
          res
          (let ((v-len (bytevector-length (car ls))))
            (bytevector-copy! res i (car ls))
            (lp (cdr ls) (+ i v-len)))))))

))
