(define-library (srfi 31 test)
  (export run-tests)
  (import
   (scheme base)
   (scheme lazy)
   (srfi 31)
   (loki test))
  (begin
    (define-tests run-tests "(srfi 31)"
      (test-eqv "factorial" 3628800
        ((rec (! n)
           (if (zero? n)
               1
               (* n (! (- n 1)))))
         10))
      (test-eqv "lazy stream" 'x
        (car (force (cdr (force (cdr (rec xs (cons 'x (delay xs))))))))))))
