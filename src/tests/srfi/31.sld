(define-library (tests srfi 31)
  (export run-tests-srfi-31)
  (import
   (scheme base)
   (scheme lazy)
   (srfi 31)
   (srfi 64)
   (tests util))
  (begin
    (define-tests run-tests-srfi-31 "(srfi 31)"
      (test-eqv "factorial" 3628800
        ((rec (! n)
           (if (zero? n)
               1
               (* n (! (- n 1)))))
         10))
      (test-eqv "lazy stream" 'x
        (car (force (cdr (force (cdr (rec xs (cons 'x (delay xs))))))))))))
