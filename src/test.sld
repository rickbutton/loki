(define-library (test)
(export bar)
(import (scheme base))
(import (scheme write))
(begin

(define (bar) (display "bar!!!"))))
