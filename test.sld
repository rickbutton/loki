(library (test)
(export bar)
(import 
  (only (rnrs (6)) define display newline))

(define (bar) 
  (display "bar!") 
  (newline)
  2))
