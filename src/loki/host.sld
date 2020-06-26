(define-library (loki host)
(import (scheme base))
(cond-expand 
  (loki)
  (chibi (import (chibi time)
                 (rename (chibi ast) (type-printer-set! chibi-type-printer-set!)))))
(export ex:unique-token type-printer-set!)
(begin

(define (ex:unique-token)
  (cond-expand
    (chibi (number->string (current-seconds) 32))
    (loki "")))

(define (type-printer-set! type printer)
  (cond-expand
    (chibi 
      (chibi-type-printer-set! type 
          (lambda (x writer out) (printer x out))))
    (loki #f)))

))
