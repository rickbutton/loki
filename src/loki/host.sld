(define-library (loki host)
(import (scheme base))
(import (scheme time))
(import (scheme write))
(import (scheme eval))
(cond-expand 
  (chibi
    (import (chibi time))
    (import (chibi ast))
    (import (rename (chibi ast) (type-printer-set! chibi-type-printer-set!)))))
(export ex:unique-token type-printer-set! host-eval)
(begin

(define (ex:unique-token)
  (cond-expand
    (chibi (number->string (current-seconds) 32))
    (else (number->string (current-jiffy)))))

(define (type-printer-set! type printer)
  (cond-expand
    (chibi 
      (chibi-type-printer-set! type 
          (lambda (x writer out) (printer x out))))
    (else #f)))

(define (host-eval x env)
  (cond-expand
    (else (eval x env))))

))
