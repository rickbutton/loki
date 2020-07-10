(define-library (loki host)
(import (scheme base))
(import (scheme time))
(import (scheme eval))
(cond-expand 
  (chibi
    (import (chibi time))
    (import (chibi ast))))
(export host:unique-token host:eval)
(begin

(define (host:unique-token)
  (cond-expand
    (chibi (number->string (current-seconds) 32))
    (else (number->string (current-jiffy) 32))))

(define (host:eval x env)
  (eval x env))

))
