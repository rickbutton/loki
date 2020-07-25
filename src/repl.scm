(import (scheme base))
(import (scheme write))
(import (scheme read))
(import (scheme eval))

(define env (environment '(scheme base)))

(define (repl)
  (guard (error (else (handle-error error)))
    (display "> ")
    (let* ((datum (read (current-input-port)))
           (result (eval datum env)))
      (display result)
      (newline)))
  (repl))
                
(define (handle-error error)
  (display error)
  (newline))

(repl)
