(import (scheme base))
(import (scheme char))
(import (scheme write))
(import (scheme read))
(import (scheme eval))
(import (scheme process-context))
(import (loki reader))
(import (core intrinsics))

(define env (environment '(scheme base)))

(define (repl)
  (guard (error (else (handle-error error)))
    (display "> ")
    (flush-output-port (current-output-port))
    (let* ((datum (read (current-input-port))))
      (if (eof-object? datum) (exit 0))
      (display (eval datum env))
      (newline)))
  (repl))
                
(define (handle-error error)
  (display error)
  (newline))

(repl)
