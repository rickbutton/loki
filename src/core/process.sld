(define-library (core process)
(import (core primitives))
(import (core let))
(import (core intrinsics))
(import (core list))
(import (rename (core intrinsics) (%emergency-exit emergency-exit)))
(export command-line get-environment-variables get-environment-variable
        emergency-exit exit)
(begin

(define (command-line) (list-copy %command-line))
(define (get-environment-variables) (list-copy %environment-variables))
(define (get-environment-variable name)
  (let ((var (assoc name (get-environment-variables))))
    (if var (cdr var) #f)))

(define exit emergency-exit)
))
