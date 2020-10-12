(define-library (srfi 145)
  (export assume)
  (import (scheme base))
  (cond-expand
    (loki (import (loki core syntax-error)))
    (gauche (import (only (gauche base) syntax-error))))
  (begin
    (define-syntax assume
      (syntax-rules ()
        ((assume expression message ...)
         (unless expression
           (error "invalid assumption" (quote expression) (list message ...))))
        ((assume . _)
         (syntax-error "invalid assume syntax"))))))
