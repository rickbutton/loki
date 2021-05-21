(define-library (scheme repl)
  (import (except (loki core primitives) eval environment))
  (import (scheme eval))
  (export interaction-environment)
  (begin
   (define (interaction-environment)
     (environment '(scheme base)))))

