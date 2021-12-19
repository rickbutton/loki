(define-library (loki profile)
(export time)
(cond-expand (gauche (import (gauche time)))
             (loki (import (scheme base))
                   (import (loki syntax))
                   (begin

(define-syntax time
  (lambda (x)
  (syntax-case () x
    ((test e) (syntax e)))))

))))
