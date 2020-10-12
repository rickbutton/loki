(define-library (loki core syntax-rules)
  (export syntax-rules)
  (import (for (loki core primitives)        expand run)
          (for (loki core with-syntax)       expand run)
          (for (loki core intrinsics)        expand))
  (begin

    (define-syntax syntax-rules
      (lambda (x)
        (syntax-case x ()
          ((_ (k ...) (pattern template) ...)
           (syntax (lambda (x)
                    (syntax-case x (k ...)
                      (pattern (syntax template))
                      ...)))))))
  
  ))

