(define-library (loki core with-syntax)
  (export with-syntax)
  (import (for (loki core primitives) run expand)
          (for (loki core intrinsics) run expand))
  (begin
   
   (define (list . args) args)
   (define-syntax with-syntax
     (lambda (x)
             (syntax-case x ()
                          ((_ ((p e0) ...) e1 e2 ...)
                           (syntax (syntax-case (list e0 ...) ()
                                                ((p ...) (begin e1 e2 ...))))))))
   ))

