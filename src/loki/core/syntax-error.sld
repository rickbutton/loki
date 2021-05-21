(define-library (loki core syntax-error)
  (import (for (loki core primitives) expand run)
          (for (loki core apply) expand)
          (for (loki core exception) expand))
  (export syntax-error)
  (begin
   
   (define-syntax syntax-error
     (lambda (x)
       (syntax-case x ()
                    ((_ . args)
                     (apply error (syntax->datum (syntax args)))))))
   
   ))
