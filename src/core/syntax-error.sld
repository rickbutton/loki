(define-library (core syntax-error)
(import (for (core primitives) expand run)
        (for (core apply) expand)
        (for (core exception) expand))
(export syntax-error)
(begin

(define-syntax syntax-error 
  (lambda (x)
    (syntax-case x ()
      ((_ . args)
        (apply error (syntax->datum (syntax args)))))))

))
