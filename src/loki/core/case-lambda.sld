(define-library (loki core case-lambda)
  (export case-lambda)
  (import (for (loki core primitives)   expand run)
          (for (loki core let)          expand run)
          (for (loki core apply)        expand run)
          (for (loki core syntax-rules) expand)
          (for (loki core list)         expand run)
          (for (loki core exception)    expand run)
          (for (loki core quasisyntax)   expand run)
          (for (loki core intrinsics)   expand run))
  (begin
   
   (define-syntax case-lambda
     (syntax-rules ()
                   ((_ (fmls b1 b2 ...))
                    (lambda fmls b1 b2 ...))
                   ((_ (fmls b1 b2 ...) ...)
                    (lambda args
                      (let ((n (length args)))
                        (case-lambda-help args n
                                          (fmls b1 b2 ...) ...))))))
   
   #;(define-syntax case-lambda-help
   (syntax-rules ()
                 ((_ args n)
                  (error "unexpected number of arguments"))
                 ((_ args n ((x ...) b1 b2 ...) more ...)
                  (if (%number-eq n (length '(x ...)))
                      (apply (lambda (x ...) b1 b2 ...) args)
                    (case-lambda-help args n more ...)))
                 ((_ args n ((x1 x2 ... . r) b1 b2 ...) more ...)
                  (if (%gte n (length '(x1 x2 ...)))
                      (apply (lambda (x1 x2 ... . r) b1 b2 ...)
                             args)
                    (case-lambda-help args n more ...)))
                 ((_ args n (r b1 b2 ...) more ...)
                  (apply (lambda r b1 b2 ...) args))))
  
  (define-syntax case-lambda-help (lambda (e)
                                    (syntax-case e ()
                                                 ((_ args n)
                                                  (syntax (error "unexpected number of arguments")))
                                                 ((_ args n ((x ...) b1 b2 ...) more ...)
                                                  (let ((n2 (length (syntax (x ...)))))
                                                    (quasisyntax (if (%number-eq n (unsyntax n2))
                                                                     (apply (lambda (x ...) b1 b2 ...) args)
                                                                   (case-lambda-help args n more ...)))))
                                                 ((_ args n ((x1 x2 ... . r) b1 b2 ...) more ...)
                                                  (let ((n2 (length (syntax (x1 x2 ...)))))
                                                    (quasisyntax (if (%gte n (unsyntax n2))
                                                                     (apply (lambda (x1 x2 ... . r) b1 b2 ...)
                                                                            args)
                                                                   (case-lambda-help args n more ...)))))
                                                 ((_ args n (r b1 b2 ...) more ...)
                                                  (syntax (apply (lambda r b1 b2 ...) args))))))
  ))

