(define-library (loki core control)
  (export when unless do)
  (import (for (loki core primitives)   expand run)
          (for (loki core let)          expand run)
          (for (loki core with-syntax)  expand)
          (for (loki core syntax-rules) expand)
          (for (loki core bool)         expand run)
          (for (loki core list)         expand run)
          (for (loki core intrinsics) expand run))
  (begin
   
   (define-syntax when
     (syntax-rules ()
                   ((when test result1 result2 ...)
                    (if test
                        (begin result1 result2 ...)))))
   
   (define-syntax unless
     (syntax-rules ()
                   ((unless test result1 result2 ...)
                    (if (not test)
                        (begin result1 result2 ...)))))
   
   (define-syntax do
     (lambda (orig-x)
             (syntax-case orig-x ()
                          ((_ ((var init . step) ...) (e0 e1 ...) c ...)
                           (with-syntax (((step ...)
                                          (map (lambda (v s)
                                                       (syntax-case s ()
                                                                    (()  v)
                                                                    ((e) (syntax e))
                                                                    (_   (syntax-violation 'do "Invalid step" orig-x s))))
                                               (syntax (var ...))
                                               (syntax (step ...)))))
                                        (syntax-case (syntax (e1 ...)) ()
                                                     (()          (syntax (let do ((var init) ...)
                                                                               (if (not e0)
                                                                                   (begin c ... (do step ...))))))
                                                     ((e1 e2 ...) (syntax (let do ((var init) ...)
                                                                               (if e0
                                                                                   (begin e1 e2 ...)
                                                                                   (begin c ... (do step ...))))))))))))
   
   
   
   )) ; core control

