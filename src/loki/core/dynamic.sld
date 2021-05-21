(define-library (loki core dynamic)
  (import (loki core intrinsics))
  (import (loki core primitives))
  (import (loki core let))
  (import (loki core apply))
  (import (loki core derived))
  (import (loki core case-lambda))
  (import (loki core intrinsics))
  (import (loki core bool))
  (import (loki core number))
  (import (loki core list))
  (import (loki core exception))
  (import (loki core values))
  (import (loki core records))
  (import (for (loki core syntax-rules) expand))
  (export make-parameter parameterize guard dynamic-wind
          call/cc
          (rename call/cc call-with-current-continuation)
          with-exception-handler)
  (begin
   
   (define make-parameter
     (case-lambda
      ((init)
       (make-parameter init (lambda (x) x)))
      ((init converter)
       (let ((value (converter init)))
         (case-lambda
          (() value)
          ((val) (set! value (converter val)) value))))))
   
   (define-syntax parameterize
     (syntax-rules ()
                   ((parameterize ("step")
                                  ((param value p old new) ...)
                                  ()
                                  body)
                    (let ((p param) ...)
                      (let ((old (p)) ...)
                        (dynamic-wind
                         (lambda () (p value) ...)
                         (lambda () . body)
                         (lambda () (p old) ...)))))
                   ((parameterize ("step")
                                  args
                                  ((param value) . rest)
                                  body)
                    (parameterize ("step")
                                  ((param value p old new) . args)
                                  rest
                                  body))
                   ((parameterize ((param value) ...) . body)
                    (parameterize ("step")
                                  ()
                                  ((param value) ...)
                                  body))))
   
   
   (define-syntax guard
     (syntax-rules ()
                   ((guard (var clause ...) e1 e2 ...)
                    ((call/cc
                      (lambda (guard-k)
                        (with-exception-handler
                         (lambda (condition)
                           ((call/cc
                             (lambda (handler-k)
                               (guard-k
                                (lambda ()
                                  (let ((var condition))
                                    (guard-aux
                                     (handler-k
                                      (lambda ()
                                        (raise-continuable condition)))
                                     clause ...))))))))
                         (lambda ()
                           (call-with-values
                            (lambda () e1 e2 ...)
                            (lambda args
                              (guard-k
                               (lambda ()
                                 (apply values args)))))))))))))
   
   (define-syntax guard-aux
     (syntax-rules (else =>)
                   ((guard-aux reraise (else result1 result2 ...))
                    (begin result1 result2 ...))
                   ((guard-aux reraise (test => result))
                    (let ((temp test))
                      (if temp
                          (result temp)
                        reraise)))
                   ((guard-aux reraise (test => result)
                               clause1 clause2 ...)
                    (let ((temp test))
                      (if temp
                          (result temp)
                        (guard-aux reraise clause1 clause2 ...))))
                   ((guard-aux reraise (test))
                    (or test reraise))
                   ((guard-aux reraise (test) clause1 clause2 ...)
                    (let ((temp test))
                      (if temp
                          temp
                        (guard-aux reraise clause1 clause2 ...))))
                   ((guard-aux reraise (test result1 result2 ...))
                    (if test
                        (begin result1 result2 ...)
                      reraise))
                   ((guard-aux reraise
                               (test result1 result2 ...)
                               clause1 clause2 ...)
                    (if test
                        (begin result1 result2 ...)
                      (guard-aux reraise clause1 clause2 ...)))))
   
   
   (define-record-type <point>
     (%make-point depth in out parent)
     %point?
     (depth %point-depth)
     (in %point-in)
     (out %point-out)
     (parent %point-parent))
   
   (define root-point			; Shared among all state spaces
     (%make-point 0
                  (lambda () (error "winding in to root!"))
                  (lambda () (error "winding out of root!"))
                  #f))
   
   (define %dk
     (let ((dk root-point))
       (lambda o (if (pair? o) (set! dk (car o)) dk))))
   
   
   (define (dynamic-wind in body out)
     (in)
     (let ((here (%dk)))
       (%dk (%make-point (+ (%point-depth here) 1)
                         in
                         out
                         here))
       (let ((res (body)))
         (%dk here)
         (out)
         res)))
   
   (define (travel-to-point! here target)
     (cond
      ((eq? here target)
       'done)
      ((< (%point-depth here) (%point-depth target))
       (travel-to-point! here (%point-parent target))
       ((%point-in target)))
      (else
       ((%point-out here))
       (travel-to-point! (%point-parent here) target))))
   
   (define (continuation->procedure cont point)
     (lambda res
       (travel-to-point! (%dk) point)
       (%dk point)
       (call-with-values (lambda () (apply values res)) cont)))
   
   (define (call/cc proc)
     (%call/cc
      (lambda (cont)
        (proc (continuation->procedure cont (%dk))))))
   
   (define (%with-exception-handler handler thunk)
     (let ((old (current-exception-handler)))
       (dynamic-wind
        (lambda () (current-exception-handler-set! handler))
        thunk
        (lambda () (current-exception-handler-set! old)))))
   
   (define (with-exception-handler handler thunk)
     (letrec ((orig-handler (current-exception-handler))
              (self (lambda (exn)
                      (%with-exception-handler orig-handler
                                               (lambda ()
                                                 (cond
                                                  ((and (exception? exn)
                                                        (eq? 'continuable (exception-type exn)))
                                                   (handler (exception-irritants exn)))
                                                  (else
                                                   (handler exn)
                                                   (error "exception handler returned"))))))))
       (%with-exception-handler self thunk)))
   
   (%dk root-point)
   
   ))
