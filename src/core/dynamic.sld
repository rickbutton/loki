(define-library (core dynamic)
(import (core primitives))
(import (core let))
(import (core derived))
(import (core intrinsics))
(import (core bool))
(import (core number))
(import (core list))
(import (core exception))
(import (core values))
(import (core records))
(import (for (core syntax-rules) expand))
(export make-parameter parameterize guard dynamic-wind
        call-with-current-continuation
        (rename (call-with-current-continuation call/cc))
        with-exception-handler)
(begin

(define <param-set!> (cons 'param-set! '()))
(define <param-convert> (cons 'param-convert '()))

(define (make-parameter init . o)
  (let* ((converter
          (if (pair? o) (car o) (lambda (x) x)))
         (value (converter init)))
    (lambda args
      (cond
       ((null? args)
        value)
       ((eq? (car args) <param-set!>)
        (set! value (cadr args)))
       ((eq? (car args) <param-convert>)
        converter)
       (else
        (error "bad parameter syntax"))))))

(define-syntax parameterize
  (syntax-rules ()
    ((parameterize ("step")
                   ((param value p old new) ...)
                   ()
                   body)
     (let ((p param) ...)
       (let ((old (p)) ...
             (new ((p <param-convert>) value)) ...)
         (dynamic-wind
          (lambda () (p <param-set!> new) ...)
          (lambda () . body)
          (lambda () (p <param-set!> old) ...)))))
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
    (cont (values res))))

(define (call-with-current-continuation proc)
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
