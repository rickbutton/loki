(define-library (scheme lazy)
    (import (loki core primitives))
    (import (loki core let))
    (import (loki core control))
    (import (loki core bool))
    (import (loki core list))
    (import (loki core intrinsics))
    (import (for (loki core syntax-rules) expand))
    (export delay
            force
            promise?
            delay-force
            make-promise)
    (begin 
      
      (define *promise-tag* (list 'promise))
      (define (promise done? proc)
        (cons (cons done? proc) *promise-tag*))
      (define (make-promise x)
         (if (promise? x) x (delay x)))

      (define (promise? x)
        (and (pair? x) (eq? *promise-tag* (cdr x))))
      (define (promise-done? x) (car (car x)))
      (define (promise-value x) (cdr (car x)))

      (define (promise-update! new old)
        (set-car! (car old) (promise-done? new))
        (set-cdr! (car old) (promise-value new))
        (set-car! new (car old)))

      (define (force promise)
        (if (promise-done? promise)
            (promise-value promise)
            (let ((promise* ((promise-value promise))))
              (if (not (promise-done? promise))
                (promise-update! promise* promise))
              (force promise))))
      
      (define-syntax delay
        (syntax-rules ()
          ((delay expression)
           (delay-force (promise #t expression)))))
      
      (define-syntax delay-force
        (syntax-rules ()
          ((delay-force expression)
           (promise #f (lambda () expression)))))
))

