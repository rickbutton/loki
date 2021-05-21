(define-library (loki core exception)
  (import (loki core primitives)
          (loki core bool)
          (rename (loki core intrinsics) (%make-exception make-exception)
                  (%exception? exception?)
                  (%exception-type exception-type)
                  (%exception-message exception-message)
                  (%exception-irritants exception-irritants)))
  (export current-exception-handler current-exception-handler-set!
          raise raise-continuable error error-object?
          error-object-message error-object-irritants
          exception? exception-message exception-type exception-irritants
          read-error? file-error? make-read-error make-file-error)
  (begin
   
   
   (define (current-exception-handler) (%exception-handler))
   (define (current-exception-handler-set! handler)
     (%exception-handler-set! handler))
   
   (define (raise obj)
     ((%exception-handler) obj))
   
   (define (raise-continuable obj)          ; TODO - this sucks
     (raise (make-exception 'continuable "" (%cons obj '()))))
   
   (define (error message . irritants)
     (raise (make-exception 'error message irritants)))
   
   (define (error-object? obj)
     (exception? obj))
   
   (define (error-object-message obj)
     (exception-message obj))
   
   (define (error-object-irritants obj)
     (exception-irritants obj))
   
   (define (read-error? obj)
     (and (exception? obj)
          (eq? (exception-type obj) 'read-error)))
   (define (file-error? obj)
     (and (exception? obj)
          (eq? (exception-type obj) 'file-error)))
   
   (define (make-read-error message irritants)
     (make-exception 'read-error message irritants))
   
   (define (make-file-error message irritants)
     (make-exception 'file-error message irritants))
   
   (%exception-handler-set! (lambda (obj) (%abort obj)))
   
   ))
