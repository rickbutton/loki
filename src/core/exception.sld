(define-library (core exception)
(import (core primitives)
        (core bool)
        (rename (core intrinsics) (%raise raise)
                                  (%make-exception make-exception)
                                  (%exception? exception?)
                                  (%exception-type exception-type)
                                  (%exception-message exception-message)
                                  (%exception-irritants exception-irritants)))
(export raise raise-continuable error error-object?
        error-object-message error-object-irritants
        read-error? file-error?)
(begin

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

))
