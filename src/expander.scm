(import (scheme base))
(import (scheme r5rs))
(import (scheme process-context))
(import (scheme file))
(import (loki runtime))
(import (loki expander))
(import (scheme eval))
(import (loki expander))
(import (loki util))
(import (loki shared))
(import (srfi 159))
(import (chibi show pretty))
(import (loki reader))

(define (handle-loki-message e)
  (let ((type (loki-message->type e))
        (location (loki-message->location e))
        (message (loki-message->message e)))
    (display type)
    (display " at ")
    (display (source-location->string location))
    (display ": ")
    (display message)
    (display "\n")
    (if (eq? type 'error) (exit 1))))

(define (handle-unexpected-error e)
  (display "unexpected error: ")
  (display e)
  (raise e)
  (exit 1))

(define (handle-error e)
        (if (loki-message? e)
            (handle-loki-message e)
            (handle-unexpected-error e)))

(define (run)
  (call/cc (lambda (k)
    (with-exception-handler
      handle-error
      (lambda ()
        (ex:expand-file "src/loki/r7rs.scm" "src/loki/r7rs.exp"))))))

(run)

(define form 
'(define-library (my)
    (export a)
    (import (scheme base))
    (import (scheme write))
    (import (test))
    (import (loki expander))
    (begin
      (define-syntax mac
        (syntax-rules ()
          ((_ arg) (display arg))))
      (define a 3)
      (mac (+ a (bar))))))
(define str (show #f (pretty form)))

; TODO - interaction-environment sort of simulates real
; expected runtime environment
; current can't resolve ex:undefined, which is inside expander
; will need to resolve these to expanded code instead of direct emit
; (including register-macro etc)
;(ex:expand-sequence (list form) 'library)
;(display (show #f (pretty (ex:lookup-library '(loki expander)))))
