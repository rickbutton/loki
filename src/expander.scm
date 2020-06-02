(import (scheme base))
(import (scheme write))
(import (loki expander))
(import (loki runtime))
(import (loki shared))

(import (srfi 159))
(import (chibi show pretty))

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

(with-loki-error-handler (lambda ()
  (ex:expand-file "src/loki/r7rs.scm")
  (ex:expand-datum-sequence (list form))))


; TODO - provide clean interface

; TODO - interaction-environment sort of simulates real
; expected runtime environment
; current can't resolve ex:undefined, which is inside expander
; will need to resolve these to expanded code instead of direct emit
; (including register-macro etc)
(display (show #f (pretty (ex:lookup-library '(loki expander)))))
