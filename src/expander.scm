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

(ex:expand-file "src/loki/r7rs.scm" "src/loki/r7rs.exp")

(define (expand forms)
  (ex:expand-sequence forms 'library))

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
(set! str (string-append str str))

(expand (list (read (open-input-string str))))
;(define syn (p01_tokens2syntax toks))
;(display (show #f (pretty syn)))
;(expand syn)

; TODO - interaction-environment sort of simulates real
; expected runtime environment
; current can't resolve ex:undefined, which is inside expander
; will need to resolve these to expanded code instead of direct emit
; (including register-macro etc)
;(expand form)
(display (show #f (pretty (ex:lookup-library '(loki expander)))))
