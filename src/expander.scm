(import (scheme base))
(import (scheme eval))
(import (scheme write))
(import (loki expander))
(import (loki runtime))
(import (loki shared))
(import (loki util))

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
      (define a 3)
      (display (+ a (bar))))))

(with-loki-error-handler (lambda ()
  (ex:expand-file "src/loki/r7rs.scm")
  (ex:expand-datum-sequence (list form))
  (let ((name (car (ex:expand-datum-sequence (list '(import (my)))))))
    (debug (eval name))
    (ex:import-main-library (eval name)))))

;(display (show #f (pretty (ex:lookup-library '(my)))))
