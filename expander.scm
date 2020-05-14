(import (scheme r5rs))
(import (scheme base))
(import (scheme write))
(import (scheme load))
(import (srfi 159))
(import (chibi show pretty))
(import (util))

(load "expander/compat-chibi.scm")
(load "expander/runtime.scm")
(load "expander/expander.scm")
;(ex:expand-file "expander/standard-libraries.scm" "expander/standard-libraries.exp")
(load "expander/standard-libraries.exp")

(define (expand forms)
  (ex:expand-sequence forms 'library))
  ;(ex:run-r6rs-sequence forms))

(define form '(
(library (main)
  (export a)
  (import 
    (only (rnrs base) define-syntax syntax-rules)
    (only (rnrs (6)) set! quote + define display newline)
    (test))

  (define-syntax nil!
    (syntax-rules ()
      ((_ x)
       (set! x '()))))

  (define a 1)
  (define b 1)
  (display (+ a (bar)))
  (newline)

  (nil! b)
)
))

(display (show #f (pretty (expand form))))
