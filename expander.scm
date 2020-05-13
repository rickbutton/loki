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

(define (expand forms)
  (ex:expand-sequence forms 'program))

(define form '(

(define-syntax nil!
  (syntax-rules ()
    ((_ x)
     (set! x '()))))

(define a 1)
(+ a a)

(nil! a)

))
(display (show #f (pretty (expand form))))
