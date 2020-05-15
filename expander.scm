(import (scheme r5rs))
(import (expander runtime))
;(ex:expand-file "expander/standard-libraries.scm" "expander/standard-libraries.exp")
;(load "compiler/src/expander/standard-libraries.exp")

(import (scheme write))
(import (scheme load))
(import (expander expander))
(import (util))

(define (expand forms)
  (ex:expand-sequence forms 'library))

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

(display (pretty-print (expand form)))
