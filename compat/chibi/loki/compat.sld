;;;===============================================================================
;;;
;;; chibi compatibility file:
;;;
;;;===============================================================================
(define-library (loki compat)
(import (chibi time))
(import (chibi trace))
(import (rename (chibi ast) (type-printer-set! chibi-type-printer-set!)))
(import (scheme base))
(import (scheme r5rs))
(import (scheme process-context))
(import (scheme write))
(import (scheme file))
(import (scheme eval))
(import (loki util))
(export ex:unique-token
        ex:undefined
        ex:undefined-set!
        ex:library-dirs
        compat-eval
        type-printer-set!
        trace)
(begin

;; A numeric string that uniquely identifies this run in the universe
(define (ex:unique-token) (number->string (current-seconds) 32))

;; The letrec black hole and corresponding setter.
(define ex:undefined '(if #f #f))
(define ex:undefined-set! 'set!)

(define ex:library-dirs (list "src" "compat/loki"))


(define compat-env #f)
(define (compat-eval e) 
    (if (not compat-env)
        (set! compat-env 
            (environment '(scheme r5rs) '(loki compat) '(loki runtime) '(loki expander))))
    (eval e compat-env))

(define (type-printer-set! type printer)
    (chibi-type-printer-set! type 
        (lambda (x writer out) (printer x out))))
))
