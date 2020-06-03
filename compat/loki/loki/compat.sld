;;;===============================================================================
;;;
;;; on-loki compatibility file:
;;;
;;;===============================================================================
(define-library (loki compat)
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
;(define (ex:unique-token) (number->string (current-seconds) 32))
; TODO - fix this
(define (ex:unique-token) "")

;; The letrec black hole and corresponding setter.
(define ex:undefined 'undefined)
(define ex:undefined-set! 'set!)

(define ex:library-dirs (list "src" "compat/loki"))

(define compat-env #f)
(define (compat-eval e) 
    (if (not compat-env)
        (set! compat-env 
            (environment '(scheme r5rs) '(loki compat) '(loki runtime) '(loki expander))))
    (eval e compat-env))

; noop
(define (type-printer-set! type printer) #f)
(define (trace f) f)
))
