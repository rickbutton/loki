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
(import (loki util))
(export ex:unique-token
        ex:undefined
        ex:undefined-set!
        ex:library-dirs
        type-printer-set!
        trace)
(begin

;; A numeric string that uniquely identifies this run in the universe
(define (ex:unique-token) (number->string (current-seconds) 32))

;; The letrec black hole and corresponding setter.
(define ex:undefined '(if #f #f))
(define ex:undefined-set! 'set!)

;; The "search prefixes" when loading a library from disk
(define ex:library-dirs (list "src" "compat/loki"))

(define (type-printer-set! type printer)
    (chibi-type-printer-set! type 
        (lambda (x writer out) (printer x out))))
))
