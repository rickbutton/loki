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
        type-printer-set!
        trace)
(begin

;; A numeric string that uniquely identifies this run in the universe
(define (ex:unique-token) "")

;; The letrec black hole and corresponding setter.
(define ex:undefined 'undefined)
(define ex:undefined-set! 'set!)

;; The "search prefixes" when loading a library from disk
(define ex:library-dirs (list "src" "compat/loki"))

; noop
(define (type-printer-set! type printer) #f)
(define (trace f) f)
))
