;;;===============================================================================
;;;
;;; on-loki compatibility file:
;;;
;;;===============================================================================
(define-library (loki compat)
(import (scheme base))
(export ex:unique-token
        ex:library-dirs
        type-printer-set!)
(begin

;; A numeric string that uniquely identifies this run in the universe
(define (ex:unique-token) "")

;; The "search prefixes" when loading a library from disk
(define ex:library-dirs (list "src" "compat/loki"))

; noop
(define (type-printer-set! type printer) #f)
))
