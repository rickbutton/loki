;;;===============================================================================
;;;
;;; chibi compatibility file:
;;;
;;;===============================================================================
(define-library (loki compat)
(import (chibi time))
(import (rename (chibi ast) (type-printer-set! chibi-type-printer-set!)))
(import (scheme base))
(export ex:unique-token
        ex:library-dirs
        type-printer-set!)
(begin

;; A numeric string that uniquely identifies this run in the universe
(define (ex:unique-token) (number->string (current-seconds) 32))

;; The "search prefixes" when loading a library from disk
(define ex:library-dirs (list "src" "compat/loki"))

(define (type-printer-set! type printer)
    (chibi-type-printer-set! type 
        (lambda (x writer out) (printer x out))))
))
