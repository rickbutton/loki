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
        ex:guid-prefix
        ex:free-prefix
        library-name->filename
        compat-eval
        type-printer-set!
        trace)
(begin

;; A numeric string that uniquely identifies this run in the universe
(define (ex:unique-token) (number->string (current-seconds) 32))

;; The letrec black hole and corresponding setter.
(define ex:undefined 'undefined)
(define ex:undefined-set! 'set!)

;; Single-character symbol prefixes.
;; No builtins may start with one of these.
;; If they do, select different values here.
(define ex:guid-prefix "&")
(define ex:free-prefix "~")

(define (library-name-part->string p)
    (if (symbol? p) (symbol->string p)
                    (number->string p)))

(define library-dirs (list "src" "compat/loki"))
(define (library-name->filename name)
    (find file-exists?
        (map (lambda (dir)
            (string-append 
                (string-join (cons dir (reverse (map library-name-part->string name))) "/") ".sld"))
            library-dirs)))

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
