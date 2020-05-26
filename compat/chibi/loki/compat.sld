;;;===============================================================================
;;;
;;; chibi compatibility file:
;;;
;;;===============================================================================
(define-library (loki compat)
(import (scheme base))
(import (scheme r5rs))
(import (scheme process-context))
(import (scheme write))
(import (scheme file))
(import (scheme eval))
(import (rename (chibi ast) (type-printer-set! chibi-type-printer-set!)))
(export ex:unique-token
        ex:undefined
        ex:undefined-set!
        ex:guid-prefix
        ex:free-prefix
        assertion-violation
        memp for-all
        file-exists?
        delete-file
        string-join
        library-name->filename
        compat-eval
        type-printer-set!)
(begin

;; A numeric string that uniquely identifies this run in the universe
;(define (ex:unique-token) (number->string (current-seconds) 32))
; TODO - fix this
(define (ex:unique-token) "")

;; The letrec black hole and corresponding setter.
(define ex:undefined 'undefined)
(define ex:undefined-set! 'set!)

;; Single-character symbol prefixes.
;; No builtins may start with one of these.
;; If they do, select different values here.
(define ex:guid-prefix "&")
(define ex:free-prefix "~")

;; Just give this damn thing a binding

(define assertion-violation 
  (lambda args 
    (display 'assertion-violation)
    (newline)
    (display args)
    (newline)
    (error)))

;; These are only partial implementations for specific use cases needed.
;; Full implementations should be provided by host implementation.

(define (memp proc ls)
  (cond ((null? ls) #f)
        ((pair? ls) (if (proc (car ls))
                        ls
                        (memp proc (cdr ls))))
        (else (assertion-violation 'memp "Invalid argument" ls))))

(define (for-all proc l . ls)
  (or (null? l)
      (and (apply proc (car l) (map car ls))
           (apply for-all proc (cdr l) (map cdr ls)))))

(define (find pred list)
    (if (null? list) #f
        (if (pred (car list))
            (car list)
            (find pred (cdr list)))))
(define (fold-right f init seq) 
    (if (null? seq) 
        init 
        (f (car seq) 
            (fold-right f init (cdr seq))))) 
(define (string-join strings delimiter)
  (if (null? strings)
      ""
      (fold-right (lambda (s so-far) (string-append so-far delimiter s))
            (car strings)
            (cdr strings))))

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
