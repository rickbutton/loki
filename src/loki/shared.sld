(define-library 
    (loki shared)
    (import (scheme base))
    (import (scheme write))
    (import (srfi 69))
    (import (loki util))
    (import (loki compat))
    (export 
        make-comment
        comment?
        comment->text

        make-source

        annotate
        annotation?
        annotation-type?
        annotation-expression
        unwrap-annotation

        identifier?
        make-identifier
        id-source
        id-name
        id-colors
        id-transformer-envs
        id-displacement
        id-maybe-library

        integer-syntax?

        raise-loki-error
        raise-loki-warning
        make-loki-message
        loki-message?
        loki-message->type
        loki-message->location
        loki-message->message)
(begin

#;(define (source-location->string l)
    (string-append 
        (source-location->path l)
        " ["
        (number->string (source-location->line l))
        ":"
        (number->string (source-location->column l))
        "]"))

(define-record-type <comment>
    (make-comment text)
    comment?
    (text comment->text))
(type-printer-set! <comment> 
    (lambda (x out) 
        (display (string-append "(;" (comment->text x) ";)") out)))

(define (make-source filename line column)
    (vector filename line column))

(define-record-type <annotation>
    (make-annotation-record type expression source context)
    annotation?
    (type annotation-type)
    (expression annotation-expression)
    (source annotation-source)
    (context annotation-context))

(define (annotation-type? type a)
    (and (annotation? a)
         (eq? type (annotation-type a))))

(define (make-annotation type expr src)
    (make-annotation-record type expr src #f))

(define (annotate type source datum)
  (assert (vector? source))
  (make-annotation type datum
                   source))

(define-record-type <identifier-context>
    (make-identifier-context colors transformer-envs displacement maybe-library)
    identifier-context?
    (colors identifier-context-colors)
    (transformer-envs identifier-context-transformer-envs)
    (displacement identifier-context-displacement)
    (maybe-library identifier-context-maybe-library))


(type-printer-set! <annotation> 
    (lambda (x out) 
        (define expr-port (open-output-string))
        (write (annotation-expression x) expr-port)

        (let ((source (annotation-source x))
              (expr-str (get-output-string expr-port)))
            (display (string-append
                "#<syntax:"
                (vector-ref source 0) ":"
                (number->string (vector-ref source 1)) ":"
                (number->string (vector-ref source 2)) " "
                expr-str ">" ) out))))

;;==========================================================================
;;
;; Identifiers:
;;
;;==========================================================================
;; <name>             ::= <symbol>
;; <colors>           ::= (<color> ...)
;; <transformer-envs> ::= (<env> ...)
;; <displacement>     ::= <integer>
;; <maybe-library>    ::= (<symbol> ...) | #f
;;
;; where
;;   <name>             : The symbolic name of the identifier in the source.
;;   <colors>           : Each time an introduced identifier is renamed, a fresh
;;                        color gets prepended to its <colors>.
;;   <transformer-envs> : List of reflected transformer environments.
;;                        The environment (env-reify (car <transformer-envs>)) was the
;;                        usage environment valid during expansion of any (syntax id)
;;                        expression whose evaluation introduced this identifier, while
;;                        (cdr <transformer-envs>) are in turn the reflected
;;                        <transformer-envs> of the original id.
;;   <displacement>     : Integer that keeps track of shifts in phases
;;                        between transformer and usage sites of identifier.
;;   <maybe-library>    : Library name if identifier was introduced by evaluation of
;;                        a (syntax ...) expression, otherwise #f.
;;                        The empty name '() is used for toplevel.
(define (make-identifier name colors transformer-envs displacement maybe-library src)
    (make-annotation-record 'identifier name src
        (make-identifier-context colors transformer-envs displacement maybe-library)))
(define (identifier? i)
    (and (annotation? i) (eq? 'identifier (annotation-type i))))
(define (id-source i)
    (assert (eq? 'identifier (annotation-type i)))
    (annotation-source i))
(define (id-name i)
    (assert (eq? 'identifier (annotation-type i)))
    (annotation-expression i))
(define (id-colors i) (identifier-context-colors (annotation-context i)))
(define (id-transformer-envs i) (identifier-context-transformer-envs (annotation-context i)))
(define (id-displacement i) (identifier-context-displacement (annotation-context i)))
(define (id-maybe-library i) (identifier-context-maybe-library (annotation-context i)))

(define (integer-syntax? x) (and (annotation-type? 'value x) (integer? (annotation-expression x))))

(define (unwrap-annotation a)
    (if (annotation? a) (annotation-expression a) a))

(define-record-type <loki-message>
    (make-loki-message type location message)
    loki-message?
    (type loki-message->type)
    (location loki-message->location)
    (message loki-message->message))

(define (raise-loki-error location message)
        (raise (make-loki-message 'error location message)))

(define (raise-loki-warning location message)
        (raise (make-loki-message 'warning location message)))
))
