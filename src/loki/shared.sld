(define-library 
    (loki shared)
    (import (scheme base))
    (import (scheme write))
    (import (scheme process-context))
    (import (srfi 69))
    (import (loki util))
    (import (loki compat))
    (export 
        make-comment
        comment?
        comment->text

        <source>
        make-source
        source?
        source-filename
        source-line
        source-column
        source->string

        <annotation>
        annotate
        annotation?
        annotation-type?
        annotation-expression
        annotation-source
        unwrap-annotation
        syntax->closest-source

        <identifier-context>
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
        loki-message->source
        loki-message->message
        with-loki-error-handler)
(begin

(define-record-type <comment>
    (make-comment text)
    comment?
    (text comment->text))
(type-printer-set! <comment> 
    (lambda (x out) 
        (display (string-append "(;" (comment->text x) ";)") out)))

(define-record-type <source>
    (make-source filename line column)
    source?
    (filename source-filename)
    (line source-line)
    (column source-column))
(define (source->string s)
    (string-append 
        (source-filename s)
        " ["
        (number->string (source-line s))
        ":"
        (number->string (source-column s))
        "]"))

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
  (assert (source? source))
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
                "#<syntax"
                (if source
                    (string-append
                        ":"
                        (source-filename source) ":"
                        (number->string (source-line source)) ":"
                        (number->string (source-column source)) " ")
                    " ")
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

(define (syntax->closest-source s)
    (cond
        ((annotation? s) (annotation-source s))
        ((and (pair? s) (annotation? (car s)))
            (annotation-source (car s)))
        ((pair? s) (syntax->closest-source (cdr s)))
        (else #f)))

(define-record-type <loki-message>
    (make-loki-message type source message)
    loki-message?
    (type loki-message->type)
    (source loki-message->source)
    (message loki-message->message))

(define (raise-loki-error source message)
        (raise (make-loki-message 'error source message)))

(define (raise-loki-warning source message)
        (raise (make-loki-message 'warning source message)))

(define (handle-loki-message e)
  (let ((type (loki-message->type e)) (source (loki-message->source e))
        (message (loki-message->message e)))
    (display type)
    (display " at ")
    (if (source? source)
        (display (source->string source))
        (display "<unknown>"))
    (display ": ")
    (display message)
    (display "\n")
    (if (eq? type 'error) (raise "error") (exit 1))))

(define (handle-unexpected-error e)
  (display "unexpected error: ")
  (display e)
  (error "unexpected error"))

(define (handle-error e)
        (if (loki-message? e)
            (handle-loki-message e)
            (handle-unexpected-error e)))


(define (with-loki-error-handler proc)
    (with-exception-handler handle-error proc))

))
