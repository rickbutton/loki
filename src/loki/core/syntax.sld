(define-library (loki core syntax)
  (import (scheme base))
  (cond-expand
   (gauche (import (scheme write)))
   (loki (import (loki core writer))))
  (export  <annotation>
          annotation?
          annotation-type?
          annotation-expression
          annotation-source
          unwrap-annotation
          datum->annotation
          
          <identifier-context>
          identifier?
          make-identifier
          id-source
          id-name
          id-colors
          id-transformer-envs
          id-displacement
          id-maybe-module
          
          <source>
          make-source
          source?
          source-file
          source-line
          source-column
          source->string
          
          sexp-map
          integer-syntax?
          syntax->datum
          datum->syntax
          syntax->source
          syntax-violation
          invalid-form)
  (begin
   
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
   
   (define (datum->annotation type source datum)
     (make-annotation type datum source))
   
   (define-record-type <identifier-context>
     (make-identifier-context colors transformer-envs displacement maybe-library)
     identifier-context?
     (colors identifier-context-colors)
     (transformer-envs identifier-context-transformer-envs)
     (displacement identifier-context-displacement)
     (maybe-library identifier-context-maybe-library))
   
   (define-record-type <source>
     (make-source file line column)
     source?
     (file source-file)
     (line source-line)
     (column source-column))
   (define (source->string s)
     (string-append
      (source-file s)
      " ["
      (number->string (source-line s))
      ":"
      (number->string (source-column s))
      "]"))
   
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
   (define (id-source i) (annotation-source i))
   (define (id-name i) (annotation-expression i))
   (define (id-colors i)
     (unless (annotation-context i) (error "id-colors: not an identifier" i))
     (identifier-context-colors (annotation-context i)))
   (define (id-transformer-envs i) (identifier-context-transformer-envs (annotation-context i)))
   (define (id-displacement i) (identifier-context-displacement (annotation-context i)))
   (define (id-maybe-module i) (identifier-context-maybe-library (annotation-context i)))
   
   (define (unwrap-annotation a)
     (if (annotation? a) (annotation-expression a) a))
   (define (integer-syntax? x) (and (annotation-type? 'value x) (integer? (annotation-expression x))))
   
   (define (sexp-map f s)
     (cond ((null? s) '())
           ((pair? s)
            (cons (sexp-map f (car s))
                  (sexp-map f (cdr s))))
           ((vector? s)
            (apply vector (sexp-map f (vector->list s))))
           (else (f s))))
   
   (define (syntax->datum exp)
     (sexp-map (lambda (leaf)
                 (cond ((identifier? leaf) (id-name leaf))
                       ((annotation? leaf) (annotation-expression leaf))
                       ((symbol? leaf)
                        (error "syntax->datum: A symbol is not a valid syntax object" leaf))
                       (else leaf)))
               exp))
   
   (define (datum->syntax tid datum)
     (unless (identifier? tid)
       (error "datum->syntax: Invalid form" tid))
     (sexp-map (lambda (leaf)
                 (cond ((symbol? leaf)
                        (make-identifier leaf
                                         (id-colors tid)
                                         (id-transformer-envs tid)
                                         (id-displacement tid)
                                         (id-maybe-module tid)
                                         (id-source tid)))
                       (else leaf)))
               datum))
   
   (define (syntax->source s)
     (cond
      ((annotation? s) (annotation-source s))
      ((and (pair? s) (annotation? (car s)))
       (annotation-source (car s)))
      ((pair? s) (syntax->source (cdr s)))
      (else #f)))
   
   (define (syntax-violation who message form)
     (let ((form-source (syntax->source form))
           (out (open-output-string)))
       (display "[" out)
       (display who out)
       (display "] syntax error at " out)
       (if form-source
           (display (source->string form-source) out)
         (display "[unknown]" out))
       (display ": " out)
       (display message out)
       (display "\n  " out)
       (display form out)
       (error (get-output-string out))))
   
   (define (invalid-form exp)
     (syntax-violation #f "Invalid form" exp))
   
   ))
