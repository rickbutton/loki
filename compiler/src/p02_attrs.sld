; attrs
; this is a poorly named pass
; this pass does a few different things, but mostly in the realm pf "semantic analysis"
; the primary job of this pass is to validate that the scheme syntax object provided
; is a valid scheme program, which mostly requires validating the shape of syntax

; secondly, this pass also does a preliminary amount of "scope tracking", annotating
; bound and free variables found in the syntax, as well as assigning all variables a
; unique identifier, that will be useful during code generation.

; ideally, after this pass the syntax records are "smooshed" into their literal equivalents
; so this is the last stop on the "compile time error" train. this means that any ambiguities
; around syntax vs definitions and scoping need to be determined by the end of this pass.
(define-library
    (p02_attrs)
    (import (scheme base))
    (import (srfi 69))
    (import (util))
    (import (shared))
    (export p02_attrs)
(begin

(define varid (make-named-id "$v"))
(define (empty-scope) '())
(define (empty-scopes) (list (empty-scope)))

(define (scopes->current scopes) (car scopes))
(define (scopes->parents scopes) (cdr scopes))

(define (var-name-equal? a b) (equal? (var->name a) (var->name b)))

(define (new-var name) (cons name (varid name)))
(define (add-var var scopes shadow)
    (let ((scope (scopes->current scopes))
          (parents (scopes->parents scopes)))
        (if (and (member var scope var-name-equal?) (not shadow))
            (raise (string-append 
                "attemped to define variable " 
                (symbol->string (var->name var)) 
                " that is already defined in the current scope")))
        (cons (cons var scope) parents)))
(define (add-var-name name scopes shadow) 
    (add-var (new-var name) scopes shadow))

(define (add-new-scope scopes)
    (cons (empty-scope) scopes))
        
(define (var->name var) (car var))
(define (var->mapped var) (cdr var))

(define (var-bound? name scopes)
    (if (null? scopes)
        #f
        (let ((found (member name (scopes->current scopes) (lambda (n c) (equal? n (var->name c))))))
            (if found (car found) #f))))
(define (var-free? name scopes)
    (if (null? scopes)
        #f
        (let ((parents (scopes->parents scopes)))
            (or (var-bound? name parents)
                (var-free? name parents)))))
(define (var-exists? name scopes) 
    (or (var-bound? name scopes) (var-free? name scopes)))

(define (syntax->attrs syntax)
    (cond
        ((cons-syntax? syntax) (cons-syntax->attrs syntax))
        ((atom-syntax? syntax) (atom-syntax->attrs syntax))
        (else (raise "unknown syntax type when getting attrs"))))
(define (syntax-set-attr syntax attr value)
    (let ((attrs (syntax->attrs syntax)))
        (hash-table-set! attrs attr value)))
(define (syntax-get-attr syntax attr)
    (let ((attrs (syntax->attrs syntax)))
        (hash-table-ref/default attrs attr #f)))

(define (cons-symbol-syntax? syntax symbol)
    (if (cons-syntax? syntax)
        (let ((car-syntax (cons-syntax->car syntax)))
            (and 
                (atom-syntax? car-syntax) 
                (equal? (atom-syntax->type car-syntax) 'symbol)
                (equal? (atom-syntax->value car-syntax) symbol)))
        #f))
(define (atom-type-syntax? syntax type)
    (and (atom-syntax? syntax) (equal? (atom-syntax->type syntax) type)))

(define (safe-car-syntax syntax) (if (cons-syntax? syntax) (cons-syntax->car syntax) #f))
(define (safe-cdr-syntax syntax) (if (cons-syntax? syntax) (cons-syntax->cdr syntax) #f))
(define (safe-cadr-syntax syntax) (safe-car-syntax (safe-cdr-syntax syntax)))
(define (safe-cddr-syntax syntax) (safe-cdr-syntax (safe-cdr-syntax syntax)))
(define (safe-caddr-syntax syntax) (safe-car-syntax (safe-cdr-syntax (safe-cdr-syntax syntax))))
(define (safe-cdddr-syntax syntax) (safe-cdr-syntax (safe-cdr-syntax (safe-cdr-syntax syntax))))
(define (safe-cadddr-syntax syntax) (safe-car-syntax (safe-cdr-syntax (safe-cdr-syntax (safe-cdr-syntax syntax)))))
(define (safe-cddddr-syntax syntax) (safe-cdr-syntax (safe-cdr-syntax (safe-cdr-syntax (safe-cdr-syntax syntax)))))

(define (null-syntax? syntax) (atom-type-syntax? syntax 'null))
(define (symbol-syntax? syntax) (atom-type-syntax? syntax 'symbol))

(define (set!-syntax? syntax) (cons-symbol-syntax? syntax 'set!))
(define (walk-syntax-validate-set! syntax scopes)
    (let ((identifier (safe-cadr-syntax syntax))
          (expression (safe-car-syntax (safe-cddr-syntax syntax)))
          (end (safe-cdr-syntax (safe-cddr-syntax syntax))))
        (if (and (symbol-syntax? identifier) (null-syntax? end))
            (if (var-exists? (atom-syntax->value identifier) scopes)
                (walk-syntax-validate-expression expression 'none scopes)
                (raise (string-append 
                    "attempted to set! undefined variable " 
                    (symbol->string (atom-syntax->value identifier)))))
            (raise "invalid set! syntax"))))

(define (quote-syntax? syntax) (cons-symbol-syntax? syntax 'quote))
(define (quasiquote-syntax? syntax) (cons-symbol-syntax? syntax 'quasiquote))
(define (unquote-syntax? syntax) (cons-symbol-syntax? syntax 'unquote))
(define (unquote-splicing-syntax? syntax) (cons-symbol-syntax? syntax 'unquote-splicing))
(define (quote->datum syntax) (safe-car-syntax (safe-cdr-syntax syntax)))
(define (unquote->expr syntax) (safe-car-syntax (safe-cdr-syntax syntax)))
(define (valid-quote-syntax? syntax) (and (quote-syntax? syntax) (null-syntax? (safe-cdr-syntax (safe-cdr-syntax syntax)))))
(define (valid-quasiquote-syntax? syntax) (and (quasiquote-syntax? syntax) (null-syntax? (safe-cdr-syntax (safe-cdr-syntax syntax)))))
(define (valid-unquote-syntax? syntax) (and (unquote-syntax? syntax) (null-syntax? (safe-cdr-syntax (safe-cdr-syntax syntax)))))
(define (valid-unquote-splicing-syntax? syntax) (and (unquote-splicing-syntax? syntax) (null-syntax? (safe-cdr-syntax (safe-cdr-syntax syntax)))))

(define (mark-symbol-syntax syntax scopes)
    (let ((name (atom-syntax->value syntax)))
        (if (var-bound? name scopes)
            (syntax-set-attr syntax 'binding 'bound)
            (if (var-free? name scopes)
                (syntax-set-attr syntax 'binding 'free)
                (raise "attempted to mark unbound variable")))
        (syntax-set-attr syntax 'unique-id (var->mapped (var-exists? name scopes)))))

(define (if-syntax? syntax) (cons-symbol-syntax? syntax 'if))
(define (walk-syntax-validate-if syntax scopes)
    (let ((test (safe-cadr-syntax syntax))
          (consequent (safe-caddr-syntax syntax))
          (alternate (safe-cadddr-syntax syntax))
          (end (safe-cddddr-syntax syntax)))
        (if (and (not (null-syntax? consequent)) (not (equal? #f consequent)))
            (walk-syntax-validate-expression consequent 'none scopes)
            (raise "invalid if syntax, not enough arguments"))
        (if (not (null-syntax? alternate))
            (walk-syntax-validate-expression alternate 'none scopes))
        (if (not (or (equal? #f end) (null-syntax? end)))
            (raise "invalid if syntax, too many arguments"))))

; TODO, handle define vs command/expression order
; for now, this will only validate that at least one expression exists
(define (walk-syntax-validate-body syntax scopes initial)
    (if (and (null-syntax? syntax) initial)
        ; body is null
        (raise "invalid empty body syntax, expected expression inside body"))
        ; body has at least one expression
        (if (cons-syntax? syntax)
            (walk-syntax-validate-expression (safe-car-syntax syntax) 'none scopes)
            (walk-syntax-validate-body (safe-cdr-syntax syntax) scopes #f)
            (raise "invalid body syntax")))

(define (lambda-syntax? syntax) (cons-symbol-syntax? syntax 'lambda))
(define (walk-syntax-validate-lambda-formals syntax names)
    (if (not (null-syntax? syntax))
        (if (cons-syntax? syntax)
            ; validate list of formals is all unique symbols
            (if (symbol-syntax? (safe-car-syntax syntax))
                (let ((name (atom-syntax->value (safe-car-syntax syntax))))
                    (if (member name names)
                        (raise (string-append
                            "invalid lambda formals syntax, attempted to declare variable "
                            (symbol->string name)
                            " more than once"))
                        (walk-syntax-validate-lambda-formals 
                            (safe-cdr-syntax syntax) 
                            (cons name names))))
                (raise "invalid lambda formal syntax, formals can only be symbols"))
            ; else, formals is invalid
            (raise "invalid lambda formals syntax"))
        ; formals are null, which is valid
        #t))

(define (lambda-formals-syntax->names* syntax names) 
    (if (null-syntax? syntax) 
        names 
        (lambda-formals-syntax->names* 
            (safe-cdr-syntax syntax) 
            (cons (atom-syntax->value (safe-car-syntax syntax)) names))))
(define (lambda-formals-syntax->names syntax)
    (lambda-formals-syntax->names* syntax '()))

(define (walk-syntax-validate-lambda syntax scopes) 
    (walk-syntax-validate-lambda-formals (safe-cadr-syntax syntax) '())
    (let* ((new-names (lambda-formals-syntax->names (safe-cadr-syntax syntax)))
           (new-scopes (fold-right (lambda (n s) (add-var-name n s #f)) (add-new-scope scopes) new-names)))
        (walk-syntax-validate-body (safe-cddr-syntax syntax) new-scopes #t)))

; quote-context => none, quote, quasiquote
(define (walk-syntax-validate-expression syntax quote-context scopes)
    (cond
        ((equal? quote-context 'none)
            ; we are not in a quote syntax of some kind
            ; so we need to validate this part of the AST
            (cond
                ; (quote <datum>)
                ((quote-syntax? syntax) 
                    (if (valid-quote-syntax? syntax) 
                        (walk-syntax-validate-expression (quote->datum syntax) 'quote scopes)
                        (raise "invalid quote syntax")))
                ; (quasiquote <datum>)
                ((quasiquote-syntax? syntax) 
                    (if (valid-quasiquote-syntax? syntax) 
                        (walk-syntax-validate-expression (quote->datum syntax) 'quasiquote scopes)
                        (raise "invalid quasiquote syntax")))
                ; (unquote <expression>)
                ((unquote-syntax? syntax)
                    (if (valid-unquote-syntax? syntax)
                        (raise "invalid attempt to unquote when not in quasiquote")
                        (raise "invalid unquote syntax")))
                ; (unquote-splicing <expression>)
                ((unquote-splicing-syntax? syntax)
                    (if (valid-unquote-splicing-syntax? syntax)
                        (raise "invalid attempt to unquote-splicing when not in quasiquote")
                        (raise "invalid unquote-splicing syntax")))
                ; <variable>
                ((symbol-syntax? syntax)
                    (if (var-exists? (atom-syntax->value syntax) scopes)
                        (mark-symbol-syntax syntax scopes)
                        (raise (string-append "attempted to reference undefined variable " (symbol->string (atom-syntax->value syntax))))))
                ; (set! <variable> <expression>)
                ((set!-syntax? syntax) (walk-syntax-validate-set! syntax scopes))
                ; (if test conse alte) / (if test conse)
                ((if-syntax? syntax) (walk-syntax-validate-if syntax scopes))
                ; (lambda (formals) <body>)
                ((lambda-syntax? syntax) (walk-syntax-validate-lambda syntax scopes))
                ; (op operand...)
                ((cons-syntax? syntax)
                    (walk-syntax-validate-expression (cons-syntax->car syntax) quote-context scopes)
                    (walk-syntax-validate-expression (cons-syntax->cdr syntax) quote-context scopes))))
        ((equal? quote-context 'quasiquote)
            ; we are inside a quasiquote syntax
            ; we only need to validate unquote and unquote splice
            (cond
                ((unquote-syntax? syntax)
                    (if (valid-unquote-syntax? syntax)
                        (walk-syntax-validate-expression (unquote->expr syntax) 'none scopes)
                        (raise "invalid unquote syntax")))
                ((unquote-splicing-syntax? syntax)
                    (if (valid-unquote-splicing-syntax? syntax)
                        (walk-syntax-validate-expression (unquote->expr syntax) 'none scopes)
                        (raise "invalid unquote-splicing syntax")))
                ((cons-syntax? syntax)
                    (walk-syntax-validate-expression (cons-syntax->car syntax) quote-context scopes)
                    (walk-syntax-validate-expression (cons-syntax->cdr syntax) quote-context scopes))))))

; TODO - need to mark "maybe" syntax primitives during variable marking step in order to 
; determine whether a syntax primitive is used as a primitive or as a variable/application

; TODO - variable definitions need to be handled, as well as the contexts in which they are allowed
; TODO - programs and definitions
; program
; - (begin ...)
; definition
; - (define (id formals) body)
; - (define-values formals body)
; - (define-record-type ...)

; TODO - implement the rest of the derived primitives
; macros are probably far away (since I will likely need to hand roll the parser to handle syntax objects)
; instead of using macros to provide the derivied primitives, implement them in the compiler
; (cond ...)
; (case ...)
; (let id bindings body)
; (let* bindings body)
; (letrec bindings body)
; (letrec* bindings body)
; (let-values mv-bindings body)
; (let*-values mv-bindings body)
; (do ...)
; (delay ...)
; (delay-force ...)
; (parameterize ...)
; (guard ...)
; (case-lambda ...)

; the ones I will probably implement for now
; (and ...)
; (or ...)
; (let bindings body)
; (begin ...)

; TODO - import declarations?
; TODO - libraries?
; TODO these primitives:
; (include string...)
; (include-ci string...)

(define (p02_attrs syntax)
    (walk-syntax-validate-expression syntax 'none (empty-scopes))
    syntax)
))