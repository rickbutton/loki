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

(define (p02_attrs syntax)
    (define varid (make-named-id "$v"))
    (define (empty-scope) '())
    (define (empty-scopes) (list (empty-scope)))

    (define (scopes->current scopes) (car scopes))
    (define (scopes->parents scopes) (cdr scopes))

    (define (var-name-equal? a b) (equal? (var->name a) (var->name b)))

    (define (new-var name) (cons name (varid name)))

    ; mode 'strict, shadow, ignore
    (define (add-var syntax var scopes mode)
        (let ((scope (scopes->current scopes))
              (parents (scopes->parents scopes)))
            (if (and (member var scope var-name-equal?) (eq? mode 'strict))
                (raise-syntax-error syntax (string-append 
                    "attemped to define variable " 
                    (symbol->string (var->name var)) 
                    " that is already defined in the current scope")))
            (if (and (member var scope var-name-equal?) (eq? mode 'ignore))
                scopes
                (cons (cons var scope) parents))))
    (define (add-var-name syntax name scopes mode) 
        (add-var syntax (new-var name) scopes mode))

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

    (define (pair-symbol-syntax? syntax symbol)
        (let ((car-syntax (safe-car-syntax syntax)))
            (if car-syntax
                (equal? (syntax->value car-syntax) symbol)
                #f)))

    (define (prim-symbol-syntax? syntax symbol scopes)
        (if (pair-symbol-syntax? syntax symbol)
            (let* ((car-syntax (safe-car-syntax syntax))
                   (car-value (syntax->value car-syntax)))
                (and
                    (symbol? car-value)
                    (not (var-exists? car-value scopes))))
            #f))

    (define (type-syntax? syntax pred)
        (and (syntax? syntax) (pred (syntax->value syntax))))
    (define (null-syntax? syntax) (type-syntax? syntax null?))
    (define (symbol-syntax? syntax) (type-syntax? syntax symbol?))

    (define (set!-syntax? syntax scopes) (prim-symbol-syntax? syntax 'set! scopes))
    (define (walk-syntax-validate-set! syntax scopes)
        (let ((identifier (safe-cadr-syntax syntax))
              (expression (safe-car-syntax (safe-cddr-syntax syntax)))
              (end (safe-cdr-syntax (safe-cddr-syntax syntax))))
            (if (and (symbol-syntax? identifier) (null-syntax? end))
                (if (var-exists? (syntax->value identifier) scopes)
                    (begin
                        (walk-syntax-validate-expression expression scopes)
                        (mark-primitive-syntax syntax)
                        (mark-symbol-reference identifier scopes))
                    (raise-syntax-error syntax (string-append 
                        "attempted to set! undefined variable " 
                        (symbol->string (syntax->value identifier)))))
                (raise-syntax-error syntax "invalid set! syntax"))
            scopes))

    (define (quote-syntax? syntax scopes) (prim-symbol-syntax? syntax 'quote scopes))
    (define (quote->datum syntax) (safe-car-syntax (safe-cdr-syntax syntax)))
    (define (valid-quote-syntax? syntax scopes) (and (quote-syntax? syntax scopes) (null-syntax? (safe-cdr-syntax (safe-cdr-syntax syntax)))))

    (define (mark-symbol-declaration syntax scopes)
        (let ((name (syntax->value syntax)))
            (syntax-set-attr syntax 'unique-id (var->mapped (var-exists? name scopes)))
            (syntax-set-attr syntax 'binding 'bound)
            (syntax-set-attr syntax 'type 'declaration)))

    (define (mark-symbol-reference syntax scopes)
        (let ((name (syntax->value syntax)))
            (if (var-bound? name scopes)
                (syntax-set-attr syntax 'binding 'bound)
                (if (var-free? name scopes)
                    (syntax-set-attr syntax 'binding 'free)
                    (raise "attempted to mark unbound variable")))
            (syntax-set-attr syntax 'unique-id (var->mapped (var-exists? name scopes)))
            (syntax-set-attr syntax 'type 'reference)
            scopes))
    (define (mark-primitive-syntax syntax)
        (let ((car-syntax (safe-car-syntax syntax)))
            (syntax-set-attr car-syntax 'type 'primitive)))

    (define (symbol-intrinsic-syntax? syntax)
        (and (symbol-syntax? syntax)
             (intrinsic-name? (syntax->value syntax))))
    (define (mark-symbol-intrinsic-syntax syntax)
        (syntax-set-attr syntax 'type 'intrinsic))

    (define (if-syntax? syntax scopes) (prim-symbol-syntax? syntax 'if scopes))
    (define (walk-syntax-validate-if syntax scopes)
        (let ((test (safe-cadr-syntax syntax))
              (consequent (safe-caddr-syntax syntax))
              (alternate (safe-cadddr-syntax syntax))
              (end (safe-cddddr-syntax syntax)))
            (walk-syntax-validate-expression test scopes)
            (if (and (not (null-syntax? consequent)) (not (equal? #f consequent)))
                (walk-syntax-validate-expression consequent scopes)
                (raise-syntax-error syntax "invalid if syntax, not enough arguments"))
            (if (not (null-syntax? alternate))
                (walk-syntax-validate-expression alternate scopes))
            (if (not (or (equal? #f end) (null-syntax? end)))
                (raise-syntax-error syntax "invalid if syntax, too many arguments"))
            (mark-primitive-syntax syntax)
            scopes))

    ; TODO - support all define forms, instead of just:
    ; (define (id formals) body)
    ; (define id expr)
    (define (define-syntax? syntax scopes) (prim-symbol-syntax? syntax 'define scopes))
    (define (walk-syntax-validate-define syntax scopes)

        (let ((formals (safe-cadr-syntax syntax))
              (body (safe-cddr-syntax syntax)))
            (cond
                ((pair-syntax? formals) 
                    (let* ((id (safe-car-syntax formals))
                           (lambda-formals (safe-cdr-syntax formals))
                           (lambda-names (lambda-formals-syntax->names lambda-formals)))
                        (if (not (symbol-syntax? id))
                            (raise-syntax-error syntax "invalid define syntax, attempted to define non-symbol"))
                        (walk-syntax-validate-lambda-formals lambda-formals '())
                        (let* ((cont-scopes (add-var-name id (syntax->value id) scopes 'ignore))
                                                                                    ; TODO V - use the syntax for the specific formal
                                                                                    ; instead of the whole list of formals
                               (body-scopes (fold-right (lambda (n s) (add-var-name lambda-formals n s 'strict)) (add-new-scope cont-scopes) lambda-names)))
                            (walk-syntax-validate-body body body-scopes #t)
                            (mark-primitive-syntax syntax)
                            (mark-symbol-declaration id cont-scopes)
                            (mark-lambda-formals-declaration lambda-formals body-scopes)
                            cont-scopes)))
                ((symbol-syntax? formals)
                    (if (null-syntax? body) (raise-syntax-error syntax "invalid define syntax, expected expression in define"))
                    (if (null-syntax? (safe-cdr-syntax body))
                        (let ((cont-scopes (add-var-name formals (syntax->value formals) scopes 'strict)))
                            (walk-syntax-validate-expression (safe-car-syntax body) cont-scopes)
                            (mark-primitive-syntax syntax)
                            (mark-symbol-declaration formals cont-scopes)
                            cont-scopes)
                        (raise-syntax-error syntax "invalid define syntax, expected single expression in define")))
                (else (raise-syntax-error syntax "invalid define syntax, attempted to define non-symbol")))))

    ; TODO: for now, this will only validate that at least one expression exists
    (define (walk-syntax-validate-body syntax scopes initial)
        (if (null-syntax? syntax)
            (if initial (raise-syntax-error syntax "invalid empty body syntax, expected expression inside body"))
            (if (pair-syntax? syntax) ; body has at least one expression
                (let ((new-scopes (walk-syntax-validate-expression (safe-car-syntax syntax) scopes)))
                    (walk-syntax-validate-body (safe-cdr-syntax syntax) new-scopes #f))
                (raise-syntax-error syntax "invalid body syntax"))))

    (define (lambda-syntax? syntax scopes) (prim-symbol-syntax? syntax 'lambda scopes))
    ; TODO - handle other types of formals
    (define (walk-syntax-validate-lambda-formals syntax names)
        (if (not (null-syntax? syntax))
            (if (pair-syntax? syntax)
                ; validate list of formals is all unique symbols
                (if (symbol-syntax? (safe-car-syntax syntax))
                    (let ((name (syntax->value (safe-car-syntax syntax))))
                        (if (member name names)
                            (raise-syntax-error syntax (string-append
                                "invalid lambda formals syntax, attempted to declare variable "
                                (symbol->string name)
                                " more than once"))
                            (walk-syntax-validate-lambda-formals 
                                (safe-cdr-syntax syntax) 
                                (cons name names))))
                    (raise-syntax-error syntax "invalid lambda formal syntax, formals can only be symbols"))
                ; else, formals is invalid
                (raise-syntax-error syntax "invalid lambda formals syntax"))
            ; formals are null, which is valid
            #t))

    (define (begin-syntax? syntax scopes) (prim-symbol-syntax? syntax 'begin scopes))
    (define (walk-syntax-validate-begin syntax scopes)
        (let ((body-syntax (safe-cdr-syntax syntax)))
            (walk-syntax-validate-body body-syntax scopes #t)
            (mark-primitive-syntax syntax)
            scopes))

    (define (call/cc-syntax? syntax scopes) (prim-symbol-syntax? syntax 'call/cc scopes))
    (define (walk-syntax-validate-call/cc syntax scopes)
        (let ((args (safe-cdr-syntax syntax)))
            (walk-syntax-validate-body args scopes #t)
            (mark-primitive-syntax syntax)
            scopes))

    (define (lambda-formals-syntax->names* syntax names) 
        (if (null-syntax? syntax) 
            names 
            (lambda-formals-syntax->names* 
                (safe-cdr-syntax syntax) 
                (cons (syntax->value (safe-car-syntax syntax)) names))))
    (define (lambda-formals-syntax->names syntax)
        (lambda-formals-syntax->names* syntax '()))

    (define (mark-lambda-formals-declaration formals scopes)
        (if (not (null-syntax? formals))
            (let ((car-syntax (safe-car-syntax formals))
                (cdr-syntax (safe-cdr-syntax formals)))
                (if (symbol-syntax? car-syntax)
                    (mark-symbol-declaration car-syntax scopes)
                    (raise-syntax-error syntax "invalid syntax, expected symbol in lambda formals"))
                (if (not (null-syntax? cdr-syntax))
                    (mark-lambda-formals-declaration cdr-syntax scopes)))))

    (define (walk-syntax-validate-lambda syntax scopes) 
        (let ((formals-syntax (safe-cadr-syntax syntax)))
            (walk-syntax-validate-lambda-formals formals-syntax '())
            (let* ((new-names (lambda-formals-syntax->names formals-syntax))
                                                                        ; TODO V - use the syntax for the specific formal
                                                                        ; instead of the whole list of formals
                   (new-scopes (fold-right (lambda (n s) (add-var-name formals-syntax n s 'strict)) (add-new-scope scopes) new-names)))
                (walk-syntax-validate-body (safe-cddr-syntax syntax) new-scopes #t)
                (mark-primitive-syntax syntax)
                (mark-lambda-formals-declaration (safe-cadr-syntax syntax) new-scopes)
                scopes)))

    ; TODO, handle define vs command/expression order
    ; TODO, are all definitions expressions?
    (define (walk-syntax-validate-expression syntax scopes)
        (cond
            ; (quote <datum>)
            ((quote-syntax? syntax scopes) 
                (if (valid-quote-syntax? syntax scopes) 
                    (mark-primitive-syntax syntax)
                    (raise-syntax-error syntax "invalid quote syntax")))
            ; <intrinsic>
            ((symbol-intrinsic-syntax? syntax)
                (mark-symbol-intrinsic-syntax syntax))
            ; <variable>
            ((symbol-syntax? syntax)
                (if (var-exists? (syntax->value syntax) scopes)
                    (mark-symbol-reference syntax scopes)
                    (raise-syntax-error syntax (string-append "attempted to reference undefined variable " (symbol->string (syntax->value syntax))))))
            ; (set! <variable> <expression>)
            ((set!-syntax? syntax scopes) (walk-syntax-validate-set! syntax scopes))
            ; (if test conse alte) / (if test conse)
            ((if-syntax? syntax scopes) (walk-syntax-validate-if syntax scopes))
            ; (lambda (formals) <body>)
            ((lambda-syntax? syntax scopes) (walk-syntax-validate-lambda syntax scopes))
            ; (define (id formals scopes) <body>)
            ; (define id <body>)
            ((define-syntax? syntax scopes) (walk-syntax-validate-define syntax scopes))
            ; (begin 123)
            ((begin-syntax? syntax scopes) (walk-syntax-validate-begin syntax scopes))
            ; (call/cc (lambda (k) (k 123)))
            ((call/cc-syntax? syntax scopes) (walk-syntax-validate-call/cc syntax scopes))
            ; (op operand...)
            ((pair-syntax? syntax)
                (walk-syntax-validate-expression (safe-car-syntax syntax) scopes)
                (walk-syntax-validate-expression (safe-cdr-syntax syntax) scopes)
                scopes)))

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

    ; the ones I will probably implement for now (that aren't already implemented)
    ; (and ...)
    ; (or ...)
    ; (let bindings body)

    ; TODO - import declarations?
    ; TODO - libraries?
    ; TODO these primitives:
    ; (include string...)
    ; (include-ci string...)

    (walk-syntax-validate-expression syntax (empty-scopes))
    syntax)
))
