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
    (import (chibi match))
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
        (syntax-set-attr syntax 'type 'primitive))
    (define (mark-symbol-intrinsic-syntax syntax)
        (syntax-set-attr syntax 'type 'intrinsic))

    (define (prim-symbol-syntax? syntax symbol scopes)
        (let ((value (syntax->value syntax))) 
            (and 
                (equal? value symbol)
                (not (var-exists? value scopes)))))

    (define (type-syntax? syntax pred)
        (and (syntax? syntax) (pred (syntax->value syntax))))
    (define (null-syntax? syntax) (type-syntax? syntax null?))
    (define (symbol-syntax? syntax) (type-syntax? syntax symbol?))
    (define (symbol-intrinsic-syntax? syntax)
        (and (symbol-syntax? syntax)
             (intrinsic-name? (syntax->value syntax))))

    (define (syntax->match-scheme syntax)
        (if (pair-syntax? syntax)
            (let ((pair (syntax->value syntax)))
                (cons (syntax->match-scheme (car pair))
                      (syntax->match-scheme (cdr pair))))
            syntax))

    (define (walk-body body scopes)
        (if (null? body) 
            scopes
            (walk-body (cdr body) (walk-expression (car body) scopes))))

    (define (walk-expression syntax scopes)
         (define (quote-syntax? syntax) (prim-symbol-syntax? syntax 'quote scopes))
         (define (set!-syntax? syntax) (prim-symbol-syntax? syntax 'set! scopes))
         (define (if-syntax? syntax) (prim-symbol-syntax? syntax 'if scopes))
         (define (define-syntax? syntax) (prim-symbol-syntax? syntax 'define scopes))
         (define (lambda-syntax? syntax) (prim-symbol-syntax? syntax 'lambda scopes))
         (define (begin-syntax? syntax) (prim-symbol-syntax? syntax 'begin scopes))
         (define (call/cc-syntax? syntax) (prim-symbol-syntax? syntax 'call/cc scopes))

        (let ((value (syntax->match-scheme syntax)))
        (match value
            (((? quote-syntax? prim) expr)
                (mark-primitive-syntax prim)
                scopes)
            (((? quote-syntax?) args ...) (raise-syntax-error syntax "invalid quote syntax"))

            ((? symbol-intrinsic-syntax?) 
                (mark-symbol-intrinsic-syntax syntax)
                scopes)

            ((? symbol-syntax?)
                (unless (var-exists? (syntax->value syntax) scopes)
                    (raise-syntax-error syntax (string-append 
                        "attempted to reference undefined variable " 
                        (symbol->string (syntax->value syntax)))))
                (mark-symbol-reference syntax scopes)
                scopes)

            (((? set!-syntax? prim) (? symbol-syntax? id) expr)
                (unless (var-exists? (syntax->value id) scopes)
                    (raise-syntax-error syntax (string-append 
                        "attempted to set! undefined variable " 
                        (symbol->string (syntax->value id)))))
                (walk-expression expr scopes)
                (mark-primitive-syntax prim)
                (mark-symbol-reference id scopes)
                scopes)
            (((? set!-syntax?) args ...) (raise-syntax-error syntax "invalid set! syntax"))

            (((? if-syntax? prim) c t f)
                (walk-expression c scopes)
                (walk-expression t scopes)
                (walk-expression f scopes)
                (mark-primitive-syntax prim)
                scopes)
            (((? if-syntax? prim) c t)
                (walk-expression c scopes)
                (walk-expression t scopes)
                (mark-primitive-syntax prim)
                scopes)
            (((? if-syntax?) args ...) (raise-syntax-error syntax "invalid if syntax"))

            (((? lambda-syntax? prim) ((? symbol-syntax? formals) ...) expr exprs ...)
                (let* ((names (map syntax->value formals))
                       (new-scopes (fold-right (lambda (n s) (add-var-name formals n s 'strict)) (add-new-scope scopes) names)))
                    (walk-body (cons expr exprs) new-scopes)
                    (mark-primitive-syntax prim)
                    (map (lambda (f) (mark-symbol-declaration f new-scopes)) formals)))
            (((? lambda-syntax?) args ...) (raise-syntax-error syntax "invalid lambda syntax"))

            ; TODO (define (id formals) body...)
            (((? define-syntax? prim) (? symbol-syntax? id) expr)
                (let ((cont-scopes (add-var-name id (syntax->value id) scopes 'strict)))
                    (walk-expression expr cont-scopes)
                    (mark-primitive-syntax prim)
                    (mark-symbol-declaration id cont-scopes)
                    cont-scopes))
            (((? define-syntax?) args ...) (raise-syntax-error syntax "invalid define syntax"))

            (((? begin-syntax? prim) expr exprs ...)
                (mark-primitive-syntax prim)
                (walk-body (cons expr exprs) scopes))
            (((? begin-syntax?) args ...) (raise-syntax-error syntax "invalid begin syntax"))

            (((? call/cc-syntax? prim) exprs ...)
                (mark-primitive-syntax prim)
                ; TODO - not a body, nested defines shouldn't work
                (walk-body exprs scopes))

            ((expr exprs ...)
                ; TODO - not a body, nested defines shouldn't work
                (walk-body (cons expr exprs) scopes))

            (else scopes))))

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

    (walk-expression syntax (empty-scopes))
    syntax)
))
