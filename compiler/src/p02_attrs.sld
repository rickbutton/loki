(define-library
    (p02_attrs)
    (import (scheme base))
    (import (util))
    (import (shared))
    (export p02_attrs)
(begin

(define varid (make-named-id "$v"))
(define (empty-vars) (cons '() '()))
(define (vars->bounds v) (car v))
(define (vars->frees v) (cdr v))
(define (vars->bounds-as-frees vars) (cons '() (append (vars->bounds vars) (vars->frees vars))))

(define (new-var name) (cons name (varid name)))
(define (add-bound var vars)
    (let ((bounds (vars->bounds vars)) (frees (vars->frees vars)))
        (if (null? bounds)
                (cons (list var) frees)
                (cons (cons var bounds) frees))))
(define (add-bound-name name vars) (add-bound (new-var name) vars))
        
(define (var->name var) (car var))
(define (var->mapped var) (cdr var))

(define (name->bound name vars) (find (lambda (v) (eq? (var->name v) name)) (vars->bounds vars)))
(define (name->free name vars) (find (lambda (v) (eq? (var->name v) name)) (vars->frees vars)))
(define (bound-exists? name vars) (if (eq? (name->bound name vars) #f) #f #t))
(define (free-exists? name vars) (if (eq? (name->free name vars) #f) #f #t))

(define (cons-symbol-syntax? syntax symbol)
    (if (cons-syntax? syntax)
        (let ((car-syntax (cons-syntax->car syntax)))
            (and 
                (atom-syntax? car-syntax) 
                (equal? (atom-syntax->type car-syntax) 'symbol)
                (equal? (atom-syntax->value car-syntax) symbol)))
        #f))

(define (safe-car-syntax syntax) (if (cons-syntax? syntax) (cons-syntax->car syntax) #f))
(define (safe-cdr-syntax syntax) (if (cons-syntax? syntax) (cons-syntax->cdr syntax) #f))
(define (null-syntax? syntax) (and (atom-syntax? syntax) (equal? (atom-syntax->type syntax) 'null)))

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

; quote-context => none, quote, quasiquote
(define (walk-syntax-validate syntax quote-context)
    (cond
        ((equal? quote-context 'none)
            ; we are not in a quote syntax of some kind
            ; so we need to validate this part of the AST
            (cond
                ((quote-syntax? syntax) 
                    (if (valid-quote-syntax? syntax) 
                        (walk-syntax-validate (quote->datum syntax) 'quote)
                        (raise "invalid quote syntax")))
                ((quasiquote-syntax? syntax) 
                    (if (valid-quasiquote-syntax? syntax) 
                        (walk-syntax-validate (quote->datum syntax) 'quasiquote)
                        (raise "invalid quasiquote syntax")))
                ((unquote-syntax? syntax)
                    (if (valid-unquote-syntax? syntax)
                        (raise "invalid attempt to unquote when not in quasiquote")
                        (raise "invalid unquote syntax")))
                ((unquote-splicing-syntax? syntax)
                    (if (valid-unquote-splicing-syntax? syntax)
                        (raise "invalid attempt to unquote-splicing when not in quasiquote")
                        (raise "invalid unquote-splicing syntax")))
                ((cons-syntax? syntax)
                    (walk-syntax-validate (cons-syntax->car syntax) quote-context)
                    (walk-syntax-validate (cons-syntax->cdr syntax) quote-context))))
        ((equal? quote-context 'quasiquote)
            ; we are inside a quasiquote syntax
            ; we only need to validate unquote and unquote splice
            (cond
                ((unquote-syntax? syntax)
                    (if (valid-unquote-syntax? syntax)
                        (walk-syntax-validate (unquote->expr syntax) 'none)
                        (raise "invalid unquote syntax")))
                ((unquote-splicing-syntax? syntax)
                    (if (valid-unquote-splicing-syntax? syntax)
                        (walk-syntax-validate (unquote->expr syntax) 'none)
                        (raise "invalid unquote-splicing syntax")))
                ((cons-syntax? syntax)
                    (walk-syntax-validate (cons-syntax->car syntax) quote-context)
                    (walk-syntax-validate (cons-syntax->cdr syntax) quote-context))))))

; valid syntax
; <variable>
; <constant>
; (quote <datum>)
; (quasiquote <datum>)
; (unquote expr)
; (unquote-splicing expr)
; (op operand...)
; (lambda (formals) body)
; (if test conse alte)
; (if test conse)
; (set! <variable> expr)
; (include string...)
; (include-ci string...)

(define (p02_attrs syntax)
    (walk-syntax-validate syntax 'none)
    syntax)
))