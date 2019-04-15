(define-library 
    (p01_scheme2cps)
    (import (scheme base))
    (import (scheme write))
    (import (util))
    (export p01_scheme2cps)
(begin

(define (satomic? x)
    (or
        (integer? x)
        (boolean? x)
        (char? x)
        (symbol? x)
        (null? x)))

(define (quote? x) (and (list? x) (eq? (car x) 'quote)))

(define prim-list '(add sub cons car cdr))
(define (prim? x) (and (list? x) (contains? prim-list (car x))))

(define (spair? n) (pair? n))
(define (let? x) (and (list? x) (eq? (car x) 'let)))
(define (define? x) (and (list? x) (eq? (car x) 'define)))
(define (begin? x) (and (list? x) (eq? (car x) 'begin)))
(define (lambda? x) (and (list? x) (eq? (car x) 'lambda)))

(define (compile-integer x next) `(constant ,x ,next))
(define (compile-boolean x next) `(constant ,x ,next))
(define (compile-char x next) `(constant ,x ,next))
(define (compile-null x next) `(constant ,x ,next))
(define (compile-symbol x next) `(refer ,x ,next))
(define (compile-quote x next) (compile-constant (cadr x) next))

(define (prim->op x) (car x))
(define (prim->args x) (cdr x))
(define (prim->argc x) (length (prim->args x)))

(define (ensure-prim-argc p name c inst)
    (if (eq? (prim->argc p) c)
        inst
        (error (string-append 
            "invalid number of args to prim " 
            (symbol->string name)
            ", expected "
            (number->string c)
            ", received "
            (number->string (prim->argc p))))))       

(define (compile-mathprim-rest args prim next)
    (fold-right (lambda (a r) (compile-expr a `(primcall ,prim ,r))) next args))

(define (compile-mathprim p next)
    (let ((op (prim->op p)) (args (prim->args p)) (argc (prim->argc p)))
        (cond
            ((eq? argc 0) (compile-expr 0 next))
            ((eq? argc 1) (compile-expr (car args) next))
            ((eq? argc 2) (compile-expr (car args) (compile-expr (cadr args) `(primcall ,op ,next))))
            (else (compile-expr (car args) (compile-expr (cadr args) `(primcall ,op ,(compile-mathprim-rest (cddr args) op next))))))))

(define (compile-prim p next)
    (let ((op (prim->op p)))
        (cond
            ((eq? op 'add) (compile-mathprim p next))
            ((eq? op 'sub) (compile-mathprim p next))
            ((eq? op 'car) (ensure-prim-argc p 'car 1 '(call $$car)))
            ((eq? op 'cdr) (ensure-prim-argc p 'cdr 1 '(call $$cdr)))
            ((eq? op 'cons) (ensure-prim-argc p 'cons 2 '(call $$alloc-pair)))
            (else (error (string-append "invalid primcall: " op))))))

(define (apply-fold x r) (compile-expr x r))
(define (apply-op x) (car x))
(define (apply-args x) (cdr x))
(define (compile-apply x next)
    (let ((op (apply-op x)) (args (apply-args x)))
        (compile-expr op (fold-right apply-fold `(apply ,(length args) ,next) args))))

(define (compile-spair x next)
    (let ((left (car x)) (right (cdr x)))
        (compile-expr left (compile-expr right `(pair ,next)))))


(define (let->bindings x) (car (cdr x)))
(define (let->body x ) (cdr (cdr x)))
(define (binding->var x) (car x))
(define (binding->val x) (car (cdr x)))
(define (compile-binding binding next)
    (compile-expr (binding->val binding) `(slot ,(binding->var binding) (store ,(binding->var binding) ,next))))
(define (compile-let x next)
    (let ((bindings (let->bindings x)) (body (let->body x)))
        (fold-right compile-binding (compile-expr (cons 'begin body) next) bindings)))

(define (define->var x) (car (cdr x)))
(define (define->body x) (cdr (cdr x)))
(define (compile-define x next)
    (let ((var (define->var x)))
        (cond
            ((list? var) (compile-define `(define ,(car var) (lambda ,(cdr var) ,@(define->body x))) next))
            ((pair? var) (compile-define `(define ,(car var) (lambda (,(cdr var)) ,@(define->body x))) next))
            (else (compile-expr (cons 'begin (define->body x)) `(slot ,(define->var x) (store ,(define->var x) ,next)))))))

(define (begin->body x) (cdr x))
(define (begin-fold x r) (compile-expr x r))
(define (compile-begin x next) (fold-right begin-fold next (begin->body x)))


; let - `(scope ,var ,(fold-right compile-let-binding (compile (cons 'begin body) '(end)) bindings) ,next)))
; define - `(define ,name ,(compile body `(slot ,name (store ,name ,next)))))
; lambda - `(close ,bindings ,(compile `(begin ,@body) '(return)) ,next))

(define (lambda->bindings x) (car (cdr x)))
(define (lambda->body x ) (cdr (cdr x)))
(define (compile-lambda-binding binding next)
    `(param ,binding ,next))
(define (compile-lambda-bindings bindings next)
    (fold-right compile-lambda-binding next bindings))
(define (compile-lambda x next)
    (let* ((body (compile-expr (cons 'begin (lambda->body x)) '(return)))
            (bindings (compile-lambda-bindings (lambda->bindings x) body)))
    `(close ,bindings ,next)))

(define (compile-constant x next)
    (cond
        ((integer?  x) (compile-integer x next))
        ((boolean? x) (compile-boolean x next))
        ((char? x) (compile-char x next))
        ((null? x) (compile-null x next))
        ((symbol? x) (compile-symbol x next))
        ((spair? x) (compile-spair x next))))

(define (compile-expr x next)
    (cond
        ((satomic? x) (compile-constant x next))
        ((quote? x) (compile-quote x next))
        ((prim? x) (compile-prim x next))
        ((let? x) (compile-let x next))
        ((define? x) (compile-define x next))
        ((begin? x) (compile-begin x next))
        ((lambda? x) (compile-lambda x next))

        ((spair? x) (compile-apply x next))
    ))

(define (p01_scheme2cps x) (compile-expr (cons 'begin x) '(return)))))