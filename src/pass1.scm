(define-module (pass1)
    #:export (scheme->pass1))

(use-modules (util))

(define (sfixnum? i) (integer? i))
(define (sboolean? b) (boolean? b))
(define (schar? c) (char? c))
(define (ssymbol? n) (symbol? n))
(define (snull? n) (null? n))

(define (satomic? x)
    (or
        (sfixnum? x)
        (sboolean? x)
        (schar? x)
        (ssymbol? x)
        (snull? x)))
(define (squote? x) (and (list? x) (eq? (car x) 'quote)))

(define prim-list '(add sub cons car cdr))
(define (sprim? x) (and (list? x) (contains? prim-list (car x))))

(define (spair? n) (pair? n))
(define (slet? x) (and (list? x) (eq? (car x) 'let)))
(define (sdefine? x) (and (list? x) (eq? (car x) 'define)))
(define (sbegin? x) (and (list? x) (eq? (car x) 'begin)))
(define (slambda? x) (and (list? x) (eq? (car x) 'lambda)))

(define (compile-sfixnum x next) `(constant ,x ,next))
(define (compile-sboolean x next) `(constant ,x ,next))
(define (compile-schar x next) `(constant ,x ,next))
(define (compile-snull x next) `(constant ,x ,next))
(define (compile-ssymbol x next) `(refer ,x ,next))
(define (compile-squote x next) (compile-constant (cadr x) next))

(define (sprim->name x) (car x))
(define (sprim->args x) (cdr x))
(define (sprim-fold x r) (compile-expr x r))
(define (compile-sprim x next)
    (let ((name (sprim->name x)) (args (sprim->args x)))
        (fold-right sprim-fold `(primcall ,name ,next) args)))

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
(define (compile-slet x next)
    (let ((bindings (let->bindings x)) (body (let->body x)))
        (fold-right compile-binding (compile-expr (cons 'begin body) next) bindings)))

(define (define->var x) (car (cdr x)))
(define (define->body x) (cdr (cdr x)))
(define (compile-sdefine x next)
    (compile-expr (cons 'begin (define->body x)) `(slot ,(define->var x) (store ,(define->var x) ,next))))

(define (begin->body x) (cdr x))
(define (begin-fold x r) (compile-expr x r))
(define (compile-sbegin x next) (fold-right begin-fold next (begin->body x)))

(define (lambda->bindings x) (car (cdr x)))
(define (lambda->body x ) (cdr (cdr x)))
(define (compile-lambda-binding binding next)
    `(param ,binding ,next))
(define (compile-lambda-bindings bindings next)
    (fold-right compile-lambda-binding next bindings))
(define (compile-slambda x next)
    (let* ((body (compile-expr (cons 'begin (lambda->body x)) '(return)))
            (bindings (compile-lambda-bindings (lambda->bindings x) body)))
    `(close ,bindings ,next)))

(define (compile-constant x next)
    (cond
        ((sfixnum?  x) (compile-sfixnum x next))
        ((sboolean? x) (compile-sboolean x next))
        ((schar? x) (compile-schar x next))
        ((snull? x) (compile-snull x next))
        ((ssymbol? x) (compile-ssymbol x next))
        ((spair? x) (compile-spair x next))))

(define (compile-expr x next)
    (cond
        ((satomic? x) (compile-constant x next))
        ((squote? x) (compile-squote x next))
        ((sprim? x) (compile-sprim x next))
        ((slet? x) (compile-slet x next))
        ((sdefine? x) (compile-sdefine x next))
        ((sbegin? x) (compile-sbegin x next))
        ((slambda? x) (compile-slambda x next))

        ((spair? x) (compile-spair x next))
    ))

(define (scheme->pass1 x) (compile-expr (cons 'begin x) '(return)))