(define-module (pass1)
    #:export (scheme->pass1))

(use-modules (util))

; fixnum
(define (sfixnum? i) (integer? i))
(define (compile-sfixnum x next)
    `(constant ,x ,next))
; end fixnum

; boolean
(define (sboolean? b) (boolean? b))
(define (compile-sboolean x next)
    `(constant ,x ,next))
; end boolean

; char
(define (schar? c) (char? c))
(define (compile-schar x next)
    `(constant ,x ,next))
; end char

; null
(define (snull? n) (null? n))
(define (compile-snull x next)
    `(constant ,x ,next))
; end null

; prim
(define prim-list '(add sub car cdr))
(define (sprim? x) (and (list? x) (contains? prim-list (car x))))
(define (sprim->name x) (car x))
(define (sprim->args x) (cdr x))

(define (sprim-fold x r) (compile-expr x r))

(define (compile-sprim x next)
    (let ((name (sprim->name x)) (args (sprim->args x)))
        (fold-right sprim-fold `(primcall ,name ,next) args)))
; end prim

; apply
(define (apply-fold x r) (compile-expr x r))
(define (apply-op x) (car x))
(define (apply-args x) (cdr x))
(define (compile-apply x next)
    (let ((op (apply-op x)) (args (apply-args x)))
        (fold-right apply-fold (compile-expr op `(apply ,(length args) ,next)) args)))
; end apply

; pair
(define (spair? n) (pair? n))
(define (compile-spair x next)
    (if (list? x) (compile-apply x next)
    (let ((left (car x)) (right (cdr x)))
        (compile-expr left (compile-expr right `(pair ,next))))))
; end pair

; symbol
(define (ssymbol? n) (symbol? n))
(define (compile-ssymbol x next)
    `(refer ,x ,next))
; end symbol

; let
(define (slet? x) (and (list? x) (eq? (car x) 'let)))

(define (let->bindings x) (car (cdr x)))
(define (let->body x ) (cdr (cdr x)))

(define (binding->var x) (car x))
(define (binding->val x) (car (cdr x)))

(define (compile-binding binding next)
    (compile-expr (binding->val binding) `(var ,(binding->var binding) (store ,(binding->var binding) ,next))))

(define (compile-slet x next)
    (let ((bindings (let->bindings x)) (body (let->body x)))
        (fold-right compile-binding (compile-expr (cons 'begin body) next) bindings)))
; end let

; define
(define (sdefine? x) (and (list? x) (eq? (car x) 'define)))

(define (define->var x) (car (cdr x)))
(define (define->body x) (cdr (cdr x)))

(define (compile-sdefine x next)
    (compile-expr (cons 'begin (define->body x)) `(var ,(define->var x) (store ,(define->var x) ,next))))
; end define

; begin
(define (sbegin? x) (and (list? x) (eq? (car x) 'begin)))

(define (begin->body x) (cdr x))

(define (begin-fold x r) (compile-expr x r))
(define (compile-sbegin x next)
    (fold-right begin-fold next (begin->body x)))
; end begin

; lambda
(define (slambda? x) (and (list? x) (eq? (car x) 'lambda)))

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

; end lambda

(define (compile-expr x next)
    (cond
        ((sfixnum?  x) (compile-sfixnum x next))
        ((sboolean? x) (compile-sboolean x next))
        ((schar? x) (compile-schar x next))
        ((snull? x) (compile-snull x next))
        ((sprim? x) (compile-sprim x next))
        ((slet? x) (compile-slet x next))
        ((sbegin? x) (compile-sbegin x next))
        ((slambda? x) (compile-slambda x next))
        ((sdefine? x) (compile-sdefine x next))
        ((spair? x) (compile-spair x next))
        ((ssymbol? x) (compile-ssymbol x next))
    ))

(define (scheme->pass1 x) (compile-expr (cons 'begin x) '(return)))