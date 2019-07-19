(define-library 
    (p04_scheme2cps)
    (import (scheme base))
    (import (scheme write))
    (import (util))
    (import (shared))
    (export p04_scheme2cps)
(begin

(define (constant? x)
    (or
        (integer? x)
        (boolean? x)
        (char? x)
        (string? x)
        (null? x)))

(define (quote? x) (and (list? x) (eq? (car x) 'quote)))

(define (intrinsic-apply? x) 
    (and 
        (list? x)
        (intrinsic? (car x))))

(define (compile-intrinsic x next)
    (let ((intrinsic (car x))
          (args (cdr x)))
        (fold-right compile-expr 
            `(intrinsic ,(intrinsic->name intrinsic) ,next) args)))

(define (let? x) (and (list? x) (equal? (car x) 'let)))
(define (define? x) (and (list? x) (equal? (car x) 'define)))
(define (begin? x) (and (list? x) (equal? (car x) 'begin)))
(define (lambda? x) (and (list? x) (equal? (car x) 'lambda)))

(define (compile-integer x next) `(constant ,x ,next))
(define (compile-boolean x next) `(constant ,x ,next))
(define (compile-char x next) `(constant ,x ,next))
(define (compile-null x next) `(constant ,x ,next))
(define (compile-symbol x next) `(constant ,x ,next))
(define (compile-string x next) `(string ,x ,next))
(define (compile-refer x next) `(refer ,x ,next))

(define (compile-constant x next)
    (cond
        ((integer?  x) (compile-integer x next))
        ((boolean? x) (compile-boolean x next))
        ((char? x) (compile-char x next))
        ((string? x) (compile-string x next))
        ((null? x) (compile-null x next))
        ((symbol? x) (compile-symbol x next))
        ((pair? x) (compile-pair x next))))
(define (compile-quote x next) (compile-constant (cadr x) next))

(define (apply-fold x r) (compile-expr x r))
(define (apply-op x) (car x))
(define (apply-args x) (cdr x))
(define (compile-apply x next)
    (let ((op (apply-op x)) (args (apply-args x)))
        (compile-expr op (fold-right apply-fold `(apply ,(length args) ,next) args))))

(define (compile-pair x next)
    (let ((left (car x)) (right (cdr x)))
        (compile-expr left (compile-expr right `(pair ,next)))))

(define (let->bindings x) (car (cdr x)))
(define (let->body x ) (cdr (cdr x)))
(define (binding->var x) (car x))
(define (binding->val x) (car (cdr x)))
(define (compile-binding binding next)
    (compile-expr (binding->val binding) `(slot (store ,(binding->var binding) ,next))))

(define (make-begin body) (cons 'begin body))

(define (compile-let x next)
    (let ((bindings (let->bindings x)) (body (let->body x)))
        (fold-right compile-binding 
            (compile-expr (make-begin body) next)
            bindings)))

(define (define->var x) (car (cdr x)))
(define (define->body x) (cdr (cdr x)))
(define (compile-define x next)
    (let ((var (define->var x)))
        (cond
            ((list? var) (compile-define `(define ,(car var) (lambda ,(cdr var) ,@(define->body x))) next))
            ((pair? var) (compile-define `(define ,(car var) (lambda (,(cdr var)) ,@(define->body x))) next))
            (else (compile-expr (make-begin (define->body x)) `(slot (store ,(define->var x) ,next)))))))

(define (begin->body x) (cdr x))
(define (begin-fold x r) (compile-expr x r))
(define (compile-begin x next) 
    (fold-right begin-fold next (begin->body x)))

(define (lambda->bindings x) (car (cdr x)))
(define (lambda->body x ) (cdr (cdr x)))
(define (compile-lambda-binding binding next)
    `(param ,binding ,next))
(define (compile-lambda-bindings bindings next)
    (fold-right compile-lambda-binding next bindings))
(define (compile-lambda x next)
    (let ((body (compile-expr (make-begin (lambda->body x)) '(return))))
    `(close ,(lambda->bindings x) ,body ,next)))

; integer 1
; (constant 1 next)

; boolean #t
; (constant #t next)

; char a
; (constant #\a next)

; null/()
; (constant '() next)

; symbol sym
; (refer sym next)

; pair (1 . 2)
; (constant 1 (constant 2 (pair)))

; quote '(1 2)
; (constant 1 (constant 2 (pair (constant '() (pair next)))

; intrinsic add
; (intrinsic add)

; let (let ((x 1)) (add x 1))
; (constant 1 (slot (store x (refer x (constant 1 (intrinsic add next))))))

; define (define x 10)
; (constant 10 (slot (store x)))

; begin (begin 1 2)
; (constant 1 (constant 2 next))

; lambda (lambda (x) (add x 1))
; (close (param x (refer x (constant 1 (intrinsic add (return))))) next)

; apply (func 1 2)
; (constant 1 (constant 2 (apply 2)))

(define (compile-expr x next)
    (cond
        ((variable? x) (compile-refer x next))
        ((constant? x) (compile-constant x next))
        ((quote? x) (compile-quote x next))
        ((intrinsic-apply? x) (compile-intrinsic x next))
        ((let? x) (compile-let x next))
        ((define? x) (compile-define x next))
        ((begin? x) (compile-begin x next))
        ((lambda? x) (compile-lambda x next))
        ((list? x) (compile-apply x next))
        ((pair? x) (compile-pair x next))
    ))

(define (p04_scheme2cps x) (compile-expr x '(return)))))
