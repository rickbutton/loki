(define-library 
(p07_lift_rodatas)
(import (scheme base))
(import (scheme write))
(import (chibi match))
(import (srfi 151))
(import (util))
(export p07_lift_rodatas)
(begin

(define (func->type f) (cadr f))
(define (func->mapping f) (caddr f))
(define (func->bounds f) (cadddr f))
(define (func->frees f) (cadddr (cdr f)))
(define (func->locals f) (cadddr (cddr f)))
(define (func->body f) (cdddr (cdddr f)))

(define (string-value? v) 
    (and (list? v) 
         (eq? (car v) 'constant)
         (string? (cadr v))))
(define (rodata? v) (string-value? v))

(define (p07_lift_rodatas funcs)
    (let ((strings '()))

        (define (lift-string x)
            (set! strings (append strings (list x)))
            `(rodata ,(index x strings) ,x))

        (define (map-expr expr)
            (match expr
                (('begin exprs ...)
                    `(begin ,@(map-body exprs)))
                (('makeclosure args ...) expr)
                ('(set! var expr)
                    `(set! ,var ,(map-expr expr)))
                (('if condition conseq alt)
                    `(if
                        ,(map-expr condition)
                        ,(map-expr conseq)
                        ,(map-expr alt)))
                (('void) expr)
                ((? pair?)
                    (cons (map-expr (car expr))
                          (map-expr (cdr expr))))
                ((? string?) (lift-string expr))
                (else expr)))

        (define (map-body body) (map map-expr body))

        (define (make-init-rodata-call rodata index)
            `(set-rodata ,index ,rodata))
        (define (make-init-rodata-func rodatas)
            `(func open $$finit () () ()
                ,@(map make-init-rodata-call 
                    rodatas (range 0 (length rodatas) 1))
                0)) ; TODO - remove 0, only needed because all funcs 
                    ; must currently return a value

        (define (map-func f)
            (let* ((body (func->body f))
                   (new-body (map-body body)))
            `(func 
                ,(func->type f) 
                ,(func->mapping f) 
                ,(func->bounds f) 
                ,(func->frees f) 
                ,(func->locals f)
                ,@new-body)))

        (define (funcs->lift-rodatas funcs)
            (let ((new-funcs (map map-func funcs)))
                (cons strings (cons (make-init-rodata-func strings) new-funcs))))
            
        (funcs->lift-rodatas funcs)
))))
