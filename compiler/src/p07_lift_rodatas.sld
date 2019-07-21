(define-library 
(p07_lift_rodatas)
(import (scheme base))
(import (scheme write))
(import (srfi 151))
(import (util))
(export p07_lift_rodatas)
(begin

(define (func->type f) (cadr f))
(define (func->mapping f) (caddr f))
(define (func->bounds f) (cadddr f))
(define (func->frees f) (cadddr (cdr f)))
(define (func->body f) (cdddr (cdr (cdr f))))

(define (string-value? v) 
    (and (list? v) 
         (eq? (car v) 'constant)
         (string? (cadr v))))
(define (rodata? v) (string-value? v))

(define (p07_lift_rodatas funcs)
    (let ((strings '()))

        (define (map-inst inst)
            (cond
                ((string-value? inst) 
                    (set! strings (append strings (list (cadr inst))))
                    `(constant ,(cadr inst) ,(index (cadr inst) strings)))
                (else inst)))
        (define (map-body body) (map map-inst body))

        (define (map-func f)
            (let* ((body (func->body f))
                   (new-body (map-body body)))
            `(func ,(func->type f) ,(func->mapping f) 
                ,(func->bounds f) ,(func->frees f) ,@new-body)))

        (define (funcs->lift-rodatas funcs)
            (let ((new-funcs (map map-func funcs)))
                (cons new-funcs strings)))
            
        (let ((funcs-and-rodatas (funcs->lift-rodatas funcs)))
            funcs-and-rodatas)))
))
