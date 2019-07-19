(define-library 
(p07_lift_rodatas)
(import (scheme base))
(import (scheme write))
(import (srfi 151))
(import (util))
(export p07_lift_rodatas)
(begin

(define (empty-rodatas) '())

(define (add-rodata new-rodata rodatas)
    (let ((i (index new-rodata rodatas)))
        (if i rodatas (append rodatas (list new-rodata)))))

(define (func->type f) (cadr f))
(define (func->mapping f) (caddr f))
(define (func->bounds f) (cadddr f))
(define (func->frees f) (cadddr (cdr f)))
(define (func->body f) (cdddr (cdr (cdr f))))

(define (string-value? v) (and (list? v) (eq? (car v) 'string)))
(define (rodata? v) (string-value? v))

(define (map-inst inst rodatas)
    (cond
        ((string-value? inst) `(string ,(cadr inst) ,(index inst rodatas)))
        (else (error "unknown rodata"))))

(define (map-inst-and-rodatas inst rodatas)
    (let ((new-rodatas (add-rodata inst rodatas)))
        (cons (map-inst inst new-rodatas) new-rodatas)))

(define (map-body-and-rodatas body rodatas)
    (let ((new-body
        (map (lambda (inst)
            (cond 
                ((rodata? inst) 
                    (let ((inst-and-rodatas (map-inst-and-rodatas inst rodatas)))
                        (set! rodatas (cdr inst-and-rodatas))
                        (car inst-and-rodatas)))
                (else inst))) body)))
    (cons new-body rodatas)))

(define (lift-rodatas f rodatas)
    (let* ((body (func->body f))
           (new-body-and-rodatas (map-body-and-rodatas body rodatas))
           (new-body (car new-body-and-rodatas))
           (new-rodatas (cdr new-body-and-rodatas)))
    (cons 
        `(func ,(func->type f) ,(func->mapping f) 
            ,(func->bounds f) ,(func->frees f) ,@new-body)
        new-rodatas)))

(define (funcs->lift-rodatas funcs rodatas)
    (let ((new-funcs 
            (map (lambda (func) 
                (let* ((func-and-rodatas (lift-rodatas func rodatas))
                    (new-func (car func-and-rodatas))
                    (new-rodatas (cdr func-and-rodatas)))
                    (set! rodatas new-rodatas)
                    new-func)) funcs)))
        (cons new-funcs rodatas)))
    
(define (p07_lift_rodatas funcs)
    (let ((funcs-and-rodatas (funcs->lift-rodatas funcs (empty-rodatas))))
        funcs-and-rodatas))))
