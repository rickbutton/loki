(define-library 
(p05_liftheapvalues)
(import (scheme base))
(import (scheme write))
(import (srfi 151))
(import (util))
(export p05_liftheapvalues)
(begin

(define (empty-heap-values) '())

(define (add-heap-value new-heap-value heap-values)
    (let ((i (index new-heap-value heap-values)))
        (if i heap-values (append heap-values (list new-heap-value)))))

(define (func->type f) (cadr f))
(define (func->mapping f) (caddr f))
(define (func->bounds f) (cadddr f))
(define (func->frees f) (cadddr (cdr f)))
(define (func->body f) (cdddr (cdr (cdr f))))

(define (string-value? v) (and (list? v) (eq? (car v) 'string)))
(define (heap-value? v) (string-value? v))

(define (map-inst inst heap-values)
    (cond
        ((string-value? inst) `(string ,(cadr inst) ,(index inst heap-values)))
        (else (error "unknown heap-value"))))

(define (map-inst-and-heap-values inst heap-values)
    (let ((new-heap-values (add-heap-value inst heap-values)))
        (cons (map-inst inst new-heap-values) new-heap-values)))

(define (map-body-and-heap-values body heap-values)
    (let ((new-body
        (map (lambda (inst)
            (cond 
                ((heap-value? inst) 
                    (let ((inst-and-heap-values (map-inst-and-heap-values inst heap-values)))
                        (set! heap-values (cdr inst-and-heap-values))
                        (car inst-and-heap-values)))
                (else inst))) body)))
    (cons new-body heap-values)))

(define (lift-heap-values f heap-values)
    (let* ((body (func->body f))
           (new-body-and-heap-values (map-body-and-heap-values body heap-values))
           (new-body (car new-body-and-heap-values))
           (new-heap-values (cdr new-body-and-heap-values)))
    (cons 
        `(func ,(func->type f) ,(func->mapping f) 
            ,(func->bounds f) ,(func->frees f) ,@new-body)
        new-heap-values)))

(define (funcs->lift-heap-values funcs heap-values)
    (let ((new-funcs 
            (map (lambda (func) 
                (let* ((func-and-heap-values (lift-heap-values func heap-values))
                    (new-func (car func-and-heap-values))
                    (new-heap-values (cdr func-and-heap-values)))
                    (set! heap-values new-heap-values)
                    new-func)) funcs)))
        (cons new-funcs heap-values)))
    
(define (p05_liftheapvalues funcs)
    (let ((funcs-and-heap-values (funcs->lift-heap-values funcs (empty-heap-values))))
        (debug (cdr funcs-and-heap-values))
        funcs-and-heap-values))))