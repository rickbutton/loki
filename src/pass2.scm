(define-module (pass2)
    #:export (pass1->pass2))

(use-modules (util))
(use-modules (srfi srfi-1))

(define (empty-vars) '())

(define (add-var name vars) (if (null? vars) 
    (list (cons name (gensym "$$v")))
    (cons (cons name (gensym "$$v")) vars)))
(define (var->name var) (car var))
(define (var->mapped var) (cdr var))

(define (name->var name vars) (find (lambda (v) (eq? (var->name v) name)) vars))
(define (name-exists name vars) (if (eq? (name->var name vars) #f) #f #t))

(define (cps->inst x) (all-but-last x))
(define (cps->cont x) (last x))

(define (var? x) (eq? (car x) 'var))
(define (mark-var inst vars)
    (let* ((name (car (cdr inst))) (nvars (add-var name vars)) (nvar (car nvars)))
        (cons `(var ,name ,(var->mapped nvar)) nvars)))

(define (param? x) (eq? (car x) 'param))
(define (mark-param inst vars)
    (let* ((name (car (cdr inst))) (nvars (add-var name vars)) (nvar (car nvars)))
        (cons `(param ,name ,(var->mapped nvar)) nvars)))

(define (store? x) (eq? (car x) 'store))
(define (store->name x) (car (cdr x)))
(define (mark-store inst vars) 
    (let* ((name (store->name inst)) (var (name->var name vars)))
        (if var
            (cons `(store ,name ,(var->mapped var)) vars)
            (error (string-append "attempt to store before var: " (symbol->string name))))))

(define (refer? x) (eq? (car x) 'refer))
(define (refer->name x) (car (cdr x)))
(define (mark-refer inst vars) 
    (let* ((name (refer->name inst)) (var (name->var name vars)))
        (if var
            (cons `(refer ,name ,(var->mapped var)) vars)
            (error (string-append "attempt to refer before store: " (symbol->string name))))))

(define (close? x) (eq? (car x) 'close))
(define (close->body x) (car (cdr x)))
(define (mark-close inst vars)
    (let* ((body-and-vars (map-cps (close->body inst) map-inst (empty-vars)))
           (body  (car body-and-vars))
           (cvars (cdr body-and-vars)))
        (cons `(close ,body) vars)))

(define (map-inst inst vars)
    (cond
        ((var?   inst) (mark-var   inst vars))
        ((param? inst) (mark-param inst vars))
        ((store? inst) (mark-store inst vars))
        ((refer? inst) (mark-refer inst vars))
        ((close? inst) (mark-close inst vars))
        (else (cons inst vars))))

(define (map-cps x func vars)
    (if (eq? (length x) 1) (cons x vars)
        (let* ((inst           (cps->inst x))
               (cont           (cps->cont x))
               (ninst-and-vars (func inst vars))
               (ninst  (car ninst-and-vars))
               (nvars  (cdr ninst-and-vars))
               (ncont-and-vars (map-cps cont func nvars))
               (ncont  (car ncont-and-vars))
               (nnvars (cdr ncont-and-vars)))
            (cons (append ninst (list ncont)) nnvars))))


(define (pass1->pass2 x)
    (let* ((pass (map-cps x map-inst (empty-vars)))
           (end-pass  (car pass))
           (end-vars  (cdr pass)))
           end-pass))