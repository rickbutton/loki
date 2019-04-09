(define-library 
    (pass2)
    (import (scheme base))
    (import (util))
    (export pass1->pass2)
    (begin
        (define varid (makeid "$$v"))

        (define (empty-vars) (cons '() '()))
        (define (vars->bounds v) (car v))
        (define (vars->frees v) (cdr v))
        (define (vars->bounds-as-frees vars) (cons '() (append (vars->bounds vars) (vars->frees vars))))

        (define (add-bound name vars) 
            (let ((bounds (vars->bounds vars)) (frees (vars->frees vars)))
                (if (null? bounds)
                    (cons (list (cons name (varid))) frees)
                    (cons (cons (cons name (varid)) bounds) frees))))
        (define (add-free name vars) 
            (let ((bounds (vars->bounds vars)) (frees (vars->frees vars)))
                (if (null? frees)
                    (cons bounds (list (cons name (varid))))
                    (cons bounds (cons (cons name (varid)) frees)))))
        (define (var->name var) (car var))
        (define (var->mapped var) (cdr var))

        (define (name->bound name vars) (find (lambda (v) (eq? (var->name v) name)) (vars->bounds vars)))
        (define (name->free name vars) (find (lambda (v) (eq? (var->name v) name)) (vars->frees vars)))
        (define (bound-exists? name vars) (if (eq? (name->bound name vars) #f) #f #t))
        (define (free-exists? name vars) (if (eq? (name->free name vars) #f) #f #t))

        (define (cps->inst x) (all-but-last x))
        (define (cps->cont x) (last x))

        (define (mark-var inst vars)
            (let* ((name (car (cdr inst))) (nvars (add-bound name vars)) (nvar (car (vars->bounds nvars))))
                (cons `(var ,name ,(var->mapped nvar)) nvars)))

        (define (param? x) (eq? (car x) 'param))
        (define (mark-param inst vars)
            (let* ((name (car (cdr inst))) (nvars (add-bound name vars)) (nvar (car (vars->bounds nvars))))
                (cons `(param ,name ,(var->mapped nvar)) nvars)))

        (define (slot? x) (eq? (car x) 'slot))
        (define (mark-slot inst vars)
            (let* ((name (car (cdr inst))) (nvars (add-bound name vars)) (nvar (car (vars->bounds nvars))))
                (cons `(slot ,name ,(var->mapped nvar)) nvars)))

        (define (store? x) (eq? (car x) 'store))
        (define (store->name x) (car (cdr x)))
        (define (mark-store inst vars) 
            (let* ((name (store->name inst)) (bound (name->bound name vars)) (free (name->free name vars)))
                (if bound
                    (cons `(store bound ,name ,(var->mapped bound)) vars)
                    (if free
                        (cons `(store free ,name ,(var->mapped free)) vars)
                        (error (string-append "attempt to store before var: " (symbol->string name)))))))

        (define (refer? x) (eq? (car x) 'refer))
        (define (refer->name x) (car (cdr x)))
        (define (mark-refer inst vars) 
            (let* ((name (refer->name inst)) (bound (name->bound name vars)) (free (name->free name vars)))
                (if bound
                    (cons `(refer bound ,name ,(var->mapped bound)) vars)
                    (if free
                        (cons `(refer free ,name ,(var->mapped free)) vars)
                        (error (string-append "attempt to refer before store: " (symbol->string name)))))))

        (define (close? x) (eq? (car x) 'close))
        (define (close->body x) (car (cdr x)))
        (define (mark-close inst vars)
            (let* ((body-and-vars (map-cps (close->body inst) map-inst (vars->bounds-as-frees vars)))
                (body  (car body-and-vars))
                (cvars (cdr body-and-vars)))
                (cons `(close ,body) vars)))

        (define (map-inst inst vars)
            (cond
                ((param? inst) (mark-param inst vars))
                ((slot? inst)  (mark-slot inst vars))
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
))