(define-library 
    (p02_markvars)
    (import (scheme base))
    (import (scheme write))
    (import (util))
    (export p02_markvars)
(begin

(define varid (make-named-id "$v"))

(define (empty-vars) (cons '() '()))
(define (vars->bounds v) (car v))
(define (vars->frees v) (cdr v))
(define (vars->bounds-as-frees vars) (cons '() (append (vars->bounds vars) (vars->frees vars))))

(define (new-var name) (cons name (varid name)))
(define (add-bound var vars)
    (let ((bounds (vars->bounds vars)) (frees (vars->frees vars)))
        (if (null? bounds)
                (cons (list var) frees)
                (cons (cons var bounds) frees))))
(define (add-bound-name name vars) (add-bound (new-var name) vars))
        
(define (var->name var) (car var))
(define (var->mapped var) (cdr var))

(define (name->bound name vars) (find (lambda (v) (eq? (var->name v) name)) (vars->bounds vars)))
(define (name->free name vars) (find (lambda (v) (eq? (var->name v) name)) (vars->frees vars)))
(define (bound-exists? name vars) (if (eq? (name->bound name vars) #f) #f #t))
(define (free-exists? name vars) (if (eq? (name->free name vars) #f) #f #t))

(define (inst->op inst) (car inst))
(define (inst->next inst) (if (= (length inst) 1) #f (last inst)))

(define (scope? inst) (eq? 'scope (inst->op inst)))
(define (refer? inst) (eq? 'refer (inst->op inst)))
(define (define? inst) (eq? 'define (inst->op inst)))
(define (close? inst) (eq? 'close (inst->op inst)))

(define (mark-store x vars)
    (let ((name (cadr x)))
        (if (bound-exists? name vars)
            `(store bound ,name ,(var->mapped (name->bound name vars)) ,(mark (inst->next x) vars))
            (if (free-exists? name vars)
                `(store free ,name ,(var->mapped (name->free name vars)) ,(mark (inst->next x) vars))
                (error (string-append "attempted to store in unbound variable: " (symbol->string name)))))))

(define (mark-refer x vars)
    (let ((name (cadr x)))
        (if (bound-exists? name vars)
            `(refer bound ,name ,(var->mapped (name->bound name vars)) ,(mark (inst->next x) vars))
            (if (free-exists? name vars)
                `(refer free ,name ,(var->mapped (name->free name vars)) ,(mark (inst->next x) vars))
                (error (string-append "attempted to refer to unbound variable: " (symbol->string name)))))))

(define (replace-end x next)
    (if (list? x)
        (if (eq? (car x) 'end)
            next
            (map (lambda (i) (replace-end i next)) x))
        x))

(define (binding->new-var binding)
    (new-var (car binding)))
(define (mark-scope x vars)
    (let* ((bindings (cadr x)) 
           (names (map car bindings))
           (new-var-list (map new-var names))
           (new-vars-list (map (lambda (v) (add-bound v vars)) new-var-list))
           (values (map cdr bindings))
           (marked-bindings (map (lambda (value nvars) (mark value nvars)) values new-vars-list))
           (folded-bindings (fold-left replace-end '(end) marked-bindings))
           (body (caddr x)) 
           (next (inst->next x))
           (nvars (fold-right add-bound vars new-var-list)))
            (replace-end folded-bindings (replace-end (mark body nvars) (mark next vars)))))

(define (mark-define x vars)
    (let* ((name (cadr x)) (nvars (add-bound-name name vars)))
        (mark (inst->next x) nvars)))

(define (mark-close x vars)
    (let* ((names (cadr x)) (body (caddr x)) (nvars (fold-right add-bound-name (vars->bounds-as-frees vars) names)))
        `(close ,(map (lambda (n) (name->bound n nvars)) names) ,(mark body nvars) ,(mark (inst->next x) vars))))

(define (mark-regular x vars)
    (let ((next (inst->next x)))
        (if next `(,@(all-but-last x) ,(mark next vars)) x)))

(define (mark x vars)
    (let ((op (inst->op x)))
        (cond
            ((eq? op 'scope) (mark-scope x vars))
            ((eq? op 'refer) (mark-refer x vars))
            ((eq? op 'store) (mark-store x vars))
            ((eq? op 'define) (mark-define x vars))
            ((eq? op 'close) (mark-close x vars))
            (else (mark-regular x vars)))))

(define (p02_markvars x) (mark x (empty-vars)))))