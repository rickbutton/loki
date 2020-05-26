(define-library 
(loki p05_liftlambda)
(import (scheme base))
(import (chibi match))
(import (loki util))
(import (loki shared))
(export p05_liftlambda)
(begin


; need to handle
; lambda
; aexprs
; if
; set!
; define
; function application?

(define (p05_liftlambda x) 
    (define funcid (make-anon-id "$$f"))

    (define funcs '())
    (define (push-func func) 
        (set! funcs (cons func funcs))
        func)
    (define (func->type func) (cadr func))
    (define (func->name func) (caddr func))
    (define (func->bounds func) (cadddr func))
    (define (func->frees func) (cadddr (cdr func)))
    (define (func->locals func) (cadddr (cddr func)))

    (define (body-expr->frees vars body-expr)
        (match body-expr
            (('makeclosure name vars frees) 
                frees)
            (('begin expr exprs ...)
                (append (body-expr->frees vars expr)
                        (body->frees vars exprs)))
            ((e es ...) 
                (append (body-expr->frees vars e) 
                        (body->frees vars es)))
            ((? variable?)
                (if (member body-expr vars) '() (list body-expr)))
            (else '())))

    (define (body->frees vars body) 
        (unique (apply append 
            (map (lambda (e) (body-expr->frees vars e)) body))))

    (define (body-expr->locals vars frees body-expr)
        (match body-expr
            (('makeclosure name bounds frees) frees)
            (('begin expr exprs ...)
                (append (body-expr->locals vars frees expr)
                        (body->locals vars frees exprs)))
            (('set! var expr)
                (if (or
                        (member var vars))
                        (member var frees))
                    '() (list var))
            ((e es ...) 
                (append (body-expr->locals vars frees e) 
                        (body->locals vars frees es)))
            ((? variable?)
                (if (or
                        (member body-expr vars))
                        (member body-expr frees))
                    '() (list body-expr))
            (else '())))

    (define (body->locals vars frees body) 
        (filter 
            (lambda (l) (not (or (member l vars) (member l frees))))
            (unique (apply append (map 
                (lambda (e) (body-expr->locals vars frees e)) body)))))

    (define (lift-lambda-body body) (map lift body))
    ; TODO - marking bounds and frees for the resulting closure?
    (define (lambda->func vars body)
        (let* ((lifted-body (lift-lambda-body body))
               (frees (body->frees vars lifted-body))
               (frees-without-bounds (filter
                    (lambda (f) (not (member f vars))) frees))
               (locals (body->locals 
                            vars frees-without-bounds lifted-body)))
            `(func close
                ,(funcid) 
                ,vars 
                ,frees-without-bounds 
                ,locals
                ,lifted-body)))

    (define (makeclosure? x) (and (pair? x) (eq? (car x) 'makeclosure)))
    (define (makeclosure->name x) (cadr x))
    (define (makeclosure->bounds x) (caddr x))
    (define (makeclosure->frees x) (cadddr x))
    (define (makeclosure func)
        `(makeclosure 
            ,(func->name func) 
            ,(func->bounds func) 
            ,(func->frees func)))

    (define (make-entry-func body)
        `(func open
            $$fentry
            ()
            ()
            ,(body->locals '() '() body)
            ,body))

    (define exit-func
        (let ((var (make-variable '$v)))
            `(func close $$fexit (,var) () () ,var)))

    (define (lift x)
        (match x
            (('lambda (vars ...) body)
                (makeclosure (push-func (lambda->func vars body))))
            (('if condition (consequent ...) (alternate ...)) 
                `(if 
                    ,(lift condition) 
                    ,(lift consequent) 
                    ,(lift alternate)))
            (('set! variable expr)
                `(set! ,variable ,(lift expr)))
            (('define variable expr)
                `(define ,variable ,(lift expr)))
            (('exit) (makeclosure exit-func))
            ((? pair?)
                (cons (lift (car x)) (lift (cdr x))))
            (else x)))

    (push-func exit-func)
    (let ((lifted (lift x)))
        (cons (make-entry-func lifted) funcs)))))
