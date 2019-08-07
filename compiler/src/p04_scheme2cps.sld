(define-library 
    (p04_scheme2cps)
    (import (scheme base))
    (import (scheme write))
    (import (chibi match))
    (import (srfi 159))
    (import (chibi show pretty))
    (import (util))
    (import (shared))
    (export p04_scheme2cps)
(begin

(define rvid (make-anon-id "$rv_"))
(define kid (make-anon-id "$k_"))



(define (rvid-var) (let ((id (rvid))) (make-variable id)))
(define (kid-var) (let ((id (kid))) (make-variable id)))

(define (constant? x)
    (or
        (integer? x)
        (boolean? x)
        (char? x)
        (string? x)
        (null? x)))
(define (quote? x) (and (list? x) (eq? (car x) 'quote)))

; cps conversion gleefully lifted from:
; http://matt.might.net/articles/cps-conversion/
(define (aexpr? expr)
    (match expr
        ((or ('lambda (_ ...) _)
             'call/cc
             (? variable?)
             (? intrinsic?)
             (? quote?)
             (? constant?)) #t)
        (else #f)))

(define (T-k expr k)
    (match expr
        ((? aexpr?) (k (M expr)))
        (('begin expr) 
            (T-k expr k))
        (('begin expr exprs ...) 
            (T-k expr (lambda (_) (T-k `(begin ,@exprs) k))))
        (('if expr-cond expr-cons expr-alt)
            (let* (($rv (rvid-var)) (cont `(lambda (,$rv) ,(k $rv))))
                (T-k expr-cond (lambda (aexp)
                    `(if ,aexp
                         ,(T-c expr-cons cont)
                         ,(T-c expr-alt cont))))))
        (('set! var expr) (T-k expr (lambda (aexp) 
            `(begin
                (set! ,var ,aexp)
                ,(k '(void))))))
        ; TODO - support (define (id formals) ...)
        (('define var expr) (T-k expr (lambda (aexp) 
            `(begin
                (set! ,var ,aexp)
                ,(k '(void))))))
        ((_ _ ...)
            (let* (($rv (rvid-var)) (cont `(lambda (,$rv) ,(k $rv))))
                (T-c expr cont)))))

(define (T-c expr c)
    (match expr
        ((? aexpr?) `(,c ,(M expr)))
        (('begin expr) (T-c expr c))
        (('begin expr exprs ...)
            (T-k expr (lambda (_) (T-c `(begin ,@exprs) c))))
        (('if expr-cond expr-cons expr-alt)
            (let (($k (kid-var)))
                `((lambda (,$k)
                    ,(T-k expr-cond (lambda (aexp) 
                        `(if ,aexp
                             ,(T-c expr-cons $k)
                             ,(T-c expr-alt $k)))))
                ,c)))
        (('set! var expr) (T-k expr (lambda (aexp) 
            `(begin
                (set! ,var ,aexp)
                (,c (void))))))
        ; TODO - support (define (id formals) ...)
        (('define var expr) (T-k expr (lambda (aexp) 
            `(begin
                (set! ,var ,aexp)
                (,c (void))))))
        (((and i (? intrinsic?)) es ...)
            (T*-k es (lambda ($es)
                `(intrinsic ,i ,@$es ,c))))
        ((f es ...)
            (T-k f (lambda ($f)
                (T*-k es (lambda ($es)
                    `(,$f ,@$es ,c))))))))

(define (T*-k exprs k)
    (cond
        ((null? exprs) (k '()))
        ((pair? exprs) (T-k (car exprs) 
            (lambda (hd) (T*-k (cdr exprs) 
                (lambda (tl) (k (cons hd tl)))))))))

(define (intrisnic-call? x)
    (and (list? x) (intrinsic? (car x) )))

; for call/cc
; TODO - clean this up?
(define fid (make-anon-id "$f_"))
(define ccid (make-anon-id "$cc_"))
(define xid (make-anon-id "$x_"))
(define emptyid (make-anon-id "$__"))

(define (M aexpr)
    (match aexpr
        (('lambda (vars ...) body)
            (let (($k (kid-var)))
                `(lambda (,@vars ,$k) ,(T-c body $k))))
        ((or 'call/cc) 
            (let ((f (make-variable (fid)))
                  (cc (make-variable (ccid)))
                  (x (make-variable (xid)))
                  (_ (make-variable (emptyid))))
                `(lambda (,f ,cc) (,f (lambda (,x ,_) (,cc ,x)) ,cc))))
        ((? constant?) aexpr)
        ((? variable?) aexpr)
        ((? quote?) aexpr)
        ((exit) aexpr)
        ((void) aexpr)
        (else (raise 
            (string-append "invalid aexpr: " (show #f aexpr))))))

(define (p04_scheme2cps x) (T-c x '(exit)))))
