(define-library (lang core)
(import (scheme base))
(import (loki reader))
(import (loki match))
(import (loki util))
(import (loki core reflect))
(import (loki compiler))
(import (loki match))
(import (srfi 1))
#;(import (loki syntax))
(export core::anon-ref
        core::if
        core::let-var
        core::let
        core::letrec
        core::lambda
        core::set!
        core::define-global!
        core::define
        core::atomic
        core::ref
        core::ref?
        core::ref-name
        core::apply
        core::apply-anon
        compile-core-to-host-scheme)
(begin

; core ir
; let
; if
; lambda
; set!
; define-global!
; define
; atomic
; ref
; apply

; normalized ir
; - define
; - let
; + letrec

(define (make-new-name prefix name suffix)
  (datum->syntax name
                (string->symbol (string-append prefix
                                               (symbol->string (syntax->datum name))
                                               suffix))))
(define (field-accessor-name name field)
  (make-new-name "core::" name (string-append "-" (symbol->string (syntax->datum field)))))

#;(define-syntax define-core-type
  (lambda (e)
    (syntax-case ()
      ((_ name (field pred?) ...)
        (let ((raw-constructor-name (make-new-name "core::" (syntax name) "-record"))
              (constructor-name (make-new-name "core::" (syntax name) "")))
          (quasisyntax (begin
            (define-record-type (unsyntax (make-new-name "<core::" (syntax name) ">"))
              ((unsyntax constructor-name) field ...)
              (unsyntax (make-new-name "core::" (syntax name) "?"))
              (fields (unsyntax (field-accessor-name (syntax name) (syntax field)))) ...)
            (define ((unsyntax constructor-name) field ...)
              (unless (pred? field)
                (error (unsyntax (string-append (symbol->string constructor-name)
                                                " : Invalid type supplied to field "
                                                (symbol->string (syntax->datum (syntax field)))
                                                " while constructing core::"
                                                (symbol->string (syntax->datum (syntax name))))))) ...
              ((unsyntax raw-constructor-name) field ...)))))))))


;(define-core-type if test consequent alternate)

;(define-core-type let-var name value)
;(define-core-type let names body)

;(define-core-type lambda formals rest body)
;(define-core-type set! name value)
;(define-core-type define-global! name value)
;(define-core-type define name value)
;(define-core-type atomic value)
;(define-core-type ref name)
;(define-core-type apply proc args)
;
;(define-core-type letrec names values body)


(define-syntax core::anon-ref
  (syntax-rules ()
    ((_ id)
     (core::ref 'id))))
(define-syntax core::apply-anon
  (syntax-rules ()
    ((_ id args ...)
      (core::apply (core::anon-ref id) (list args ...)))))

(define-record-type <core::let-var>
  (core::let-var name value)
  core::let-var?
  (name core::let-var-name)
  (value core::let-var-value))

(define-record-type <core::let>
  (core::let vars body)
  core::let?
  (vars core::let-vars)
  (body core::let-body))

(define-record-type <core::if>
  (core::if test consequent alternate)
  core::if?
  (test core::if-test)
  (consequent core::if-consequent)
  (alternate core::if-alternate))

(define-record-type <core::lambda>
  (core::lambda formals rest body)
  core::lambda?
  (formals core::lambda-formals)
  (rest core::lambda-rest)
  (body core::lambda-body))

(define-record-type <core::set!>
  (core::set! name value)
  core::set!?
  (name core::set!-name)
  (value core::set!-value))

(define-record-type <core::define-global!>
  (core::define-global! name value)
  core::define-global!?
  (name core::define-global!-name)
  (value core::define-global!-value))

(define-record-type <core::define>
  (core::define name value)
  core::define?
  (name core::define-name)
  (value core::define-value))

(define-record-type <core::atomic>
  (core::atomic value)
  core::atomic?
  (value core::atomic-value))

(define-record-type <core::ref>
  (core::ref name)
  core::ref?
  (name core::ref-name))

(define-record-type <core::apply>
  (core::apply proc args)
  core::apply?
  (proc core::apply-proc)
  (args core::apply-args))

(define-record-type <core::letrec>
  (core::letrec vars body)
  core::letrec?
  (vars core::letrec-vars)
  (body core::letrec-body))

; anf transform based on http://matt.might.net/articles/a-normalization/

(define void (if #f #f))
(define (void? obj) (eq? void obj))

(define (atomic-value? term)
  (or (is-a? term <core::atomic>)
      (is-a? term <core::ref>)
      (is-a? term <core::lambda>)
      (void? term)))

;; Expression normalization:
(define (normalize-term exp) (normalize exp (lambda (x) x)))

(define (normalize-terms exps)
  (let loop ((exps exps)
             (terms '())
             (vars '()))
    (if (null? exps)
        (if (null? vars)
          (reverse terms)
          (list (core::letrec (reverse vars) (reverse terms))))
        (let ((exp (car exps)))
          (match exp
            (($ <core::define> name value)
              (loop (cdr exps) terms (cons (core::let-var name (normalize-term value)) vars)))
            (else (loop (cdr exps) (cons (normalize-term exp) terms) vars)))))))
          
(define (normalize exp k)
  (match exp

     (($ <core::let> '() body)
      (if (null? (cdr body))
        (normalize (car body) k)
        (k (core::letrec '() (normalize-terms body)))))

     (($ <core::let> (($ <core::let-var> name value) . clause) body)
      (normalize value (lambda (aexp-value) 
       (core::letrec (list (core::let-var name aexp-value))
         (list (normalize (core::let clause body) k))))))

    (($ <core::if> exp1 exp2 exp3)    
      (normalize-name exp1 (lambda (t)
       (k (core::if t
                    (normalize-term exp2) 
                    (normalize-term exp3))))))

    (($ <core::lambda> formals rest body)
      (k (core::lambda formals rest (normalize-terms body))))
    
    (($ <core::set!> name value)
      (normalize-name value (lambda (t)
        (k (core::set! name t)))))

    (($ <core::define-global!> name value)
      (k (core::define name (normalize-term value))))

    (($ <core::atomic>)            
     (k exp))
    (($ <core::ref>)            
     (k exp))

    (($ <core::apply> proc args) 
      (normalize-name proc (lambda (t) 
       (normalize-name* args (lambda (t*)
        (k (core::apply t t*)))))))))
    

(define (normalize-name exp k)
  (normalize exp (lambda (aexp) 
    (if (atomic-value? aexp) (k aexp) 
        (let ((t (core::ref (generate-guid 't))))
          (core::letrec (list (core::let-var t aexp))
                        (list (k t))))))))

(define (normalize-name* exp* k)
  (unless (list? exp*) (error "not list" exp*))
  (if (null? exp*)
      (k '())
      (normalize-name (car exp*) (lambda (t) 
       (normalize-name* (cdr exp*) (lambda (t*) 
        (k `(,t . ,t*))))))))

(define (compile-term term)
  (match term
    (($ <core::letrec> vars body)
      `(letrec ,(map (lambda (var) (list (core::ref-name (core::let-var-name var)) (compile-term (core::let-var-value var)))) vars)
        ,@(map compile-term body)))
    (($ <core::if> exp1 exp2 exp3) `(if ,(compile-term exp1) ,(compile-term exp2) ,(compile-term exp3)))
    (($ <core::lambda> formals rest body)
      (let ((formals (map core::ref-name formals))
            (rest (if rest (core::ref-name rest) #f)))
        `(lambda ,(if rest (if (pair? formals) (apply cons* (append formals (list rest))) rest) formals) ,@(map compile-term body))))
    (($ <core::set!> name value) `(set! ,(core::ref-name name) ,(compile-term value)))
    (($ <core::define> name value) `(define ,(core::ref-name name) ,(compile-term value)))
    (($ <core::define-global!> name value) `(define-global! ,(core::ref-name name) ,(compile-term value)))
    (($ <core::atomic> value) `(quote ,value))
    (($ <core::ref> name) name)
    (($ <core::apply> proc args) `(,(compile-term proc) ,@(map compile-term args)))))

(define (compile-core-to-host-scheme terms)
  (let ((normalized (normalize-terms terms)))
    (map compile-term normalized)))

))
