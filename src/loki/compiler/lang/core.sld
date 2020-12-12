(define-library (loki compiler lang core)
(import (scheme base))
(import (loki match))
(import (loki util))
(import (loki core reflect))
(import (loki match))
(import (srfi 1))
(export core::anon-ref
        core::if
        core::let-var
        core::letrec
        core::lambda
        core::lambda?
        core::set!
        core::define-global!
        core::constant
        core::ref
        core::ref?
        core::ref-name
        core::apply
        core::apply-anon
        core::atomic?
        compile-core-to-host-scheme
        core::serialize)
(begin

; core language!

;(define-core-type if test consequent alternate)

;(define-core-type let-var name value)
;(define-core-type letrec names values body)

;(define-core-type lambda formals rest body)
;(define-core-type set! name value)
;(define-core-type define-global! name value)
;(define-core-type atomic value)
;(define-core-type ref name)
;(define-core-type apply proc args)

(define-record-type <core::let-var>
  (core::let-var name value)
  core::let-var?
  (name core::let-var-name)
  (value core::let-var-value))

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

(define-record-type <core::constant>
  (core::constant value)
  core::constant?
  (value core::constant-value))

(define-record-type <core::ref>
  (core::ref name)
  core::ref?
  (name core::ref-name))
(define-syntax core::anon-ref
  (syntax-rules ()
    ((_ id)
     (core::ref 'id))))

(define-record-type <core::apply>
  (core::apply proc args)
  core::apply?
  (proc core::apply-proc)
  (args core::apply-args))
(define-syntax core::apply-anon
  (syntax-rules ()
    ((_ id args ...)
      (core::apply (core::anon-ref id) (list args ...)))))

(define-record-type <core::letrec>
  (core::letrec vars body)
  core::letrec?
  (vars core::letrec-vars)
  (body core::letrec-body))

(define-record-type <core::module>
  (make-core::module name type envs exports imports builds syntax-defs bound-vars forms build)
  core::module?
  (name          core::module-name)
  (type          core::module-type)
  (envs          core::module-envs)
  (exports       core::module-exports)
  (imports       core::module-imports)
  (builds        core::module-builds)
  (syntax-defs   core::module-syntax-defs)
  (bound-vars    core::module-bound-vars)
  (forms         core::module-forms)
  (build         core::module-build))

(define (core::atomic? term)
  (or (is-a? term <core::constant>)
      (is-a? term <core::ref>)
      (is-a? term <core::lambda>)))

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
    (($ <core::define-global!> name value) `(define ,(core::ref-name name) ,(compile-term value)))
    (($ <core::constant> value) `(quote ,value))
    (($ <core::ref> name) name)
    (($ <core::apply> proc args) `(,(compile-term proc) ,@(map compile-term args)))))

(define (compile-core-to-host-scheme terms)
  (map compile-term terms))

(define core::serialize compile-term)

))
