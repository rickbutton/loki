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
          core::apply-prim
          core::module
          core::module-name
          core::module-envs
          core::module-exports
          core::module-imports
          core::module-imported-libraries
          core::module-builds
          core::module-syntax-defs
          core::module-forms
          core::module-build
          core::module-visited?
          core::module-invoked?
          core::module-visited?-set!
          core::module-invoked?-set!
          core::atomic?
          core::module->scheme
          core::serialize
          core::deserialize)
  (begin
   
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
   
   ; module ref
   ; toplevel ref?
   ; local ref
   ; free ref
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
   (define-syntax core::apply-prim
     (syntax-rules ()
                   ((_ id args ...)
                    (core::apply (core::anon-ref id) (list args ...)))))
   
   (define-record-type <core::letrec>
     (core::letrec vars body)
     core::letrec?
     (vars core::letrec-vars)
     (body core::letrec-body))
   
   (define-record-type <core::module>
     (make-core::module name envs exports imports imported-libraries builds syntax-defs forms build visited? invoked?)
     module?
     ; (symbol ...)
     (name        core::module-name)
     (envs        core::module-envs)
     (exports     core::module-exports)
     ; ((name module binding) ...)
     (imports     core::module-imports)
     (imported-libraries core::module-imported-libraries)
     ; (build-id ...)
     (builds      core::module-builds)
     (syntax-defs core::module-syntax-defs)
     ; (core ...)
     (forms       core::module-forms)
     ; build-id
     (build       core::module-build)
     (visited?    core::module-visited? core::module-visited?-set!)
     (invoked?    core::module-invoked? core::module-invoked?-set!))
   (define (core::module name envs exports imports imported-libraries builds syntax-defs forms build)
     (make-core::module name envs exports imports imported-libraries builds syntax-defs forms build #f #f))
   
   (define (core::atomic? term)
     (or (is-a? term <core::constant>)
         (is-a? term <core::ref>)
         (is-a? term <core::lambda>)))
   
   (define (core::serialize term)
     (match term
       (($ <core::letrec> vars body)
        `(letrec ,(map (lambda (var) (list (core::ref-name (core::let-var-name var)) (core::serialize (core::let-var-value var)))) vars)
          ,@(map core::serialize body)))
       (($ <core::if> exp1 exp2 exp3) `(if ,(core::serialize exp1) ,(core::serialize exp2) ,(core::serialize exp3)))
       (($ <core::lambda> formals rest body)
        (let ((formals (map core::ref-name formals))
              (rest (if rest (core::ref-name rest) #f)))
          `(lambda ,(if rest (if (pair? formals) (apply cons* (append formals (list rest))) rest) formals) ,@(map core::serialize body))))
       (($ <core::set!> name value) `(set! ,(core::ref-name name) ,(core::serialize value)))
       (($ <core::define-global!> name value) `(define ,(core::ref-name name) ,(core::serialize value)))
       (($ <core::constant> value) `(quote ,value))
       (($ <core::ref> name) name)
       (($ <core::apply> proc args) `(,(core::serialize proc) ,@(map core::serialize args)))))
   
   (define (core::module->scheme module)
     (map core::serialize (core::module-forms module)))
   
   (define (core::deserialize term)
     (match term
       (('letrec vars body ...)
        (core::letrec (map (lambda (v) (core::let-var (core::deserialize (car v)) (core::deserialize (cadr v)))) vars)
                      (map core::deserialize body)))
       (('if exp1 exp2 exp3)
        (core::if (core::deserialize exp1) (core::deserialize exp2) (core::deserialize exp3)))
       (('lambda formals body ...)
        (let-values (((formals rest) (match formals
                                       (formal (values '() formal))
                                       ((formals ...) (values formals #f))
                                       ((formals . rest) (values formals rest)))))
          (core::lambda (map core::ref formals) (if rest (core::ref rest) #f) (map core::deserialize body))))
       (('set! name value) (core::set! (core::ref name) (core::deserialize value)))
       (('define name value) (core::define-global! (core::ref name) (core::deserialize value)))
       (('quote value) (core::constant value))
       ((? symbol? name) (core::ref name))
       ((proc args ...)
        (core::apply (core::deserialize proc) (map core::deserialize args)))))
   
   
   ))
