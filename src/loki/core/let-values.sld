(define-library (loki core let-values)
  (export let-values let*-values define-values)
  (import (for (loki core primitives)   expand run)
          (for (loki core syntax-rules) expand run)
          (for (loki core values)       expand run)
          (for (loki core let)          expand run)
          (for (loki core list)         expand run)
          (for (loki core intrinsics)   expand run))
  (begin
   
   (define-syntax let-values
     (syntax-rules ()
                   ((let-values (?binding ...) ?body0 ?body1 ...)
                    (let-values "bind" (?binding ...) () (begin ?body0 ?body1 ...)))
                   
                   ((let-values "bind" () ?tmps ?body)
                    (let ?tmps ?body))
                   
                   ((let-values "bind" ((?b0 ?e0) ?binding ...) ?tmps ?body)
                    (let-values "mktmp" ?b0 ?e0 () (?binding ...) ?tmps ?body))
                   
                   ((let-values "mktmp" () ?e0 ?args ?bindings ?tmps ?body)
                    (call-with-values
                     (lambda () ?e0)
                     (lambda ?args
                             (let-values "bind" ?bindings ?tmps ?body))))
                   
                   ((let-values "mktmp" (?a . ?b) ?e0 (?arg ...) ?bindings (?tmp ...) ?body)
                    (let-values "mktmp" ?b ?e0 (?arg ... x) ?bindings (?tmp ... (?a x)) ?body))
                   
                   ((let-values "mktmp" ?a ?e0 (?arg ...) ?bindings (?tmp ...) ?body)
                    (call-with-values
                     (lambda () ?e0)
                     (lambda (?arg ... . x)
                             (let-values "bind" ?bindings (?tmp ... (?a x)) ?body))))))
   
   (define-syntax let*-values
     (syntax-rules ()
                   ((let*-values () ?body0 ?body1 ...)
                    (let () ?body0 ?body1 ...))
                   
                   ((let*-values (?binding0 ?binding1 ...) ?body0 ?body1 ...)
                    (let-values (?binding0)
                                (let*-values (?binding1 ...) ?body0 ?body1 ...)))))
   
   
   (define-syntax define-values
     (syntax-rules ()
                   ((define-values () expr)
                    (define dummy
                      (call-with-values (lambda () expr)
                                        (lambda args #f))))
                   ((define-values (var) expr)
                    (define var expr))
                   ((define-values (var0 var1 ... varn) expr)
                    (begin
                     (define var0
                       (call-with-values (lambda () expr)
                                         list))
                     (define var1
                       (let ((v (cadr var0)))
                            (set-cdr! var0 (cddr var0))
                            v)) ...
                     (define varn
                       (let ((v (cadr var0)))
                            (set! var0 (car var0))
                            v))))
                   ((define-values (var0 var1 ... . varn) expr)
                    (begin
                     (define var0
                       (call-with-values (lambda () expr)
                                         list))
                     (define var1
                       (let ((v (cadr var0)))
                            (set-cdr! var0 (cddr var0))
                            v)) ...
                     (define varn
                       (let ((v (cdr var0)))
                            (set! var0 (car var0))
                            v))))
                   ((define-values var expr)
                    (define var
                      (call-with-values (lambda () expr)
                                        list)))))
   
   )) ; core let-values
