(define-library (loki core let)
  (export let letrec letrec*)
  (import (for (except (loki core primitives) _)        expand run)
          (for (loki core intrinsics)        expand run)
          (for (loki core syntax-rules)      expand run))
  (begin
   
   (define-syntax let
     (syntax-rules ()
                   ((let ((name val) ...) body1 body2 ...)
                    (primitive-let ((name val) ...) body1 body2 ...))
                   ((let tag ((name val) ...) body1 body2 ...)
                    (let ()
                         (define tag (lambda (name ...) body1 body2 ...))
                         (tag val ...)))))
   
   (define-syntax letrec
     (syntax-rules ()
                   ((letrec ((var1 init1) ...) body ...)
                    (letrec "generate_temp_names"
                            (var1 ...)
                            ()
                            ((var1 init1) ...)
                            body ...))
                   ((letrec "generate_temp_names"
                            ()
                            (temp1 ...)
                            ((var1 init1) ...)
                            body ...)
                    (let ((var1 %void) ...)
                         (let ((temp1 init1) ...)
                              (set! var1 temp1)
                              ...
                              (let () body ...))))
                   ((letrec "generate_temp_names"
                            (x y ...)
                            (temp ...)
                            ((var1 init1) ...)
                            body ...)
                    (letrec "generate_temp_names"
                            (y ...)
                            (newtemp temp ...)
                            ((var1 init1) ...)
                            body ...))))
   
   (define-syntax letrec*
     (syntax-rules ()
                   ((letrec* ((var1 init1) ...) body1 body2 ...)
                    (let ((var1 %void) ...)
                         (set! var1 init1)
                         ...
                         (let () body1 body2 ...)))))
   
   )) ; let

