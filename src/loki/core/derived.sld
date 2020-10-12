(define-library (loki core derived)
  (export let* cond case else =>)   
  (import (for (loki core primitives)       expand run)
          (for (loki core let)              expand run)
          (for (loki core with-syntax)      expand)
          (for (loki core syntax-rules)     expand)
          (for (loki core list)             expand run)
          (for (loki core intrinsics)       expand run))
  (begin
  
  
  (define-syntax let*
    (syntax-rules ()
      ((let* () body1 body2 ...)
       (let () body1 body2 ...))
      ((let* ((name1 val1) (name2 val2) ...)
         body1 body2 ...)
       (let ((name1 val1))
         (let* ((name2 val2) ...)
           body1 body2 ...)))))

  
  (define-syntax cond
    (syntax-rules (else =>)
      ((cond (else result1 result2 ...))
       (begin result1 result2 ...))
      ((cond (test => result))
       (let ((temp test))
         (if temp (result temp))))
      ((cond (test => result) clause1 clause2 ...)
       (let ((temp test))
         (if temp
             (result temp)
             (cond clause1 clause2 ...))))
      ((cond (test)) test)
      ((cond (test) clause1 clause2 ...)
       (let ((temp test))
         (if temp
             temp
             (cond clause1 clause2 ...))))
      ((cond (test result1 result2 ...))
       (if test (begin result1 result2 ...)))
      ((cond (test result1 result2 ...)
             clause1 clause2 ...)
       (if test
           (begin result1 result2 ...)
           (cond clause1 clause2 ...)))))

  (define-syntax case
   (syntax-rules (else =>)
     ((case (key ...)
        clauses ...)
      (let ((atom-key (key ...)))
        (case atom-key clauses ...)))
     ((case key
        (else => result))
      (result key))
     ((case key
        (else result1 result2 ...))
      (begin result1 result2 ...))
     ((case key
        ((atoms ...) => result))
      (if (memv key '(atoms ...))
          (result key)))
     ((case key
        ((atoms ...) => result)
        clause clauses ...)
      (if (memv key '(atoms ...))
          (result key)
          (case key clause clauses ...)))
     ((case key
        ((atoms ...) result1 result2 ...))
      (if (memv key '(atoms ...))
          (begin result1 result2 ...)))
     ((case key
        ((atoms ...) result1 result2 ...)
        clause clauses ...)
      (if (memv key '(atoms ...))
          (begin result1 result2 ...)
          (case key clause clauses ...)))))

  (define-syntax =>
    (lambda (x)
      (syntax-violation '=> "Invalid expression" x)))
  
  (define-syntax else
    (lambda (x)
      (syntax-violation 'else "Invalid expression" x)))
  
  )) ; derived

