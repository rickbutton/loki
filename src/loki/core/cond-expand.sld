(define-library (loki core cond-expand)
(import (for (loki core primitives)       expand run)
        (for (loki core let)              expand run)
        (for (loki core list)             expand run)
        (for (loki core intrinsics)       expand run)
        (for (loki core bool)             expand run)
        (for (loki core derived)          expand run))
(export cond-expand)
(begin

; TODO - support (library (library-name)) syntax
; for detecting if library exists
(define-syntax cond-expand
  (lambda (x)
    (let ((current-features (features)))
      (syntax-case x (and or not else)
        ((_) #f)
        ((_ (else body ...)) (syntax (begin body ...)))
        ((_ ((and) body ...) more ...) (syntax (begin body ...)))
        ((_ ((and req1 req2 ...) body ...) more ...)
          (syntax (cond-expand
            (req1
              (cond-expand
                ((and req2 ...) body ...)
                more ...))
            more ...)))
         ((_ ((or) body ...) more ...)
           (syntax (cond-expand more ...)))
         ((_ ((or req1 req2 ...) body ...) more ...)
           (syntax (cond-expand
             (req1
              (begin body ...))
             (else
              (cond-expand
                 ((or req2 ...) body ...)
                 more ...)))))
         ((_ ((not req) body ...) more ...)
           (syntax (cond-expand
             (req
               (_ more ...))
             (else body ...))))
         ((_ (feature-id body ...) more ...)
           (let ((feature (syntax->datum (syntax feature-id))))
             (if (and (symbol? feature)
                      (member feature current-features))
               (syntax (begin body ...))
               (syntax (cond-expand more ...)))))))))
))
