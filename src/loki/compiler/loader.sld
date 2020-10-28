(define-library (loki compiler loader)
(import (scheme base))
(import (loki reader))
(import (loki runtime))
(import (loki compiler util))
(import (loki compiler environment))
(import (loki compiler macro))
(export current-builds
        import-libraries-for-expand
        evaluate-macro)
(begin

(define (current-builds imported-libraries)
  (map (lambda (lib-entry)
         (rt:library-build (rt:lookup-library (car lib-entry))))
       imported-libraries))

;; Register macros in library
(define (visit-library! library)
  (for-all (lambda (def)
             (let ((name (car def)) (macro (cdr def)))
                (if (macro? macro)
                  (register-macro! name macro)
                  (register-macro! name (make-transformer (evaluate-macro macro))))))
           (rt:library-syntax-defs library)))

(define (import-libraries-for-expand imports builds phase)
  (rt:import-libraries-for
   imports
   builds
   phase
   (lambda (library phase imported)
     (if (and (>= phase 0)
              (not (rt:library-visited? library)))
         (begin
           (load-reified-env-table (rt:library-envs library))
           (visit-library! library)
           (rt:library-visited?-set! library #t)))
     (if (and (>= phase 1)
              (not (rt:library-invoked? library)))
         (begin 
           (rt:invoke-library! library)
           (rt:library-invoked?-set! library #t))))
   'expand))

(define (evaluate-macro module)
  (rt:register-library! module)
  (rt:library-visited?-set! module #f)
  (rt:library-invoked?-set! module #f)
  (rt:import-library (rt:library-name module)))

))
