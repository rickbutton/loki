(define-library (loki printer)
(import (scheme base))
(import (scheme case-lambda))
(cond-expand
  (loki
    (import (core intrinsics))
    (import (core records))
    (export print-object))
  (gauche (import (gauche base))
          (import (scheme write))))
(export type-printer-set!)
(begin

(cond-expand
  (loki
    (define (default-record-printer obj writer port)
      (let* ((type (record-type obj))
             (tags (record-type-field-tags type)))
        (writer (record-type-name type) port)
        (if (= (length tags) 0)
          (write-string "#{}" port)
          (begin
            (write-string "#{ " port)
            (for-each (lambda (tag)
              (writer tag port)
              (write-string ": " port)
              (writer ((record-accessor type tag) obj) port)
              (write-char #\space port)) tags)
            (write-char #\} port)))))
 
    (define (print-object obj writer port)
      (if (record? obj)
        (or (record-printer obj)
            default-record-printer)
        (write-string (%repr obj) port))))
  (else #f))

(cond-expand
  (loki
    (define (type-printer-set! type printer)
      (record-type-printer-set! type printer)))
  (gauche
    (define-syntax type-printer-set!
      (syntax-rules ()
        ((_ type printer)
         (define-method write-object ((self type) port)
            (define (writer x . _) (write x port))
            (printer self writer port)))))))

))
