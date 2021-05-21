(define-library (loki core printer)
  (import (scheme base))
  (import (scheme case-lambda))
  (cond-expand
   (loki
    (import (loki core intrinsics))
    (import (loki core records))
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
         
         (write-string "#" port)
         (writer (record-type-name type) port)
         (if (= (length tags) 0)
             (write-string "{}" port)
           (begin
            (write-string "#{ " port)
            (for-each (lambda (tag)
                        (writer tag port)
                        (write-string ": " port)
                        (writer ((record-accessor type tag) obj) port)
                        (write-char #\space port)) tags)
            (write-char #\} port)))))
     
     (define (print-object obj writer port)
       (cond
        ((record? obj) ((or (record-printer obj) default-record-printer) obj writer port))
        ((error-object? obj)
         (write-string "ERROR: " port)
         (writer (error-object-message obj) port)
         (write-string "\n" port))
        (else (write-string "#<unknown>" port)))))
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
                        (define (writer x . o)
                          (let ((write? (if (pair? o) (car o) #f)))
                            (if write?
                                (write x port)
                              (display x port))))
                        (printer self writer port)))))))
   
   ))
