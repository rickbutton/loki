(define-library (loki printer)
(import (scheme base))
(import (scheme case-lambda))
(cond-expand
  (chibi
    (import (scheme write) (rename (chibi ast) (type-printer-set! chibi-type-printer-set!))))
  (loki
    (import (core intrinsics))
    (import (core records))
    (export print-object)))
(export type-printer-set!)
(begin

(cond-expand (loki
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
      (write-string (%repr obj) port)))))

(define (type-printer-set! type printer)
  (cond-expand
    (chibi
      (chibi-type-printer-set! type
                               (lambda (x writer out)
                                 (define (writer2 x . opt)
                                   (if (or (null? opt) (not (car opt)))
                                       (writer x)
                                       (display x out)))
                                 (printer x writer2 out))))
    (loki (record-type-printer-set! type printer))))


))
