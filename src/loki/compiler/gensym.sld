(define-library (loki compiler gensym)
(import (scheme base))
(import (scheme write))
(import (srfi 69))
(export make-gensym)
(begin

(define (make-gensym)
  (let ((table (make-hash-table)))
    (lambda (id)
      (unless (symbol? id)
        (error "invalid argument to gensym" id))
      (let ((value (hash-table-ref/default table id #f)))
        (if value
            (hash-table-set! table id (+ value 1))
            (hash-table-set! table id 0))
        (let ((out (open-output-string)))
          (write id out)
          (write (or value 0) out)
          (string->symbol (get-output-string out)))))))

))
