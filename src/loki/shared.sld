(define-library 
    (loki shared)
    (import (scheme base))
    (import (scheme process-context))
    (import (scheme write))
    (import (srfi 69))
    (import (loki util))
    (import (loki printer))
    (export 
        make-comment
        comment?
        comment->text)
(begin

(define-record-type <comment>
    (make-comment text)
    comment?
    (text comment->text))

(type-printer-set! <comment> 
    (lambda (x out) 
        (display (string-append "(;" (comment->text x) ";)") out)))

))
