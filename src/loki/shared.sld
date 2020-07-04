(define-library 
    (loki shared)
    (import (scheme base))
    (import (scheme write))
    (import (scheme process-context))
    (import (srfi 69))
    (import (loki util))
    (import (loki host))
    (import (core reader))
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
