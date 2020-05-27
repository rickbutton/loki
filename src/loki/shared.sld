(define-library 
    (loki shared)
    (import (scheme base))
    (import (scheme write))
    (import (srfi 69))
    (import (loki util))
    (import (loki compat))
    (export 
        make-comment
        comment?
        comment->text

        make-source-location
        source-location->string

        raise-loki-error
        raise-loki-warning
        make-loki-message
        loki-message?
        loki-message->type
        loki-message->location
        loki-message->message)
(begin

(define-record-type <source-location>
    (make-source-location path line column)
    source-location?
    (path source-location->path)
    (line source-location->line)
    (column  source-location->column))
(define (source-location->string l)
    (string-append 
        (source-location->path l)
        " ["
        (number->string (source-location->line l))
        ":"
        (number->string (source-location->column l))
        "]"))

(define-record-type <comment>
    (make-comment text)
    comment?
    (text comment->text))
(type-printer-set! <comment> 
    (lambda (x out) 
        (display (string-append "(;" (comment->text x) ";)") out)))

(define-record-type <loki-message>
    (make-loki-message type location message)
    loki-message?
    (type loki-message->type)
    (location loki-message->location)
    (message loki-message->message))

(define (raise-loki-error location message)
        (raise (make-loki-message 'error location message)))

(define (raise-loki-warning location message)
        (raise (make-loki-message 'warning location message)))
))
