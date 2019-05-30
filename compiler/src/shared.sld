(define-library 
    (shared)
    (import (scheme base))
    (import (scheme write))
    (export 
        make-source-location
        source-location->line
        source-location->col
        make-token
        token?
        token->string
        token->type
        token->value
        token->location)
(begin

(define-record-type <source-location>
    (make-source-location line col)
    source-location?
    (line source-location->line)
    (col  source-location->col))

(define-record-type <token>
    (make-token string type value location)
    token?
    (string token->string)
    (type token->type)
    (value token->value)
    (location token->location))

))