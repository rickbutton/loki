(define-library 
    (shared)
    (import (scheme base))
    (import (scheme write))
    (export 
        make-source-location
        source-location->line
        source-location->col)
(begin

(define-record-type <source-location>
    (make-source-location line col)
    source-location?
    (line source-location->line)
    (col  source-location->col))

))