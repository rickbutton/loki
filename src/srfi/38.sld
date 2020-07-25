(define-library (srfi 38)
(import (scheme read))
(import (scheme write))
(export 
        (rename write-shared write-with-shared-structure)
        (rename write-shared write/ss)
        (rename read read-with-shared-structure)
        (rename read/ss)))
