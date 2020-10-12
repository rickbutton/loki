(define-library (scheme process-context)
    (import (loki core process))
    (export command-line
            exit
            get-environment-variable
            get-environment-variables
            emergency-exit))

