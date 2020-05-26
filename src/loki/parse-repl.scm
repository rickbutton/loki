(import (scheme base))
(import (scheme read))
(import (scheme write))
(import (scheme repl))

(import (loki util))
(import (loki shared))
(import (loki p00_string2tokens))
(import (loki p01_tokens2syntax))
(import (loki p02_attrs))

(import (srfi 159))
(import (chibi show pretty))

(define (read-as-string)
    (show #f (read)))

(define (handle-compile-error e)
    (let ((location (compile-error->location e))
          (message (compile-error->message e)))
        (display "compile error at ")
        (display (source-location->string location))
        (display ": ")
        (display message)
        (display "\n")
        (display "\n")
        (repl)))

(define (handle-unexpected-error e)
    (display "unexpected error in compiler: ")
    (display (show #f (pretty e)))
    (raise e)
    (exit 1))

(define (handle-error e)
    (if (compile-error? e)
        (handle-compile-error e)
        (handle-unexpected-error e)))

(define (repl)
    (display ">")
    (let ((prog (read-as-string)))
       (with-exception-handler handle-error
            (lambda ()
                (display (show #f (pretty
                            (p02_attrs 
                            (p01_tokens2syntax 
                            (p00_string2tokens (open-input-string prog)))))))
                (newline)
                (repl)))))

(repl)
