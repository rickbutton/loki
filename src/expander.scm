(import (scheme base))
(import (scheme eval))
(import (scheme write))
(import (loki expander))
(import (loki runtime))
(import (loki shared))
(import (loki util))

(define form 
'(define-library (my)
    (export a)
    (import (scheme base))
    (import (scheme write))
    (begin
      (define a 3)
      (display "abolish the nypd!\nfuck the police.\n"))))

(define form2
`(define-library (my2)
    (export c)
    (import (scheme base))
    (import (scheme write))
    (import (loki expander))
    (import (loki runtime))
    (begin
      (define c 10)
      (display "hello from the second compiler...\n\n")
      (ex:expand-file "src/loki/r7rs.scm")
      (ex:expand-datum-sequence ',(list form))
      (ex:import-library '(my))
    )))

(with-loki-error-handler (lambda ()
  (display "hello from the first compiler...\n")
  (ex:expand-file "src/loki/r7rs.scm")
  (ex:expand-datum-sequence (list form2))
  (ex:import-library '(my2))))
  
