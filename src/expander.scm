(import (scheme base))
(import (scheme eval))
(import (scheme write))
(import (loki expander))
(import (loki runtime))
(import (loki shared))
(import (loki util))

(define form '(
  (import (scheme base))
  (import (scheme write))
  (display "abolish the nypd!\nfuck the police.\n")))

(define inside2
`(define-library (inside2)
    (import (scheme base))
    (import (scheme write))
    (import (scheme eval))
    (import (loki expander))
    (import (loki runtime))
    (begin
      (display "hello from the third compiler...\n\n")
      (map ex:runtime-eval (ex:expand-datum-sequence ',form))
)))


(define inside
`(define-library (inside)
    (import (scheme base))
    (import (scheme write))
    (import (scheme eval))
    (import (loki expander))
    (import (loki runtime))
    (begin
      (display "hello from the second compiler...\n\n")
      (ex:expand-datum-sequence (list ',inside2))
      (ex:import-library '(inside2))
)))

(with-loki-error-handler (lambda ()
  (display "hello from the first compiler...\n")
  (ex:expand-datum-sequence (list inside))
  (ex:import-library '(inside))))
