(define-library (srfi 146 gleckler vector-edit-test)
  (import (scheme base) (loki test) (srfi 146 gleckler vector-edit))
  (export run-vector-edit-tests)
  (include "vector-edit-test.scm"))
