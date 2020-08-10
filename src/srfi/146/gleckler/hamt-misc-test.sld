(define-library (srfi 146 gleckler hamt-misc-test)
  (import (scheme base) (loki test) (srfi 146 gleckler hamt-misc))
  (export run-hamt-misc-tests)
  (include "hamt-misc-test.scm"))
