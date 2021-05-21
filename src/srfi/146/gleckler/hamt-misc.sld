(define-library (srfi 146 gleckler hamt-misc)
  (import (scheme base)
          (scheme case-lambda)
          (only (srfi 69) make-hash-table string-hash)
          (only (srfi 128) make-comparator))
  (export assert do-list
          make-string-hash-table
          with-output-to-string)
  (include "hamt-misc.scm"))
