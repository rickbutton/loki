(import (scheme base))
(import (scheme write))
(import (scheme load))
(import (tests srfi 31))
(import (tests srfi 64))
(import (tests srfi 121))
(import (tests srfi 151))
(import (srfi 64))

(display "loki tests!\n")
(test-begin "loki")


(let ((runner (test-runner-current)))
  ; r7rs test suite requires a toplevel
  ; TODO - this should be a relative path
  ; but because load is a primitive, not a macro
  ; we can't do the normal path resolution based on
  ; the identifier's source location
  ; instead, we will need to remember the current file
  ; during expansion and use that instead, which sucks
  (load "src/tests/lang/r7rs.scm")
  (run-tests-srfi-31 runner)
  (run-tests-srfi-64 runner)
  (run-tests-srfi-121 runner)
  (run-tests-srfi-151 runner))

(test-end "loki")
