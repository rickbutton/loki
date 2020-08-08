(import (scheme base))
(import (scheme write))
(import (scheme load))
(import (srfi 64))

(import (rename (srfi 31 test) (run-tests run-tests-srfi-31)))
(import (rename (srfi 64 test) (run-tests run-tests-srfi-64)))
(import (rename (srfi 95 test) (run-tests run-tests-srfi-95)))
(import (rename (srfi 113 test) (run-tests run-tests-srfi-113)))
(import (rename (srfi 121 test) (run-tests run-tests-srfi-121)))
(import (rename (srfi 128 test) (run-tests run-tests-srfi-128)))
(import (rename (srfi 151 test) (run-tests run-tests-srfi-151)))

(display "loki tests!\n")
(test-begin "loki")

(let ((runner (test-runner-current)))
  (run-tests-srfi-31 runner)
  (run-tests-srfi-64 runner)
  (run-tests-srfi-95 runner)
  (run-tests-srfi-113 runner)
  (run-tests-srfi-121 runner)
  ;(run-tests-srfi-125 runner)
  (run-tests-srfi-128 runner)
  (run-tests-srfi-151 runner)

  ; r7rs test suite requires a toplevel
  ; TODO - this should be a relative path
  ; but because load is a primitive, not a macro
  ; we can't do the normal path resolution based on
  ; the identifier's source location
  ; instead, we will need to remember the current file
  ; during expansion and use that instead, which sucks
  (load "src/lang/r7rs-tests.scm"))

(test-end "loki")
