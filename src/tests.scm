(import (scheme base))
(import (scheme write))
(import (tests srfi 151))
(import (tests srfi 64))
(import (srfi 64))
;(import (tests loki path))

(display "loki tests!\n")
(test-begin "loki")

(let ((runner (test-runner-current)))
  (run-tests-srfi-151 runner)
  (run-tests-srfi-64 runner))

(test-end "loki")
