(define-library (srfi 64 source-info)
  (import (scheme base))
  (import (for (loki path) expand))
  (import (srfi 64 test-runner))
  (import (for (loki core primitives) run expand))
  (import (for (loki core quasisyntax) expand))
  (export source-info set-source-info!)
  (include "source-info.body.scm"))
