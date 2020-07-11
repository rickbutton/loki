(import (scheme base))
(import (scheme process-context))
(import (loki cli))

(run-loki-cli (cdr (command-line)))
