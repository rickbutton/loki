(define-library (scheme file)
  (import (loki core io))
  (export call-with-input-file
          delete-file
          open-binary-input-file
          open-input-file
          with-input-from-file
          call-with-output-file
          file-exists?
          open-binary-output-file
          open-output-file
          with-output-to-file))

