(define-library (tests util)
(export define-tests test)
(import (for (core primitives) expand))
(import (for (core quasisyntax) expand))
(import (for (loki util) expand))
(import (scheme base))
(import (scheme write))
(import (scheme case-lambda))
(import (srfi 64))
(begin
(define-syntax define-tests
  (syntax-rules ()
    ((_ proc-name suite-name form ...)
     (define proc-name
       (case-lambda
         (() (proc-name (test-runner-create)))
         ((runner)
          (parameterize ((test-runner-current runner))
            (test-begin suite-name)
            form ...
            (test-end suite-name)
            (and (= 0 (test-runner-xpass-count runner))
                 (= 0 (test-runner-fail-count runner))))))))))
(define-syntax test
  (lambda (x)
    (syntax-case x ()
      ((test expected expr)
        (let ((name (write-to-string (syntax->datum (syntax expected)))))
          (quasisyntax (test-equal (unsyntax name) expected expr)))))))

))
