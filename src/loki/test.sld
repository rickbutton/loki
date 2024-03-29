(define-library (loki test)
  (import (scheme base))
  (import (scheme write))
  (import (scheme case-lambda))
  (import (loki syntax))
  (import (srfi 64))
  (import (srfi 64 execution))
  (import (srfi 128))
  (export
   ;; Execution
   test-begin test-end test-group test-group-with-cleanup
   
   test-skip test-expect-fail
   test-match-name test-match-nth
   test-match-all test-match-any
   
   test-assert test-eqv test-eq test-equal test-approximate
   test-error test-read-eval-string
   
   test-apply test-with-runner
   
   test-exit
   
   ;; Test runner
   test-runner-null test-runner? test-runner-reset
   
   test-result-alist test-result-alist!
   test-result-ref test-result-set!
   test-result-remove test-result-clear
   
   test-runner-pass-count
   test-runner-fail-count
   test-runner-xpass-count
   test-runner-xfail-count
   test-runner-skip-count
   
   test-runner-test-name
   
   test-runner-group-path
   test-runner-group-stack
   
   test-runner-aux-value test-runner-aux-value!
   
   test-result-kind test-passed?
   
   test-runner-on-test-begin test-runner-on-test-begin!
   test-runner-on-test-end test-runner-on-test-end!
   test-runner-on-group-begin test-runner-on-group-begin!
   test-runner-on-group-end test-runner-on-group-end!
   test-runner-on-final test-runner-on-final!
   test-runner-on-bad-count test-runner-on-bad-count!
   test-runner-on-bad-end-name test-runner-on-bad-end-name!
   
   test-runner-factory test-runner-create
   test-runner-current test-runner-get
   
   ;; Simple test runner
   test-runner-simple
   test-on-group-begin-simple test-on-group-end-simple test-on-final-simple
   test-on-test-begin-simple test-on-test-end-simple
   test-on-bad-count-simple test-on-bad-end-name-simple
   
   ;; for (loki test)
   current-test-comparator
   define-tests
   test)
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
                                      (let () form ...)
                                      (test-end suite-name)
                                      (and (= 0 (test-runner-xpass-count runner))
                                           (= 0 (test-runner-fail-count runner))))))))))
   (define-syntax test
     (lambda (x)
       (syntax-case x ()
                    ((test name expected expr)
                     (quasisyntax (test-equal name expected expr)))
                    ((test expected expr)
                     (let ((out (open-output-string)))
                       (write (syntax->datum (syntax expected)) out)
                       (quasisyntax (test-equal (unsyntax (get-output-string out)) expected expr)))))))))

