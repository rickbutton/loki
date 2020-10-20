(define-library (loki cli)
(import (scheme base))
(import (scheme write))
(import (scheme process-context))
(import (loki util))
(import (loki message))
(import (prefix (loki expander) ex:))
(import (loki compiler))
(import (loki runtime))
(import (loki path))
(import (srfi 37))
(export run-loki-cli *version*)
(begin

(define *version* "0.0.1")
(define (version-string)
  (string-append
    "loki v" *version* "\n"))

(define-record-type <loki-options>
  (make-loki-options targets)
  loki-options?
  (targets loki-options-targets loki-options-targets-set!))
(define (default-options)
  (make-loki-options '()))

(define (display-and-exit-proc msg)
  (lambda (opt name arg options)
    (display msg)
    (exit 0)))

(define (parse-options arguments)
  (args-fold
    arguments
    (list (option '(#\v "version") #f #f
                  (display-and-exit-proc (version-string)))
          (option '(#\h "help") #f #f
                  (display-and-exit-proc
                   "Usage: TODO, someone please fill this in...")))
    (lambda (opt name arg loads)
      (error "Unrecognized option" name))
    (lambda (op options)
      (loki-options-targets-set! options
        (cons op (loki-options-targets options)))
      options)
    (default-options)))

(define (run-loki-cli arguments)
      (let ((options (parse-options arguments)))
        (if (null? options)
          (error "target required"))
        (for-each
          (lambda (target)
            (debug "running" target)
            (rt:import-library (rt:library-name (ex:expand-file (make-path target)))))
          (loki-options-targets options))))

))
