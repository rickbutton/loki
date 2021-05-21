(import (scheme base))
(import (scheme write))
(import (scheme process-context))
(import (loki util))
(import (prefix (loki compiler expander) ex:))
(import (loki path))
(import (loki compiler loader))
(import (loki compiler runtime))
(import (loki compiler lang core))
(import (srfi 37))

(define *version* "0.0.1")
(define (version-string)
  (string-append
    "loki v" *version* "\n"))

(define-record-type <loki-options>
  (make-loki-options targets args)
  loki-options?
  (targets loki-options-targets loki-options-targets-set!)
  (args    loki-options-args    loki-options-args-set!))
(define (default-options)
  (make-loki-options '() '()))

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
    (let ((mode 'targets)) ; targets | options
      (lambda (op options)
        (if (string=? op "--")
          (set! mode 'args)
          (case mode
            ((targets)
             (loki-options-targets-set! options (append (loki-options-targets options) (list op))))
            ((args)
             (loki-options-args-set! options (append (loki-options-args options) (list op))))))
          options))
    (default-options)))

(define (run-loki-cli arguments)
  (let ((options (parse-options arguments)))
    (if (null? options)
      (error "target required"))
    (with-loki-command-line (loki-options-args options) (lambda ()
      (for-each
        (lambda (target)
          (import-module (core::module-name (ex:expand-file (make-path target)))))
        (loki-options-targets options))))))

(with-exception-handler
  (lambda (error)
    (if (runtime-exception? error)
        (debug "runtime exception"
               (runtime-exception-type error)
               (runtime-exception-message error)
               (runtime-exception-irritants error))
        (debug error))
    (raise error))
  (lambda () (run-loki-cli (cdr (command-line)))))
