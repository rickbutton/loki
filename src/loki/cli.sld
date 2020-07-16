(define-library (loki cli)
(import (scheme base))
(import (scheme write))
(import (scheme process-context))
(import (loki util))
(import (loki message))
(import (loki expander))
(import (loki compiler))
(import (loki runtime))
(import (loki path))
(import (srfi 37))
(cond-expand
  (chibi (import (chibi ast)))
  (loki (import (core exception))))
(export run-loki-cli)
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

(define (emit-library library invoke?)
  (display "emiting library")
  (display (rt:library-name library))
  (display "!\n")
  (when invoke?
    (compile library)
    (rt:import-library (rt:library-name library))))

;; Load the r7rs standard library into the expander
(define (load-stdlib)
  (debug "expanding r7rs/lang.scm")
  (ex:expand-file (make-path "src/r7rs/lang.scm") emit-library))

(define (run-loki-cli arguments)
  (with-exception-handler
    (lambda (err)
      (display err)
      (exit 1))
    (lambda ()
      (let ((options (parse-options arguments)))
        (if (null? options)
          (error "target required"))
        (load-stdlib)
        (for-each
          (lambda (target)
            (ex:expand-file (make-path target) emit-library))
          (loki-options-targets options))))))

))
