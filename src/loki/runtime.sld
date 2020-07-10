(define-library (loki runtime)
(import (scheme base))
(import (scheme write))
(import (scheme eval))
(import (scheme char))
(import (scheme inexact))
(import (scheme file))
(import (scheme time))
(import (scheme process-context))
(import (loki util))
(import (loki shared))
(import (loki host))
(import (loki reader))
(import (srfi 69))

(export ex:library-dirs
        ex:map-while
        ex:make-library
        ex:library-name
        ex:library-envs
        ex:library-exports
        ex:library-imports
        ex:library-builds
        ex:library-syntax-defs
        ex:library-bound-vars
        ex:library-forms
        ex:library-build
        ex:library-visited?
        ex:library-invoked?
        ex:library-visited?-set!
        ex:library-invoked?-set!
        ex:import-libraries-for
        ex:import-libraries-for-run
        ex:import-library
        ex:register-library!
        ex:invoke-library!
        ex:lookup-library
        ex:lookup-library/false
        ex:runtime-add-primitive
        ex:runtime-eval)
(begin

(define ex:library-dirs (list "src"))

(define (util:filter p? lst)
  (if (null? lst)
      '()
      (if (p? (car lst))
          (cons (car lst)
                (util:filter p? (cdr lst)))
          (util:filter p? (cdr lst)))))

(define (ex:map-while f lst k)
  (cond ((null? lst) (k '() '()))
        ((pair? lst)
         (let ((head (f (car lst))))
           (if head
               (ex:map-while f
                          (cdr lst)
                          (lambda (answer rest)
                            (k (cons head answer)
                               rest)))
               (k '() lst))))
        (else  (k '() lst))))

(define-record-type <library>
    (make-library-record name envs exports imports builds syntax-defs bound-vars forms build visited? invoked?)
    ex:library?
    (name        ex:library-name)
    (envs        ex:library-envs)
    (exports     ex:library-exports)
    (imports     ex:library-imports)
    (builds      ex:library-builds)
    (syntax-defs ex:library-syntax-defs)
    (bound-vars  ex:library-bound-vars)
    (forms       ex:library-forms)
    (build       ex:library-build)
    (visited?    ex:library-visited? ex:library-visited?-set!)
    (invoked?    ex:library-invoked? ex:library-invoked?-set!))

(define (ex:make-library name envs exports imports builds syntax-defs bound-vars forms build)
  (make-library-record name envs exports imports builds syntax-defs bound-vars forms build #f #f))

(define ex:imported '())
(define (import-library name build phase importer run-or-expand)
  (if (not (member (cons name (cons phase run-or-expand)) ex:imported))
      (let ((library (ex:lookup-library name)))
        (or (not build)
            (eq? build (ex:library-build library))
            (let () 
              (display build) (newline)
              (display (ex:library-build library)) (newline)
            (error 
             "Import failed: client was expanded against a different build of this library" name)))
        (ex:import-libraries-for (ex:library-imports library) 
                          (ex:library-builds library)
                          phase
                          importer 
                          run-or-expand)
        (importer library phase ex:imported)
        (set! ex:imported (cons (cons name (cons phase run-or-expand)) ex:imported)))))

(define (importer library phase imported)
  (if (and (= phase 0)
    (not (ex:library-invoked? library)))
    (begin 
      (ex:invoke-library! library)
      (ex:library-invoked?-set! library #t))))

(define (ex:import-libraries-for imports builds phase importer run-or-expand)
  (for-each (lambda (import build)
              (let ((name   (car import))
                    (levels (cdr import)))
                (for-each (lambda (level)
                            (import-library name build (+ phase level) importer run-or-expand))
                          levels)))
            imports
            builds)
  (values))
(define (ex:import-libraries-for-run imports builds phase)
  (ex:import-libraries-for imports 
                           builds
                           phase 
                           importer
                           'run))

(define (ex:import-library name)
    (let ((library (ex:lookup-library name)))
      (ex:import-libraries-for-run (ex:library-imports library) (ex:library-builds library) 0)
      (import-library (ex:library-name library) (ex:library-build library) 0 importer 'run)))

(define table '())
(define ex:register-library! 
    (lambda (library)
      (set! table (cons library table))
      (set! ex:imported (util:filter (lambda (entry)
                                  (not (equal? (ex:library-name library) 
                                               (car entry))))
                                ex:imported))))

(define ex:invoke-library!
    (lambda (library)
        (ex:runtime-eval `(begin
                      ,@(map (lambda (var) `(define ,var '(if #f #f))) (ex:library-bound-vars library))
                      ,@(ex:library-forms library)))))

(define ex:lookup-library 
    (lambda (name)
      (let ((library (ex:lookup-library/false name)))
        (if library
            library
            (error "library lookup failed, library not loaded" name)))))

(define ex:lookup-library/false
    (lambda (name)
      (let ((library (find (lambda (l) (equal? name (ex:library-name l))) table)))
        (if library
            library
            #f))))

(define runtime-env #f)

(define (runtime-env-init!)
  (set! runtime-env 
    (environment
     '(scheme case-lambda)
     '(scheme char)
     '(scheme complex)
     '(scheme cxr)
     '(scheme eval)
     '(scheme file)
     '(scheme inexact)
     '(scheme lazy)
     '(scheme load)
     '(scheme process-context)
     '(scheme read)
     '(scheme repl)
     '(scheme time)
     '(scheme write)
     '(scheme base)
     )))

(define (ex:runtime-add-primitive name value)
  (ex:runtime-eval `(define ,name ,value)))

(define (ex:runtime-eval e)
  (if (not runtime-env) (runtime-env-init!))
  (host:eval e runtime-env))

; TODO - this sucks, we need exceptions really early
; so we can't define exceptions using target-system records
; when record types are moved away from vectors, we should
; be able to move this back to (core exception)
(define-record-type <exception>
  (make-exception type message irritants)
  exception?
  (type exception-type)
  (message exception-message)
  (irritants exception-irritants))

; root abort
(define (abort obj)
  (for-each (lambda (trace) 
    (debug "trace:" trace)) traces)
  (raise obj))

(define traces '())
(define (trace src proc . args)
  (debug "TRACE" src)
  (set! traces (cons src traces))
  (apply proc args))

(define (string-cmp a b ci?)
  (if ci?
    (cond
      ((string-ci<? a b) -1)
      ((string-ci>? a b) 1)
      (else 0))
    (cond
      ((string<? a b) -1)
      ((string>? a b) 1)
      (else 0))))

(define-record-type <loki-port>
  (make-loki-port input output type)
  loki-port?
  (input loki-port-input loki-port-input-set!)
  (output loki-port-output loki-port-output-set!)
  (type loki-port-type))

(define-record-type <loki-eof-object>
  (make-loki-eof-object)
  loki-eof-object?)
(define %loki-eof-object (make-loki-eof-object))
(define (loki-eof-object) %loki-eof-object)
(define (loki-wrap-eof obj)
  (if (eof-object? obj)
    %loki-eof-object
    obj))

(define (loki-port-ready? port)
  (unless (loki-port-input port)
    (raise "loki-port-ready?: not an input port"))
  (let ((type (loki-port-type port)))
    (cond
      ((eq? type 'textual) (char-ready? (loki-port-input port)))
      ((eq? type 'binary) (u8-ready? (loki-port-input port)))
      (else (raise "loki-port-ready?: unknown type")))))

(define (loki-input-port-open? port)
  (and (loki-port-input port)
       (input-port-open? (loki-port-input port))))

(define (loki-output-port-open? port)
  (and (loki-port-output port)
       (output-port-open? (loki-port-output port))))

(define (loki-close-input-port port)
  (if (loki-port-input port)
    ((close-input-port (loki-port-input port)))))

(define (loki-close-output-port port)
  (if (loki-port-output port)
    ((close-output-port (loki-port-output port)))))

(define (loki-get-output-string port)
  (get-output-string (loki-port-output port)))
(define (loki-get-output-bytevector port)
  (get-output-bytevector (loki-port-output port)))

(define (loki-open-output-string)
  (make-loki-port #f (open-output-string) 'textual))
(define (loki-open-input-string string)
  (make-loki-port (open-input-string string) #f 'textual))

(define (loki-open-output-bytevector)
  (make-loki-port #f (open-output-bytevector) 'binary))
(define (loki-open-input-bytevector bytevector)
  (make-loki-port (open-input-bytevector bytevector) #f 'binary))


(define (loki-open-output-file file)
  (make-loki-port #f (open-output-file file) 'textual))
(define (loki-open-input-file file)
  (make-loki-port (open-input-file file) #f 'textual))

(define (loki-open-binary-output-file file)
  (make-loki-port #f (open-binary-output-file file) 'binary))
(define (loki-open-binary-input-file file)
  (make-loki-port (open-binary-input-file file) #f 'binary))

(define (loki-stderr) (make-loki-port #f (current-error-port) 'textual))
(define (loki-stdin) (make-loki-port (current-input-port) #f 'textual))
(define (loki-stdout) (make-loki-port #f (current-output-port) 'textual))

(define (loki-flush-output-port port)
  (unless (output-port? (loki-port-type port))
    (raise "flush-output-port: not an output port"))
  (flush-output-port (loki-port-output port)))



(define (loki-peek-u8 port)
  (unless (eq? (loki-port-type port) 'binary)
    (raise "peek-u8: not a binary port"))
  (loki-wrap-eof (peek-u8 (loki-port-input port))))
(define (loki-peek-char port)
  (unless (eq? (loki-port-type port) 'textual)
    (raise "peek-char: not a textual port"))
  (loki-wrap-eof (peek-u8 (loki-port-input port))))

(define (loki-read-bytevector! bytevector port start end)
  (loki-wrap-eof (read-bytevector! bytevector (loki-port-input port) start end)))
(define (loki-read-bytevector bytevector port)
  (loki-wrap-eof (read-bytevector bytevector (loki-port-input port))))
(define (loki-read-string k port)
  (loki-wrap-eof (read-string k (loki-port-input port))))
(define (loki-read-char port)
  (loki-wrap-eof (read-char (loki-port-input port))))
(define (loki-read-line port)
  (loki-wrap-eof (read-line (loki-port-input port))))
(define (loki-read-u8 port)
  (loki-wrap-eof (read-u8 (loki-port-input port))))

(define (loki-write-bytevector bytevector port start end)
  (write-bytevector bytevector (loki-port-output port) start end))
(define (loki-write-string string port start end)
  (write-string string (loki-port-output port) start end))
(define (loki-write-char char port)
  (write-char char (loki-port-output port)))
(define (loki-write-u8 u8 port)
  (write-u8 u8 (loki-port-output port)))

;; Register the required runtime primitives
(ex:runtime-add-primitive 'void (if #f #f))
(ex:runtime-add-primitive 'ex:map-while ex:map-while)
(ex:runtime-add-primitive 'ex:import-library ex:import-library)

(ex:runtime-add-primitive '%add        +)
(ex:runtime-add-primitive '%sub        -)
(ex:runtime-add-primitive '%mul        *)
(ex:runtime-add-primitive '%div        /)
(ex:runtime-add-primitive '%lt         <)
(ex:runtime-add-primitive '%lte       <=)
(ex:runtime-add-primitive '%number-eq  =)
(ex:runtime-add-primitive '%gt         >)
(ex:runtime-add-primitive '%gte       >=)

(ex:runtime-add-primitive '%cons      cons)
(ex:runtime-add-primitive '%pair?     pair?)
(ex:runtime-add-primitive '%null?     null?)
(ex:runtime-add-primitive '%list?     list?)
(ex:runtime-add-primitive '%car       car)
(ex:runtime-add-primitive '%cdr       cdr)
(ex:runtime-add-primitive '%set-car!  set-car!)
(ex:runtime-add-primitive '%set-cdr!  set-cdr!)

(ex:runtime-add-primitive '%vector?        vector?)
(ex:runtime-add-primitive '%vector-set!    vector-set!)
(ex:runtime-add-primitive '%vector-ref     vector-ref)
(ex:runtime-add-primitive '%vector-length  vector-length)
(ex:runtime-add-primitive '%make-vector    make-vector)

(ex:runtime-add-primitive '%bytevector?        bytevector?)
(ex:runtime-add-primitive '%bytevector-u8-set! bytevector-u8-set!)
(ex:runtime-add-primitive '%bytevector-u8-ref  bytevector-u8-ref)
(ex:runtime-add-primitive '%make-bytevector    make-bytevector)
(ex:runtime-add-primitive '%bytevector-length  bytevector-length)

(ex:runtime-add-primitive '%char->integer char->integer)
(ex:runtime-add-primitive '%integer->char integer->char)
(ex:runtime-add-primitive '%char-foldcase char-foldcase)
(ex:runtime-add-primitive '%char-upcase   char-upcase)
(ex:runtime-add-primitive '%char-downcase char-downcase)
(ex:runtime-add-primitive '%char?         char?)

(ex:runtime-add-primitive '%string-set!   string-set!)
(ex:runtime-add-primitive '%string-ref    string-ref)
(ex:runtime-add-primitive '%make-string   make-string)
(ex:runtime-add-primitive '%string-length string-length)
(ex:runtime-add-primitive '%string->symbol string->symbol)
(ex:runtime-add-primitive '%symbol->string symbol->string)
(ex:runtime-add-primitive '%string-downcase string-downcase)
(ex:runtime-add-primitive '%string-upcase string-upcase)
(ex:runtime-add-primitive '%string-foldcase string-foldcase)
(ex:runtime-add-primitive '%number->string number->string)
(ex:runtime-add-primitive '%string->number string->number)

(ex:runtime-add-primitive '%apply               apply)
(ex:runtime-add-primitive '%abort               abort)
(ex:runtime-add-primitive '%make-exception      make-exception)
(ex:runtime-add-primitive '%exception?          exception?)
(ex:runtime-add-primitive '%exception-type      exception-type)
(ex:runtime-add-primitive '%exception-message   exception-message)
(ex:runtime-add-primitive '%exception-irritants exception-irritants)
(ex:runtime-add-primitive '%procedure?          procedure?)
(ex:runtime-add-primitive '%symbol?             symbol?)
(ex:runtime-add-primitive '%string?             string?)
(ex:runtime-add-primitive '%string-cmp          string-cmp)

(ex:runtime-add-primitive '%eq?    eq?)
(ex:runtime-add-primitive '%eqv?   eqv?)
(ex:runtime-add-primitive '%equal? equal?)

(ex:runtime-add-primitive '%number?    number?)
(ex:runtime-add-primitive '%finite?    finite?)
(ex:runtime-add-primitive '%infinite?  infinite?)
(ex:runtime-add-primitive '%nan?       nan?)
(ex:runtime-add-primitive '%floor      floor)
(ex:runtime-add-primitive '%ceiling    ceiling)
(ex:runtime-add-primitive '%truncate   truncate)
(ex:runtime-add-primitive '%round      round)
(ex:runtime-add-primitive '%sqrt       sqrt)
(ex:runtime-add-primitive '%expt       expt)

(ex:runtime-add-primitive '%command-line          ''())
(ex:runtime-add-primitive '%environment-variables ''())
(ex:runtime-add-primitive '%emergency-exit        exit) ; TODO - this sucks

(ex:runtime-add-primitive '%port?                   loki-port?)
(ex:runtime-add-primitive '%eof-object              loki-eof-object)
(ex:runtime-add-primitive '%eof-object?             loki-eof-object?)
(ex:runtime-add-primitive '%port-input              loki-port-input)
(ex:runtime-add-primitive '%port-output             loki-port-output)
(ex:runtime-add-primitive '%port-type               loki-port-type)
(ex:runtime-add-primitive '%port-ready?             loki-port-ready?)
(ex:runtime-add-primitive '%input-port-open?        loki-input-port-open?)
(ex:runtime-add-primitive '%output-port-open?       loki-output-port-open?)
(ex:runtime-add-primitive '%close-input-port        loki-close-input-port)
(ex:runtime-add-primitive '%close-output-port       loki-close-output-port)
(ex:runtime-add-primitive '%delete-file             delete-file)
(ex:runtime-add-primitive '%file-exists?            file-exists?)
(ex:runtime-add-primitive '%get-output-string       loki-get-output-string)
(ex:runtime-add-primitive '%get-output-bytevector   loki-get-output-bytevector)
(ex:runtime-add-primitive '%open-output-string      loki-open-output-string)
(ex:runtime-add-primitive '%open-input-string       loki-open-input-string)
(ex:runtime-add-primitive '%open-input-bytevector   loki-open-input-bytevector)
(ex:runtime-add-primitive '%open-output-bytevector  loki-open-output-bytevector)
(ex:runtime-add-primitive '%open-output-file        loki-open-output-file)
(ex:runtime-add-primitive '%open-input-file         loki-open-input-file)
(ex:runtime-add-primitive '%open-binary-input-file  loki-open-binary-input-file)
(ex:runtime-add-primitive '%open-binary-output-file loki-open-binary-output-file)
(ex:runtime-add-primitive '%stderr                  loki-stderr)
(ex:runtime-add-primitive '%stdin                   loki-stdin)
(ex:runtime-add-primitive '%stdout                  loki-stdout)
(ex:runtime-add-primitive '%flush-output-port       flush-output-port)

(ex:runtime-add-primitive '%peek-char        loki-peek-char)
(ex:runtime-add-primitive '%peek-u8          loki-peek-u8)
(ex:runtime-add-primitive '%read-bytevector! loki-read-bytevector!)
(ex:runtime-add-primitive '%read-bytevector! loki-read-bytevector!)
(ex:runtime-add-primitive '%read-bytevector  loki-read-bytevector)
(ex:runtime-add-primitive '%read-string      loki-read-string)
(ex:runtime-add-primitive '%read-char        loki-read-char)
(ex:runtime-add-primitive '%read-line        loki-read-line)
(ex:runtime-add-primitive '%read-u8          loki-read-u8)
(ex:runtime-add-primitive '%write-bytevector loki-write-bytevector)
(ex:runtime-add-primitive '%write-string     loki-write-string)
(ex:runtime-add-primitive '%write-char       loki-write-char)
(ex:runtime-add-primitive '%write-u8         loki-write-u8)

(ex:runtime-add-primitive '%current-jiffy         current-jiffy)
(ex:runtime-add-primitive '%current-second        current-second)
(ex:runtime-add-primitive '%jiffies-per-second    jiffies-per-second)

(ex:runtime-add-primitive '%hash-by-identity hash-by-identity)
(ex:runtime-add-primitive '%trace trace)


;; Only instantiate part of the bootstrap library 
;; that would be needed for invocation at runtime.

(ex:register-library! 
 (ex:make-library
  '(core primitive-macros)
  ;; envs
  '()
  ;; exports
  '()
  ;; imported-libraries
  '()
  ;; builds
  '()
  ;; syntax-defs
  '()
  ;; bound-vars
  '()
  ;; forms
  '()
  ;; build
  'system))
))
