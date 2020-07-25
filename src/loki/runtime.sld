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
(import (loki reader))
(import (loki core fs))
(import (srfi 1))
(import (srfi 69))
(import (srfi 151))

(export rt:library-dirs
        rt:make-library
        rt:library-name
        rt:library-envs
        rt:library-exports
        rt:library-imports
        rt:library-builds
        rt:library-syntax-defs
        rt:library-bound-vars
        rt:library-forms
        rt:library-build
        rt:library-visited?
        rt:library-invoked?
        rt:library-visited?-set!
        rt:library-invoked?-set!
        rt:import-libraries-for
        rt:import-libraries-for-run
        rt:import-library
        rt:register-library!
        rt:invoke-library!
        rt:lookup-library
        rt:lookup-library/false
        rt:runtime-add-primitive
        rt:runtime-eval)
(begin

(define rt:library-dirs (list "src"))

(define (util:filter p? lst)
  (if (null? lst)
      '()
      (if (p? (car lst))
          (cons (car lst)
                (util:filter p? (cdr lst)))
          (util:filter p? (cdr lst)))))


(define-record-type <library>
    (make-library-record name envs exports imports builds syntax-defs bound-vars forms build visited? invoked?)
    rt:library?
    (name        rt:library-name)
    (envs        rt:library-envs)
    (exports     rt:library-exports)
    (imports     rt:library-imports)
    (builds      rt:library-builds)
    (syntax-defs rt:library-syntax-defs)
    (bound-vars  rt:library-bound-vars)
    (forms       rt:library-forms)
    (build       rt:library-build)
    (visited?    rt:library-visited? rt:library-visited?-set!)
    (invoked?    rt:library-invoked? rt:library-invoked?-set!))

(define (rt:make-library name envs exports imports builds syntax-defs bound-vars forms build)
  (make-library-record name envs exports imports builds syntax-defs bound-vars forms build #f #f))

(define rt:imported '())
(define (import-library name build phase importer run-or-expand)
  (if (not (member (cons name (cons phase run-or-expand)) rt:imported))
      (let ((library (rt:lookup-library name)))
        (or (not build)
            (eq? build (rt:library-build library))
            (let () 
              (display build) (newline)
              (display (rt:library-build library)) (newline)
            (error 
             "Import failed: client was expanded against a different build of this library" name)))
        (rt:import-libraries-for (rt:library-imports library) 
                          (rt:library-builds library)
                          phase
                          importer 
                          run-or-expand)
        (importer library phase rt:imported)
        (set! rt:imported (cons (cons name (cons phase run-or-expand)) rt:imported)))))

(define (importer library phase imported)
  (if (and (= phase 0)
    (not (rt:library-invoked? library)))
    (begin 
      (rt:invoke-library! library)
      (rt:library-invoked?-set! library #t))))

(define (rt:import-libraries-for imports builds phase importer run-or-expand)
  (for-each (lambda (import build)
              (let ((name   (car import))
                    (levels (cdr import)))
                (for-each (lambda (level)
                            (import-library name build (+ phase level) importer run-or-expand))
                          levels)))
            imports
            builds)
  (values))
(define (rt:import-libraries-for-run imports builds phase)
  (rt:import-libraries-for imports 
                           builds
                           phase 
                           importer
                           'run))

(define (rt:import-library name)
    (let ((library (rt:lookup-library name)))
      (rt:import-libraries-for-run (rt:library-imports library) (rt:library-builds library) 0)
      (import-library (rt:library-name library) (rt:library-build library) 0 importer 'run)))

(define table '())
(define rt:register-library! 
    (lambda (library)
      (set! table (cons library table))
      (set! rt:imported (util:filter (lambda (entry)
                                  (not (equal? (rt:library-name library) 
                                               (car entry))))
                                rt:imported))))

(define rt:invoke-library!
    (lambda (library)
        (map rt:runtime-eval (rt:library-forms library))))

(define rt:lookup-library 
    (lambda (name)
      (let ((library (rt:lookup-library/false name)))
        (if library
            library
            (error "library lookup failed, library not loaded" name)))))

(define rt:lookup-library/false
    (lambda (name)
      (let ((library (find (lambda (l) (equal? name (rt:library-name l))) table)))
        (if library
            library
            #f))))

(define runtime-env #f)

(define (runtime-env-init!)
  (set! runtime-env 
    (environment '(scheme base))))

(define (rt:runtime-add-primitive name value)
  (rt:runtime-eval `(define ,name ,value)))

(define (rt:runtime-eval e)
  (if (not runtime-env) (runtime-env-init!))
  (with-exception-handler
    (lambda (err)
      (current-exception-handler err))
    (lambda () (eval e runtime-env))))

(define-record-type <loki-values>
  (make-loki-values values)
  loki-values?
  (values loki-values-values))
(define (loki-values . args)
  (if (= (length args) 1)
    (car args)
    (make-loki-values args)))
(define (loki-call-with-values producer consumer)
  (let ((res (producer)))
    (if (loki-values? res)
      (apply consumer (loki-values-values res))
      (consumer res))))
(define (loki-apply . args)
  (apply apply args))
(define (loki-call/cc thunk)
  (define (kont k)
    (thunk k))
  (call/cc kont))

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

(define current-exception-handler #f)
(define (exception-handler) current-exception-handler)
(define (exception-handler-set! handler)
  (set! current-exception-handler handler))

; root abort
(define (abort obj)
  (display "ABORT!\n")
  (display obj)
  (display "\n")
  (for-each (lambda (trace) 
    (display trace)
    (display "\n")) traces)
  (exit 1))

(define traces '())
(define (trace src)
  (set! traces (cons src traces)))
(define (pop-trace)
  (if (null? traces) (error "pop-trace, but no traces, this shouldn't happen"))
  (set! traces (cdr traces)))

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
  (unless (output-port? (loki-port-output port))
    (raise "flush-output-port: not an output port"))
  (flush-output-port (loki-port-output port)))



(define (loki-peek-u8 port)
  (unless (eq? (loki-port-type port) 'binary)
    (raise "peek-u8: not a binary port"))
  (loki-wrap-eof (peek-u8 (loki-port-input port))))
(define (loki-peek-char port)
  (unless (eq? (loki-port-type port) 'textual)
    (raise "peek-char: not a textual port"))
  (loki-wrap-eof (peek-char (loki-port-input port))))

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

(define (loki-repr obj)
  (write-to-string obj))
(define (loki-debug . obj)
  (apply debug "DEBUG" obj))

;; Register the required runtime primitives
(rt:runtime-add-primitive '%void (if #f #f))
(rt:runtime-add-primitive '%add        +)
(rt:runtime-add-primitive '%sub        -)
(rt:runtime-add-primitive '%mul        *)
(rt:runtime-add-primitive '%div        /)
(rt:runtime-add-primitive '%lt         <)
(rt:runtime-add-primitive '%lte       <=)
(rt:runtime-add-primitive '%number-eq  =)
(rt:runtime-add-primitive '%gt         >)
(rt:runtime-add-primitive '%gte       >=)
(rt:runtime-add-primitive '%bit-not    bitwise-not)
(rt:runtime-add-primitive '%bit-and    bitwise-and)
(rt:runtime-add-primitive '%bit-ior    bitwise-ior)
(rt:runtime-add-primitive '%bit-xor    bitwise-xor)
(rt:runtime-add-primitive '%bit-shift  arithmetic-shift)
(rt:runtime-add-primitive '%bit-count  bit-count)
(rt:runtime-add-primitive '%bit-length integer-length)

(rt:runtime-add-primitive '%cons      cons)
(rt:runtime-add-primitive '%pair?     pair?)
(rt:runtime-add-primitive '%null?     null?)
(rt:runtime-add-primitive '%list?     list?)
(rt:runtime-add-primitive '%car       car)
(rt:runtime-add-primitive '%cdr       cdr)
(rt:runtime-add-primitive '%set-car!  set-car!)
(rt:runtime-add-primitive '%set-cdr!  set-cdr!)

(rt:runtime-add-primitive '%vector?        vector?)
(rt:runtime-add-primitive '%vector-set!    vector-set!)
(rt:runtime-add-primitive '%vector-ref     vector-ref)
(rt:runtime-add-primitive '%vector-length  vector-length)
(rt:runtime-add-primitive '%make-vector    make-vector)

(rt:runtime-add-primitive '%bytevector?        bytevector?)
(rt:runtime-add-primitive '%bytevector-u8-set! bytevector-u8-set!)
(rt:runtime-add-primitive '%bytevector-u8-ref  bytevector-u8-ref)
(rt:runtime-add-primitive '%make-bytevector    make-bytevector)
(rt:runtime-add-primitive '%bytevector-length  bytevector-length)

(rt:runtime-add-primitive '%char->integer char->integer)
(rt:runtime-add-primitive '%integer->char integer->char)
(rt:runtime-add-primitive '%char-foldcase char-foldcase)
(rt:runtime-add-primitive '%char-upcase   char-upcase)
(rt:runtime-add-primitive '%char-downcase char-downcase)
(rt:runtime-add-primitive '%char?         char?)

(rt:runtime-add-primitive '%string-set!   string-set!)
(rt:runtime-add-primitive '%string-ref    string-ref)
(rt:runtime-add-primitive '%make-string   make-string)
(rt:runtime-add-primitive '%string-length string-length)
(rt:runtime-add-primitive '%string->symbol string->symbol)
(rt:runtime-add-primitive '%symbol->string symbol->string)
(rt:runtime-add-primitive '%string-downcase string-downcase)
(rt:runtime-add-primitive '%string-upcase string-upcase)
(rt:runtime-add-primitive '%string-foldcase string-foldcase)
(rt:runtime-add-primitive '%number->string number->string)
(rt:runtime-add-primitive '%string->number string->number)

(rt:runtime-add-primitive '%abort               abort)
(rt:runtime-add-primitive '%make-exception      make-exception)
(rt:runtime-add-primitive '%exception?          exception?)
(rt:runtime-add-primitive '%exception-type      exception-type)
(rt:runtime-add-primitive '%exception-message   exception-message)
(rt:runtime-add-primitive '%exception-irritants exception-irritants)
(rt:runtime-add-primitive '%exception-handler   exception-handler)
(rt:runtime-add-primitive '%exception-handler-set! exception-handler-set!)
(rt:runtime-add-primitive '%procedure?          procedure?)
(rt:runtime-add-primitive '%symbol?             symbol?)
(rt:runtime-add-primitive '%string?             string?)
(rt:runtime-add-primitive '%string-cmp          string-cmp)

(rt:runtime-add-primitive '%eq?    eq?)
(rt:runtime-add-primitive '%eqv?   eqv?)
(rt:runtime-add-primitive '%equal? equal?)

(rt:runtime-add-primitive '%number?    number?)
(rt:runtime-add-primitive '%finite?    finite?)
(rt:runtime-add-primitive '%infinite?  infinite?)
(rt:runtime-add-primitive '%nan?       nan?)
(rt:runtime-add-primitive '%floor      floor)
(rt:runtime-add-primitive '%ceiling    ceiling)
(rt:runtime-add-primitive '%truncate   truncate)
(rt:runtime-add-primitive '%quotient   quotient)
(rt:runtime-add-primitive '%remainder  remainder)
(rt:runtime-add-primitive '%round      round)
(rt:runtime-add-primitive '%sqrt       sqrt)
(rt:runtime-add-primitive '%expt       expt)

(rt:runtime-add-primitive '%command-line          ''())
(rt:runtime-add-primitive '%environment-variables ''())
(rt:runtime-add-primitive '%emergency-exit        exit) ; TODO - this sucks

(rt:runtime-add-primitive '%port?                   loki-port?)
(rt:runtime-add-primitive '%eof-object              loki-eof-object)
(rt:runtime-add-primitive '%eof-object?             loki-eof-object?)
(rt:runtime-add-primitive '%port-input              loki-port-input)
(rt:runtime-add-primitive '%port-output             loki-port-output)
(rt:runtime-add-primitive '%port-type               loki-port-type)
(rt:runtime-add-primitive '%port-ready?             loki-port-ready?)
(rt:runtime-add-primitive '%input-port-open?        loki-input-port-open?)
(rt:runtime-add-primitive '%output-port-open?       loki-output-port-open?)
(rt:runtime-add-primitive '%close-input-port        loki-close-input-port)
(rt:runtime-add-primitive '%close-output-port       loki-close-output-port)
(rt:runtime-add-primitive '%delete-file             delete-file)
(rt:runtime-add-primitive '%file-exists?            file-exists?)
(rt:runtime-add-primitive '%get-output-string       loki-get-output-string)
(rt:runtime-add-primitive '%get-output-bytevector   loki-get-output-bytevector)
(rt:runtime-add-primitive '%open-output-string      loki-open-output-string)
(rt:runtime-add-primitive '%open-input-string       loki-open-input-string)
(rt:runtime-add-primitive '%open-input-bytevector   loki-open-input-bytevector)
(rt:runtime-add-primitive '%open-output-bytevector  loki-open-output-bytevector)
(rt:runtime-add-primitive '%open-output-file        loki-open-output-file)
(rt:runtime-add-primitive '%open-input-file         loki-open-input-file)
(rt:runtime-add-primitive '%open-binary-input-file  loki-open-binary-input-file)
(rt:runtime-add-primitive '%open-binary-output-file loki-open-binary-output-file)
(rt:runtime-add-primitive '%stderr                  loki-stderr)
(rt:runtime-add-primitive '%stdin                   loki-stdin)
(rt:runtime-add-primitive '%stdout                  loki-stdout)
(rt:runtime-add-primitive '%flush-output-port       loki-flush-output-port)

(rt:runtime-add-primitive '%peek-char        loki-peek-char)
(rt:runtime-add-primitive '%peek-u8          loki-peek-u8)
(rt:runtime-add-primitive '%read-bytevector! loki-read-bytevector!)
(rt:runtime-add-primitive '%read-bytevector! loki-read-bytevector!)
(rt:runtime-add-primitive '%read-bytevector  loki-read-bytevector)
(rt:runtime-add-primitive '%read-string      loki-read-string)
(rt:runtime-add-primitive '%read-char        loki-read-char)
(rt:runtime-add-primitive '%read-line        loki-read-line)
(rt:runtime-add-primitive '%read-u8          loki-read-u8)
(rt:runtime-add-primitive '%write-bytevector loki-write-bytevector)
(rt:runtime-add-primitive '%write-string     loki-write-string)
(rt:runtime-add-primitive '%write-char       loki-write-char)
(rt:runtime-add-primitive '%write-u8         loki-write-u8)

(rt:runtime-add-primitive '%current-jiffy         current-jiffy)
(rt:runtime-add-primitive '%current-second        current-second)
(rt:runtime-add-primitive '%jiffies-per-second    jiffies-per-second)

(rt:runtime-add-primitive '%apply loki-apply)
(rt:runtime-add-primitive '%values loki-values)
(rt:runtime-add-primitive '%call-with-values loki-call-with-values)
(rt:runtime-add-primitive '%call/cc loki-call/cc)
(rt:runtime-add-primitive '%hash-by-identity hash-by-identity)
(rt:runtime-add-primitive '%current-directory current-directory)
(rt:runtime-add-primitive '%trace trace)
(rt:runtime-add-primitive '%pop-trace pop-trace)
(rt:runtime-add-primitive '%repr loki-repr)
(rt:runtime-add-primitive '%debug loki-debug)


;; Only instantiate part of the bootstrap library 
;; that would be needed for invocation at runtime.

(rt:register-library! 
 (rt:make-library
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
