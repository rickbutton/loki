(define-library (loki compiler runtime)
(import (scheme base))
(import (scheme cxr))
(import (scheme write))
(import (scheme eval))
(import (scheme char))
(import (scheme inexact))
(import (scheme file))
(import (scheme time))
(import (scheme process-context))
(import (loki util))
(import (loki core fs))
(import (loki core reflect))
(import (loki core reader))
(import (loki compiler lang core))
(import (srfi 69))
(import (srfi 151))
(cond-expand
 (gauche (import (only (gauche base) report-error))))
(export runtime-run-program
        runtime-add-primitive
        (rename exception? runtime-exception?)
        (rename exception-type runtime-exception-type)
        (rename exception-message runtime-exception-message)
        (rename exception-irritants runtime-exception-irritants)
        with-loki-command-line)
(begin

(define runtime-env #f)

(define (runtime-env-init!)
  (set! runtime-env 
    (environment '(only (scheme base)
    begin
    if
    lambda
    quote
    set!
    define
    letrec

    map
    list
    list?
    vector->list
    list->vector
    append
    length
    ))))

(define env (cond-expand (loki "loki") (else "base")))

(define (error->string err)
  (let ((out (open-output-string)))
    (cond-expand
      (loki
        (if (exception? err)
          (begin
            (display (exception-type err) out)
            (display ": " out)
            (display (exception-message err) out)
            (newline out)
            (display (exception-irritants err) out)
            (newline out))
          (display err out)))
      (else
        (if (error-object? err)
          (display (report-error err #f) out))))
    (get-output-string out)))

(define (runtime-run-program prog)
  (if (not runtime-env) (runtime-env-init!))
  (with-exception-handler
    (lambda (err)
      (let ((err (if (error-object? err)
                     (make-exception 'eval-error
                                     (error->string err)
                                     (error-object-irritants err))
                     (make-exception 'eval-error
                                     "wrapped error"
                                     (error->string err)))))
        (if current-exception-handler
            (current-exception-handler err)
            (begin
              (display "ERROR: current-exception-handler is not setup, aborting\n")
              (raise err)))))
    (lambda ()
      (let ((host-scheme (compile-core-to-host-scheme prog)))
        (eval `(begin ,@host-scheme) runtime-env)))))

(define (runtime-add-primitive name value)
  (if (not runtime-env) (runtime-env-init!))
  (eval `(define ,name ,value) runtime-env))

; TODO - this sucks, we need exceptions really early
; so we can't define exceptions using target-system records
; when record types are moved away from vectors, we should
; be able to move this back to (loki core exception)
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
  (if (exception? obj)
    (begin
      (display (exception-type obj))
      (display ": ")
      (display (exception-message obj))
      (newline)
      (display (exception-irritants obj))
      (newline))
    (display obj))
  (exit 1))

(define max-traces (* 1024 10))
(define traces (make-vector max-traces #f))
(define next-trace-slot 0)
(define (trace src k)
  (vector-set! traces next-trace-slot src)
  (if (= next-trace-slot (- max-traces 1))
      (set! next-trace-slot 0)
      (set! next-trace-slot (+ next-trace-slot 1)))
  k)
(define (emit-traces)
  (do ((i next-trace-slot (+ i 1)))
      ((= i max-traces) #f)
    (display (vector-ref traces i))
    (display "\n"))
  (do ((i 0 (+ i 1)))
      ((= i next-trace-slot) #f)
    (display (vector-ref traces i))
    (display "\n")))


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
    (close-input-port (loki-port-input port))))

(define (loki-close-output-port port)
  (if (loki-port-output port)
    (close-output-port (loki-port-output port))))

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

(define *current-command-line* (make-parameter '()))
(define (with-loki-command-line cmd thunk)
  (parameterize ((*current-command-line* (cons "loki" cmd)))
    (thunk)))
(define (loki-command-line)
  (list-copy (*current-command-line*)))

;; Register the required runtime primitives
(runtime-add-primitive '%void (if #f #f))
(runtime-add-primitive '%add        +)
(runtime-add-primitive '%sub        -)
(runtime-add-primitive '%mul        *)
(runtime-add-primitive '%div        /)
(runtime-add-primitive '%lt         <)
(runtime-add-primitive '%lte       <=)
(runtime-add-primitive '%number-eq  =)
(runtime-add-primitive '%gt         >)
(runtime-add-primitive '%gte       >=)
(runtime-add-primitive '%bit-not    bitwise-not)
(runtime-add-primitive '%bit-and    bitwise-and)
(runtime-add-primitive '%bit-ior    bitwise-ior)
(runtime-add-primitive '%bit-xor    bitwise-xor)
(runtime-add-primitive '%bit-shift  arithmetic-shift)
(runtime-add-primitive '%bit-count  bit-count)
(runtime-add-primitive '%bit-length integer-length)

(runtime-add-primitive '%cons      cons)
(runtime-add-primitive '%pair?     pair?)
(runtime-add-primitive '%null?     null?)
(runtime-add-primitive '%list?     list?)
(runtime-add-primitive '%car       car)
(runtime-add-primitive '%cdr       cdr)
(runtime-add-primitive '%set-car!  set-car!)
(runtime-add-primitive '%set-cdr!  set-cdr!)

(runtime-add-primitive '%vector?        vector?)
(runtime-add-primitive '%vector-set!    vector-set!)
(runtime-add-primitive '%vector-ref     vector-ref)
(runtime-add-primitive '%vector-length  vector-length)
(runtime-add-primitive '%make-vector    make-vector)

(runtime-add-primitive '%bytevector?        bytevector?)
(runtime-add-primitive '%bytevector-u8-set! bytevector-u8-set!)
(runtime-add-primitive '%bytevector-u8-ref  bytevector-u8-ref)
(runtime-add-primitive '%make-bytevector    make-bytevector)
(runtime-add-primitive '%bytevector-length  bytevector-length)

(runtime-add-primitive '%char->integer char->integer)
(runtime-add-primitive '%integer->char integer->char)
(runtime-add-primitive '%char-foldcase char-foldcase)
(runtime-add-primitive '%char-upcase   char-upcase)
(runtime-add-primitive '%char-downcase char-downcase)
(runtime-add-primitive '%char?         char?)

(runtime-add-primitive '%string-set!   string-set!)
(runtime-add-primitive '%string-ref    string-ref)
(runtime-add-primitive '%make-string   make-string)
(runtime-add-primitive '%string-length string-length)
(runtime-add-primitive '%string->symbol string->symbol)
(runtime-add-primitive '%symbol->string symbol->string)
(runtime-add-primitive '%string-downcase string-downcase)
(runtime-add-primitive '%string-upcase string-upcase)
(runtime-add-primitive '%string-foldcase string-foldcase)
(runtime-add-primitive '%number->string number->string)
(runtime-add-primitive '%string->number string->number)

(runtime-add-primitive '%abort               abort)
(runtime-add-primitive '%make-exception      make-exception)
(runtime-add-primitive '%exception?          exception?)
(runtime-add-primitive '%exception-type      exception-type)
(runtime-add-primitive '%exception-message   exception-message)
(runtime-add-primitive '%exception-irritants exception-irritants)
(runtime-add-primitive '%exception-handler   exception-handler)
(runtime-add-primitive '%exception-handler-set! exception-handler-set!)
(runtime-add-primitive '%procedure?          procedure?)
(runtime-add-primitive '%symbol?             symbol?)
(runtime-add-primitive '%string?             string?)
(runtime-add-primitive '%string-cmp          string-cmp)

(runtime-add-primitive '%eq?    eq?)
(runtime-add-primitive '%eqv?   eqv?)
(runtime-add-primitive '%equal? equal?)

(runtime-add-primitive '%number?    number?)
(runtime-add-primitive '%exact?     exact?)
(runtime-add-primitive '%inexact?   inexact?)
(runtime-add-primitive '%exact      exact)
(runtime-add-primitive '%inexact    inexact)
(runtime-add-primitive '%infinite?  infinite?)
(runtime-add-primitive '%finite?    finite?)
(runtime-add-primitive '%nan?       nan?)
(runtime-add-primitive '%floor      floor)
(runtime-add-primitive '%ceiling    ceiling)
(runtime-add-primitive '%truncate   truncate)
(runtime-add-primitive '%quotient   quotient)
(runtime-add-primitive '%remainder  remainder)
(runtime-add-primitive '%round      round)
(runtime-add-primitive '%sqrt       sqrt)
(runtime-add-primitive '%expt       expt)

(runtime-add-primitive '%command-line          loki-command-line)
(runtime-add-primitive '%environment-variables ''())
(runtime-add-primitive '%emergency-exit        exit) ; TODO - this sucks

(runtime-add-primitive '%port?                   loki-port?)
(runtime-add-primitive '%eof-object              loki-eof-object)
(runtime-add-primitive '%eof-object?             loki-eof-object?)
(runtime-add-primitive '%port-input              loki-port-input)
(runtime-add-primitive '%port-output             loki-port-output)
(runtime-add-primitive '%port-type               loki-port-type)
(runtime-add-primitive '%port-ready?             loki-port-ready?)
(runtime-add-primitive '%input-port-open?        loki-input-port-open?)
(runtime-add-primitive '%output-port-open?       loki-output-port-open?)
(runtime-add-primitive '%close-input-port        loki-close-input-port)
(runtime-add-primitive '%close-output-port       loki-close-output-port)
(runtime-add-primitive '%delete-file             delete-file)
(runtime-add-primitive '%file-exists?            file-exists?)
(runtime-add-primitive '%get-output-string       loki-get-output-string)
(runtime-add-primitive '%get-output-bytevector   loki-get-output-bytevector)
(runtime-add-primitive '%open-output-string      loki-open-output-string)
(runtime-add-primitive '%open-input-string       loki-open-input-string)
(runtime-add-primitive '%open-input-bytevector   loki-open-input-bytevector)
(runtime-add-primitive '%open-output-bytevector  loki-open-output-bytevector)
(runtime-add-primitive '%open-output-file        loki-open-output-file)
(runtime-add-primitive '%open-input-file         loki-open-input-file)
(runtime-add-primitive '%open-binary-input-file  loki-open-binary-input-file)
(runtime-add-primitive '%open-binary-output-file loki-open-binary-output-file)
(runtime-add-primitive '%stderr                  loki-stderr)
(runtime-add-primitive '%stdin                   loki-stdin)
(runtime-add-primitive '%stdout                  loki-stdout)
(runtime-add-primitive '%flush-output-port       loki-flush-output-port)

(runtime-add-primitive '%peek-char        loki-peek-char)
(runtime-add-primitive '%peek-u8          loki-peek-u8)
(runtime-add-primitive '%read-bytevector! loki-read-bytevector!)
(runtime-add-primitive '%read-bytevector! loki-read-bytevector!)
(runtime-add-primitive '%read-bytevector  loki-read-bytevector)
(runtime-add-primitive '%read-string      loki-read-string)
(runtime-add-primitive '%read-char        loki-read-char)
(runtime-add-primitive '%read-line        loki-read-line)
(runtime-add-primitive '%read-u8          loki-read-u8)
(runtime-add-primitive '%write-bytevector loki-write-bytevector)
(runtime-add-primitive '%write-string     loki-write-string)
(runtime-add-primitive '%write-char       loki-write-char)
(runtime-add-primitive '%write-u8         loki-write-u8)

(runtime-add-primitive '%current-jiffy         current-jiffy)
(runtime-add-primitive '%current-second        current-second)
(runtime-add-primitive '%jiffies-per-second    jiffies-per-second)

(runtime-add-primitive '%apply apply)
(runtime-add-primitive '%values values)
(runtime-add-primitive '%call-with-values call-with-values)
(runtime-add-primitive '%call/cc call/cc)
(runtime-add-primitive '%hash-by-identity hash-by-identity)
(runtime-add-primitive '%current-directory current-directory)
(runtime-add-primitive '%trace trace)

))
