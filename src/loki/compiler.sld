(define-library (loki compiler)
(import (scheme base))
(import (scheme time))
(import (loki util))
(import (loki match))
(export compiler-intrinsics intrinsic? generate-guid)
(begin

(define compiler-intrinsics '(
      %trace %call-with-values %values

      %void %blackhole
      %add %sub %mul %div
      %lt %lte %number-eq %gt %gte
      %bit-not %bit-and %bit-ior   
      %bit-xor %bit-shift %bit-count 
      %bit-length
      %number? %exact? %inexact?
      %exact %inexact %finite? %infinite?
      %nan? %floor %ceiling %truncate
      %remainder %quotient
      %round %sqrt %expt
      %cons %pair? %null? %list? %car %cdr
      %set-car! %set-cdr!
      %vector? %vector-set! %vector-ref
      %vector-length %make-vector
      %bytevector %bytevector-u8-ref %bytevector-u8-set!
      %bytevector-length %make-bytevector %bytevector?
      %char->integer %integer->char %char-foldcase
      %char-upcase %char-downcase %char? %call/cc %apply
      %string-set! %string-ref %make-string %string-length
      %string-downcase %string-upcase %string-foldcase
      %string->symbol %symbol->string %string-cmp
      %abort %make-exception %exception? %exception-type
      %exception-message %exception-irritants %exception-handler
      %exception-handler-set!
      %procedure? %symbol? %string? %eq? %eqv? %equal?
      %command-line %environment-variables %emergency-exit

      %port? %eof-object %eof-object? %port-input %port-output
      %port-type %port-ready? %input-port-open? %output-port-open?
      %close-input-port %close-output-port %delete-file %file-exists?
      %get-output-string %get-output-bytevector
      %open-output-string %open-input-string %open-input-bytevector
      %open-output-bytevector %open-output-file %open-input-file
      %open-binary-input-file %open-binary-output-file
      %stderr %stdin %stdout %flush-output-port
      %peek-u8 %peek-char
      %read-bytevector! %read-bytevector %read-string %read-char
      %read-line %read-u8 %write-bytevector %write-string %repr %debug
      %write-char %write-u8 %current-directory
      %hash-by-identity %current-jiffy %current-second %jiffies-per-second
      %number->string %string->number))

(define (intrinsic? i)
  (member i compiler-intrinsics))

;; Generate-guid returns a fresh symbol that has a globally
;; unique external representation and is read-write invariant.
;; Your local gensym will probably not satisfy both conditions.
;; Prefix makes it disjoint from all builtins.
;; Uniqueness is important for incremental and separate expansion.

(define guid-prefix "&")
(define (unique-token)
  (number->string (current-jiffy) 32))
(define generate-guid
  (let ((token (unique-token))
        (ticks 0))
    (lambda (symbol)
      (set! ticks (+ ticks 1))
      (string->symbol
       (string-append guid-prefix
                      (symbol->string symbol)
                      "~"
                      token
                      "~"
                      (number->string ticks))))))

))
