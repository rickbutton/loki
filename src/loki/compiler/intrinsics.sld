(define-library (loki compiler intrinsics)
(import (scheme base))
(export compiler-intrinsics intrinsic?)
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
      %cons %pair? %null? %list? %length %car %cdr
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
      %read-line %read-u8 %write-bytevector %write-string
      %write-char %write-u8 %current-directory
      %hash-by-identity %current-jiffy %current-second %jiffies-per-second
      %number->string %string->number

      ex:identifier?
      ex:bound-identifier=?
      ex:free-identifier=?
      ex:generate-temporaries
      ex:datum->syntax
      ex:syntax->datum
      ex:syntax->source
      ex:source-file
      ex:source-line
      ex:source-column
      ex:environment
      ex:eval
      ex:load
      ex:syntax-violation
      ex:features

      ex:invalid-form
      ex:register-macro!
      ex:syntax-rename
      ex:map-while
      ex:dotted-length
      ex:dotted-butlast
      ex:dotted-last
      ex:free=?))


(define (intrinsic? i)
  (member i compiler-intrinsics))

))
