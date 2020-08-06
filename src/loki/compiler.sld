(define-library (loki compiler)
(import (scheme base))
(import (scheme time))
(import (loki util))
(import (loki match))
(export compiler-intrinsics compile-terms generate-guid)
(begin

(define compiler-intrinsics '(
      %trace %pop-trace %call-with-values %values

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
      %procedure-name-set!

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

(define compiler-primitives '(begin if let lambda quote set! define))

; anf transform based on http://matt.might.net/articles/a-normalization/

(define void (if #f #f))
(define (void? obj) (eq? void obj))
(define (atomic? exp)
  (match exp
    (('quote _)          #t)
    ((? number?)         #t)
    ((? boolean?)        #t)
    ((? string?)         #t)
    ((? char?)           #t)
    ((? symbol?)         #t)
    ((? intrinsic?)      #t)
    ((? void?)           #t)
    ((? procedure?)      #t)
    ((? vector?)         #t)
    ((? bytevector?)     #t)
    (else                #f)))


;; Expression normalization:
(define (normalize-term exp) (normalize exp (lambda (x) x)))

(define (normalize-terms exps)
  (let loop ((exps exps)
             (terms '())
             (defines '()))
    (if (null? exps)
        (if (null? defines)
          (reverse terms)
          `((letrec (,@(reverse defines)) ,@(reverse terms))))
        (let ((exp (car exps)))
          (match exp
            (('define name value)
              (loop (cdr exps) terms (cons (list name (normalize-term value)) defines)))
            (('begin . exp*)
              (loop (append exp* (cdr exps)) terms defines))
            (else (loop (cdr exps) (cons (normalize-term exp) terms) defines)))))))
          

(define (normalize exp k)
  (let ((out (match exp

    (('begin exp . exp*)
     (k `(begin ,@(normalize-terms (cons exp exp*)))))

     (('let () exp . exp*)
      (if (null? exp*)
        (normalize exp k)
        (k `(let () ,@(normalize-terms (cons exp exp*))))))

     (('let ((formal value) . clause) exp . exp*) 
      (normalize value (lambda (aexp-value) 
       `(letrec ((,formal ,aexp-value))
         ,(normalize `(let (,@clause) ,exp ,@exp*) k)))))

    (('if exp1 exp2 exp3)    
      (normalize-name exp1 (lambda (t) 
       (k `(if ,t ,(normalize-term exp2) 
                  ,(normalize-term exp3))))))
    (('if exp1 exp2)    
      (normalize `(if ,exp1 ,exp2 %void) k))


    (('lambda params body . body*)   
      (k `(lambda ,params
        ,@(normalize-terms (cons body body*)))))
    
    (('set! v exp)
      (normalize-name exp (lambda (t)
        (k `(set! ,v ,t)))))

    (('define-global v exp) (k `(define ,v ,(normalize-term exp))))

    ((? atomic?)            
     (k exp))

    ((f . e*) 
      (normalize-name f (lambda (t) 
       (normalize-name* e* (lambda (t*)
        (k `(,t . ,t*))))))))))
  out))
    

(define (normalize-name exp k)
  (normalize exp (lambda (aexp) 
    (if (atomic? aexp) (k aexp) 
        (let ((t (generate-guid 't))) 
         `(let ((,t ,aexp)) ,(k t)))))))

(define (normalize-name* exp* k)
  (if (null? exp*)
      (k '())
      (normalize-name (car exp*) (lambda (t) 
       (normalize-name* (cdr exp*) (lambda (t*) 
        (k `(,t . ,t*))))))))

(define (compile-terms terms)
  (let ((anf (normalize-terms terms)))
    anf))

))
