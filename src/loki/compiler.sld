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
      %number? %finite? %infinite?
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
  (map (lambda (exp) (normalize exp (lambda (x) x))) exps))

(define (normalize exp k)
  (let ((out (match exp

    (('begin exp . exp*)
     (k `(begin ,(normalize-term exp)
                ,@(normalize-terms exp*))))

     (('let () exp . exp*)
      (if (null? exp*)
        (normalize exp k)
        (k `(let ()
          ,(normalize-term exp)
          ,@(normalize-terms exp*)))))

     (('let ((formal value) . clause) exp . exp*) 
      (normalize value (lambda (aexp-value) 
       `(let ((,formal ,aexp-value))
         ,(normalize `(let (,@clause) ,exp ,@exp*) k)))))

    (('if exp1 exp2 exp3)    
      (normalize-name exp1 (lambda (t) 
       (k `(if ,t ,(normalize-term exp2) 
                  ,(normalize-term exp3))))))
    (('if exp1 exp2)    
      (normalize `(if ,exp1 ,exp2 %void) k))


    (('lambda params body . body*)   
      (k `(lambda ,params
        ,(normalize-term body)
        ,@(normalize-terms body*))))
    
    (('set! v exp)
      (normalize-name exp (lambda (t)
        (k `(set! ,v ,t)))))

    (('define v exp)
      (k `(define ,v ,(normalize-term exp))))

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

; TODO - normalize the position of defines
; types of defines -
; define in library (possible exports)
; define at toplevel
; define in body (local defines)

(define (map-terms terms matcher)
  (map (lambda (term) (map-term term matcher)) terms))

(define (map-term term matcher)
  (let ((match? (matcher term)))
    (if match?
        match?
        (match term
          (('begin exp . exp*)
            `(begin ,(map-term exp matcher)
                    ,@(map-terms exp* matcher)))
          (('let () exp . exp*)
            `(let () ,(map-term exp matcher)
                    ,@(map-terms exp* matcher)))
          (('let ((formal value)) exp . exp*)
            `(let (,formal ,(map-term value matcher))
              ,(map-term exp matcher)
              ,@(map-terms exp* matcher)))
          (('if exp1 exp2 exp3)
            `(if ,(map-term exp1 matcher)
                 ,(map-term exp2 matcher)
                 ,(map-term exp3 matcher)))
          (('lambda params body . body*)
            `(lambda ,params
              ,(map-term body matcher)
              ,@(map-terms body* matcher)))
          (('set! v exp)
            `(set! ,v ,(map-term exp matcher)))
          (('define v exp)
            `(define ,v ,(map-term exp matcher)))
          ((? atomic?) term)
          ((f . e*) 
            `(,(map-term f matcher) ,@(map-terms e* matcher)))))))

(define (find-lets terms)
  (map-terms terms
    (lambda (term)
      (match term
        (('let ((formal value)) . body*)
          (debug "formal!" formal)
          term)
        (else #f)))))

(define (compile-terms terms)
  (let ((anf (normalize-terms terms)))
    (find-lets terms)
    anf))

))
