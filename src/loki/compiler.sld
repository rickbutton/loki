(define-library (loki compiler)
(import (scheme base))
(import (scheme time))
(import (loki util))
(import (loki runtime))
(import (loki match))
(export compiler-intrinsics compile generate-guid)
(begin

(define compiler-intrinsics '(
      %void %blackhole
      %add %sub %mul %div
      %lt %lte %number-eq %gt %gte
      %number? %finite? %infinite?
      %nan? %floor %ceiling %truncate
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
      %read-line %read-u8 %write-bytevector %write-string
      %write-char %write-u8
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
;; anormalize: A simple A-Normalizer for a subset of Scheme

;; Author: Matt Might
;; Site:   http://matt.might.net/

;; Input language:

;; <prog> ::= <dec> ...

;; <dec> ::= (define <var> <exp>)
;;        |  <exp>

;; <f>   ::= <var> | (<var> ...) | (<var> ... . <var>)
;; <exp> ::= (begin <exp> <exp> ...)
;;        |  (let ((<var> <exp>) ...) <exp> ...)
;;        |  (if <exp> <exp> <exp>)
;;        |  (lambda <f> <exp> ...)
;;        |  (set! <var> <exp>)
;;        |  (define <var> <exp>)
;;        |  (quote <exp>)
;;        |  (<exp> <exp> ...)
;;        |  <number>
;;        |  <boolean>
;;        |  <string>
;;        |  <var>


;; Output language:


;; <aexp> ::= (lambda (<name> ...) <exp>)
;;         |  (set! <var> <var>)
;;         |  (quote exp)
;;         |  <number>
;;         |  <boolean>
;;         |  <string>
;;         |  <var>

;; <cexp> ::= (<aexp> <aexp> ...)
;;         |  (if <aexp> <exp> <exp>)

;; <exp>  ::= (let ((<var> <cexp>)) <exp> ...)
;;         |  (begin <exp> <exp> ...)
;;         |  <aexp>
;;         |  <cexp>

;; <prog> ::= <exp> ...

(define (atomic? exp)
  (match exp
    (('quote _)          #t)
    ((? number?)         #t)
    ((? boolean?)        #t)
    ((? string?)         #t)
    ((? char?)           #t)
    ((? symbol?)         #t)
    ((? intrinsic?)      #t)
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
        (k `(begin
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

    (('lambda params body . body*)   
      (k `(lambda ,params
        ,(normalize-term body)
        ,@(normalize-terms body*))))
    
    (('set! v exp)
      (normalize-name exp (lambda (t)
        (k `(set! ,v ,t)))))

    (('define v exp)
      (normalize-name exp (lambda (t)
        (k `(set! ,v ,t)))))

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


(define (flatten-top exp v)
  (match exp
    (('let ((x cexp)) exp)
     (cons `(define ,x ,cexp) 
            (flatten-top exp v)))
    
    (else
     `((define ,v ,exp)))))


(define (normalize-program decs)
  (match decs
    ('() 
     '())
    
    ((exp . rest)
     (cons (normalize-term exp)
           (normalize-program rest)))))

(define (compile library)
  (let* ((forms (rt:library-forms library))
         (normalized (normalize-program forms)))
    (debug "forms" forms)
    (debug "normalized" normalized)
    forms))

))
