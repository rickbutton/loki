(define-library (loki compiler)
(import (scheme base))
(import (loki util))
(import (loki runtime))
(import (loki match))
(export compiler-intrinsics compile)
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

(define compiler-primitives '(begin if lambda quote set! define))

(define (intrinsic? i)
  (if (member i compiler-intrinsics) #t #f))

(define rvid (make-anon-id "$rv_"))
(define kid (make-anon-id "$k_"))

(define (constant? x)
    (or
        (boolean? x)
        (char? x)
        (null? x)
        (symbol? x)
        ;(bytevector? x)
        (number? x)
        (string? x)
        ;(vector? x)
        ))

(define (quote? x) (and (list? x) (eq? (car x) 'quote)))

; cps conversion gleefully lifted from:
; http://matt.might.net/articles/cps-conversion/
(define (aexpr? expr)
    (match expr
        ((or ('lambda (_ ...) _)
             'call/cc
             (? symbol?)
             (? intrinsic?)
             (? quote?)
             (? constant?)) #t)
        (else #f)))

(define (T-k expr k)
    (match expr
        ((? aexpr?) (k (M expr)))
        (('begin expr) 
            (T-k expr k))
        (('begin expr exprs ...) 
            (T-k expr (lambda (_) (T-k `(begin ,@exprs) k))))
        (('if expr-cond expr-cons expr-alt)
            (let* (($rv (rvid)) (cont `(lambda (,$rv) ,(k $rv))))
                (T-k expr-cond (lambda (aexp)
                    `(if ,aexp
                         ,(T-c expr-cons cont)
                         ,(T-c expr-alt cont))))))
        (('set! var expr) (T-k expr (lambda (aexp) 
            (k `(set! ,var ,aexp)))))
        (('define var expr) (T-k expr (lambda (aexp) 
            (k `(define ,var ,aexp)))))
        ((_ _ ...)
            (let* (($rv (rvid)) (cont `(lambda (,$rv) ,(k $rv))))
                (T-c expr cont)))))

(define (T-c expr c)
    (match expr
        ((? aexpr?) `(,c ,(M expr)))
        (('begin expr) (T-c expr c))
        (('begin expr exprs ...)
            (T-k expr (lambda (_) (T-c `(begin ,@exprs) c))))
        (('if expr-cond expr-cons expr-alt)
            (let (($k (kid)))
                `((lambda (,$k)
                    ,(T-k expr-cond (lambda (aexp) 
                        `(if ,aexp
                             ,(T-c expr-cons $k)
                             ,(T-c expr-alt $k)))))
                ,c)))
        (('set! var expr) (T-k expr (lambda (aexp) 
            `(,c (set! ,var ,aexp)))))
        (('define var expr) (T-k expr (lambda (aexp) 
            `(,c (define ,var ,aexp)))))
        (((? intrinsic? i) es ...)
            (T*-k es (lambda ($es)
                `(intrinsic ,i ,@$es ,c))))
        ((f es ...)
            (T-k f (lambda ($f)
                (T*-k es (lambda ($es)
                    `(,$f ,@$es ,c))))))))

(define (T*-k exprs k)
    (cond
        ((null? exprs) (k '()))
        ((pair? exprs) (T-k (car exprs) 
            (lambda (hd) (T*-k (cdr exprs) 
                (lambda (tl) (k (cons hd tl)))))))))

; for call/cc
; TODO - clean this up?
(define fid (make-anon-id "$f_"))
(define ccid (make-anon-id "$cc_"))
(define xid (make-anon-id "$x_"))
(define emptyid (make-anon-id "$__"))

(define (M aexpr)
    (match aexpr
        (('lambda (vars ...) body)
            (let (($k (kid)))
                `(lambda (,@vars ,$k) ,(T-c body $k))))
        ('%call/cc 
            (let ((f (fid))
                  (cc (ccid))
                  (x (xid))
                  (_ (emptyid)))
                `(lambda (,f ,cc) (,f (lambda (,x ,_) (,cc ,x)) ,cc))))
        ((? constant?) aexpr)
        ((? symbol?) aexpr)
        ((? quote?) aexpr)
        (else (error "invalid aexpr" aexpr))))

(define (scheme2cps x) (T-c x '(%blackhole)))

(define (compile library)
  (let ((forms (rt:library-forms library)))
    (debug forms)
    (scheme2cps forms)))

))
