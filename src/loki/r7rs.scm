;;;
;;; This set of standard libraries is a mixture of r6rs libraries from the
;;; SRFI-72 reference implementation and ones written directly for r7rs.
;;;
;;; Original License:
;;;=====================================================================
;;;
;;; Derived forms:
;;;
;;;   Copyright (c) 2006 Andre van Tonder
;;;
;;;   Copyright statement at http://srfi.schemers.org/srfi-process.html
;;;
;;;=====================================================================  

;;;=====================================================================
;;;                  |---- sike!
;;;                  V
;;; This file builds r6rs up using a sequence of libraries.
;;; It constitutes a nontrivial example, tutorial and test
;;; of the library system.  
;;;
;;; It is meant to be expanded by expander.scm and compiled 
;;; together with the latter before using in a production system.
;;;
;;; Various of the standard macros were copied from
;;; SRFI-93 reference implementation.
;;;
;;; An explicit renaming library is included for easier 
;;; porting of legacy macros in some implementations.
;;;
;;;=====================================================================
(define-library (core primitives)
  
  (export
   
   ;; Macros defined in core expander:
   
   begin if lambda quote set! and or
   define define-syntax let-syntax letrec-syntax
   include include-ci
   _ ... syntax syntax-case
      
   ;; Procedures and values defined in core expander:
   
   (rename (ex:make-variable-transformer make-variable-transformer)
           (ex:identifier?               identifier?)
           (ex:bound-identifier=?        bound-identifier=?)
           (ex:free-identifier=?         free-identifier=?)
           (ex:generate-temporaries      generate-temporaries) 
           (ex:datum->syntax             datum->syntax)
           (ex:syntax->datum             syntax->datum)
           (ex:syntax-violation          syntax-violation)
           (ex:features                  features)
           (ex:environment               environment)
           (ex:environment-bindings      environment-bindings)
           (ex:eval                      eval)
           (ex:load                      load))
   void)
  
  (import
   
   (for (only (core primitive-macros)
     
     begin if set! and or lambda quote
     define define-syntax let-syntax letrec-syntax 
     include include-ci
     syntax syntax-case _ ...) run expand)
   
   ;; An extension to the import syntax, used here to make  
   ;; available variable bindings provided natively.
   ;; This will not work for macros, which have to be defined
   ;; within the context of this expander.  
   
   (primitives
   
    ;; Procedures and values defined in the core expander:
    
    ex:make-variable-transformer ex:identifier? ex:bound-identifier=?
    ex:free-identifier=? ex:generate-temporaries ex:datum->syntax ex:syntax->datum 
    ex:syntax-violation ex:environment ex:environment-bindings ex:eval ex:load ex:features
    ))
  (begin
      (define void (if #f #f))
  )) ;; core primitives

(define-library (core intrinsics)
    (import (core primitives))
    (import (primitives 
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
                        %abort %make-exception %exception? %exception-type
                        %exception-message %exception-irritants
                        %procedure? %symbol? %eq? %eqv? %equal?

                        ; ports
                        binary-port?
                        call-with-port
                        char-ready? 
                        close-input-port
                        close-output-port
                        close-port
                        current-error-port
                        current-input-port
                        current-output-port
                        eof-object? 
                        flush-output-port
                        input-port? 
                        output-port?
                        peek-u8
                        read-bytevector!
                        read-string
                        eof-object
                        get-output-bytevector
                        input-port-open?
                        output-port-open?
                        peek-char
                        port?
                        read-bytevector
                        read-char
                        read-line
                        read-u8
                        write-char
                        write-u8
                        textual-port?
                        write-bytevector
                        write-string
                        u8-ready?
                        newline

                        ; synthetic ports
                        get-output-string
                        open-output-string
                        open-input-string
                        open-input-bytevector
                        open-output-bytevector
                      
                        ; strings
                        number->string
                        string
                        string->number
                        string->utf8
                        string-append
                        list->string
                        make-string
                        string->list
                        string->symbol
                        string-copy
                        string-copy!
                        string-for-each
                        string-map
                        string-set! 
                        string<?
                        string>=?
                        string? 
                        symbol->string
                        string-fill!
                        string-length
                        string-ref
                        string<=?
                        string=?
                        string>?
                        substring
                        utf8->string
                        ))
    (export 
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
          %abort %make-exception %exception? %exception-type
          %exception-message %exception-irritants
          %procedure? %symbol? %eq? %eqv? %equal?


          binary-port? call-with-port
          char-ready?  close-input-port
          close-output-port close-port
          current-error-port current-input-port current-output-port
          eof-object?
          flush-output-port get-output-string
          input-port?
          number->string
          open-input-bytevector open-output-bytevector output-port?
          peek-u8 read-bytevector!  read-string
          string string->number string->utf8 string-append
          eof-object
          get-output-bytevector input-port-open?
          list->string make-string
          newline open-input-string
          open-output-string output-port-open?  peek-char port?
          read-bytevector read-char read-line
          read-u8 string->list string->symbol
          string-copy string-copy!  string-for-each string-map
          string-set!  string<?  string>=?  string?  symbol->string
          u8-ready?  utf8->string
          write-char write-u8 string-fill!  string-length string-ref string<=?
          string=?  string>?  substring textual-port?
          write-bytevector write-string))

(define-library (core apply)
  (export (rename (%apply apply)))
  (import (core intrinsics)))

(define-library (core with-syntax)
  (export with-syntax)
  (import (for (core primitives) run expand)
          (for (core intrinsics) run expand))
  (begin
  
  (define (list . args) args)
  (define-syntax with-syntax
    (lambda (x)
      (syntax-case x ()
        ((_ ((p e0) ...) e1 e2 ...)
         (syntax (syntax-case (list e0 ...) ()
                   ((p ...) (begin e1 e2 ...)))))))) 
  ))

(define-library (core syntax-rules)
  (export syntax-rules)
  (import (for (core primitives)        expand run)
          (for (core with-syntax)       expand run)
          (for (core intrinsics)        expand))
  (begin

    (define-syntax syntax-rules
      (lambda (x)
        (syntax-case x ()
          ((_ (k ...) (pattern template) ...)
           (syntax (lambda (x)
                    (syntax-case x (k ...)
                      (pattern (syntax template))
                      ...)))))))
  
  ))

(define-library (core let)
  (export let letrec letrec*)
  (import (for (core primitives)        expand run)
          (for (core syntax-rules)      expand run))
  (begin
  
  (define-syntax let
    (syntax-rules ()
      ((let ((name val) ...) body1 body2 ...)
       ((lambda (name ...) body1 body2 ...)
        val ...))
      ((let tag ((name val) ...) body1 body2 ...)
       ((letrec ((tag (lambda (name ...)
                        body1 body2 ...)))
          tag)
        val ...))))
  
  (define-syntax letrec
    (syntax-rules ()
      ((letrec ((var1 init1) ...) body ...)
       (letrec "generate_temp_names"
         (var1 ...)
         ()
         ((var1 init1) ...)
         body ...))
      ((letrec "generate_temp_names"
         ()
         (temp1 ...)
         ((var1 init1) ...)
         body ...)
       (let ((var1 void) ...)
         (let ((temp1 init1) ...)
           (set! var1 temp1)
           ...
           body ...)))
      ((letrec "generate_temp_names"
         (x y ...)
         (temp ...)
         ((var1 init1) ...)
         body ...)
       (letrec "generate_temp_names"
         (y ...)
         (newtemp temp ...)
         ((var1 init1) ...)
         body ...))))
  
  (define-syntax letrec*
    (syntax-rules ()
      ((letrec* ((var1 init1) ...) body1 body2 ...)
       (let ((var1 void) ...)
         (set! var1 init1)
         ...
         (let () body1 body2 ...)))))
  
  )) ; let

(define-library (core define-missing)
  (import (core primitives))
  (import (core intrinsics))
  (import (core exception))
  (import (for (core syntax-rules) expand))
  (export define-missing)
  (begin
    (define-syntax define-missing
      (syntax-rules ()
        ((_ name)
          (define (name . args)
              (raise (string-append "not implemented: "
                                    (symbol->string 'name) "\n"))))
        ((_ name names ...)
          (begin
            (define-missing name)
            (define-missing names ...)))))))

(define-library (core case-lambda)
  (export case-lambda)
  (import (for (core primitives)   expand run)
          (for (core let)          expand run)
          (for (core apply)        expand run)
          (for (core syntax-rules) expand)
          (for (core list)         expand run)
          (for (core exception)    expand run)
          (for (core intrinsics)   expand run))
  (begin

  (define-syntax case-lambda
    (syntax-rules ()
      ((_ (fmls b1 b2 ...))
       (lambda fmls b1 b2 ...))
      ((_ (fmls b1 b2 ...) ...)
       (lambda args
         (let ((n (length args)))
           (case-lambda-help args n
                             (fmls b1 b2 ...) ...))))))
  
  (define-syntax case-lambda-help
    (syntax-rules ()
      ((_ args n)
       (error "unexpected number of arguments"))
      ((_ args n ((x ...) b1 b2 ...) more ...)
       (if (%number-eq n (length '(x ...)))
           (apply (lambda (x ...) b1 b2 ...) args)
           (case-lambda-help args n more ...)))
      ((_ args n ((x1 x2 ... . r) b1 b2 ...) more ...)
       (if (%gte n (length '(x1 x2 ...)))
           (apply (lambda (x1 x2 ... . r) b1 b2 ...)
                  args)
           (case-lambda-help args n more ...)))
      ((_ args n (r b1 b2 ...) more ...)
       (apply (lambda r b1 b2 ...) args))))                                      
))

(define-library (core control)
  (export when unless do)
  (import (for (core primitives)   expand run)
          (for (core let)          expand run)
          (for (core with-syntax)  expand)
          (for (core syntax-rules) expand)
          (for (core bool)         expand run)
          (for (core list)         expand run)
          (for (core intrinsics) expand run))
  (begin
  
  (define-syntax when
    (syntax-rules ()
      ((when test result1 result2 ...)
       (if test
           (begin result1 result2 ...)))))
  
  (define-syntax unless
    (syntax-rules ()
      ((unless test result1 result2 ...)
       (if (not test)
           (begin result1 result2 ...)))))
  
  (define-syntax do
    (lambda (orig-x)
      (syntax-case orig-x ()
        ((_ ((var init . step) ...) (e0 e1 ...) c ...)
         (with-syntax (((step ...)
                        (map (lambda (v s)
                               (syntax-case s ()
                                 (()  v)
                                 ((e) (syntax e))
                                 (_   (syntax-violation 'do "Invalid step" orig-x s))))
                             (syntax (var ...))
                             (syntax (step ...)))))
           (syntax-case (syntax (e1 ...)) ()
             (()          (syntax (let do ((var init) ...)
                                    (if (not e0)
                                        (begin c ... (do step ...))))))
             ((e1 e2 ...) (syntax (let do ((var init) ...)
                                    (if e0
                                        (begin e1 e2 ...)
                                        (begin c ... (do step ...))))))))))))                         

  
  
  )) ; core control                                      

(define-library (core derived)
  (export let* cond case else =>)   
  (import (for (core primitives)       expand run)
          (for (core let)              expand run)
          (for (core with-syntax)      expand)
          (for (core syntax-rules)     expand)
          (for (core list)             expand run)
          (for (core intrinsics)       expand run))
  (begin
  
  
  (define-syntax let*
    (syntax-rules ()
      ((let* () body1 body2 ...)
       (let () body1 body2 ...))
      ((let* ((name1 val1) (name2 val2) ...)
         body1 body2 ...)
       (let ((name1 val1))
         (let* ((name2 val2) ...)
           body1 body2 ...)))))

  
  (define-syntax cond
    (syntax-rules (else =>)
      ((cond (else result1 result2 ...))
       (begin result1 result2 ...))
      ((cond (test => result))
       (let ((temp test))
         (if temp (result temp))))
      ((cond (test => result) clause1 clause2 ...)
       (let ((temp test))
         (if temp
             (result temp)
             (cond clause1 clause2 ...))))
      ((cond (test)) test)
      ((cond (test) clause1 clause2 ...)
       (let ((temp test))
         (if temp
             temp
             (cond clause1 clause2 ...))))
      ((cond (test result1 result2 ...))
       (if test (begin result1 result2 ...)))
      ((cond (test result1 result2 ...)
             clause1 clause2 ...)
       (if test
           (begin result1 result2 ...)
           (cond clause1 clause2 ...)))))

  (define-syntax case
    (syntax-rules (else =>)
      ((case (key ...)
         clauses ...)
       (let ((atom-key (key ...)))
         (case atom-key clauses ...)))
      ((case key
         (else => result))
       (result key))
      ((case key
         (else result1 result2 ...))
       (begin result1 result2 ...))
      ((case key
         ((atoms ...) result1 result2 ...))
       (if (memv key '(atoms ...))
           (begin result1 result2 ...)))
      ((case key
         ((atoms ...) => result))
       (if (memv key '(atoms ...))
           (result key)))
      ((case key
         ((atoms ...) => result)
         clause clauses ...)
       (if (memv key '(atoms ...))
           (result key)
           (case key clause clauses ...)))
      ((case key
         ((atoms ...) result1 result2 ...)
         clause clauses ...)
       (if (memv key '(atoms ...))
           (begin result1 result2 ...)
           (case key clause clauses ...)))))  

  (define-syntax =>
    (lambda (x)
      (syntax-violation '=> "Invalid expression" x)))
  
  (define-syntax else
    (lambda (x)
      (syntax-violation 'else "Invalid expression" x)))
  
  )) ; derived

(define-library (core records)
  (import (for (core primitives)   expand run)
          (for (core let)          expand run)
          (for (core derived)      expand run)
          (for (core syntax-rules) expand run)
          (for (core number)       expand run)
          (for (core bool)         expand run)
          (for (core list)         expand run)
          (for (core vector)       expand run)
          (for (core exception)    expand run)
          (for (core intrinsics)   expand run))
  (export define-record-type vector?)
  (begin

    ; This implements a record abstraction that is identical to vectors,
    ; except that they are not vectors (VECTOR? returns false when given a
    ; record and RECORD? returns false when given a vector).  The following
    ; procedures are provided:
    ;   (record? <value>)                -> <boolean>
    ;   (make-record <size>)             -> <record>
    ;   (record-ref <record> <index>)    -> <value>
    ;   (record-set! <record> <index> <value>) -> <unspecific>
    ;
    ; These can implemented in R5RS Scheme as vectors with a distinguishing
    ; value at index zero, providing VECTOR? is redefined to be a procedure
    ; that returns false if its argument contains the distinguishing record
    ; value.  EVAL is also redefined to use the new value of VECTOR?.
    
    ; Define the marker and redefine VECTOR? and EVAL.
    
    (define record-marker (list 'record-marker))
    
    (define (vector? x)
      (and (%vector? x)
           (or (= 0 (vector-length x))
    	   (not (eq? (vector-ref x 0)
    		record-marker)))))

    ; Definitions of the record procedures.
    
    (define (record? x)
      (and (%vector? x)
           (< 0 (vector-length x))
           (eq? (vector-ref x 0)
                record-marker)))
    
    (define (make-record size)
      (let ((new (make-vector (+ size 1))))
        (vector-set! new 0 record-marker)
        new))
    
    (define (record-ref record index)
      (vector-ref record (+ index 1)))
    
    (define (record-set! record index value)
      (vector-set! record (+ index 1) value))

    ; Definition of DEFINE-RECORD-TYPE
    
    (define-syntax define-record-type
      (syntax-rules ()
        ((define-record-type type
           (constructor constructor-tag ...)
           predicate
           (field-tag accessor . more) ...)
         (begin
           (define type
             (make-record-type 'type '(field-tag ...)))
           (define constructor
             (record-constructor type '(constructor-tag ...)))
           (define predicate
             (record-predicate type))
           (define-record-field type field-tag accessor . more)
           ...))))
    
    ; An auxilliary macro for define field accessors and modifiers.
    ; This is needed only because modifiers are optional.
    
    (define-syntax define-record-field
      (syntax-rules ()
        ((define-record-field type field-tag accessor)
         (define accessor (record-accessor type 'field-tag)))
        ((define-record-field type field-tag accessor modifier)
         (begin
           (define accessor (record-accessor type 'field-tag))
           (define modifier (record-modifier type 'field-tag))))))
    
    ;; Record types
    
    ; We define the following procedures:
    ; 
    ; (make-record-type <type-name <field-names>)    -> <record-type>
    ; (record-constructor <record-type<field-names>) -> <constructor>
    ; (record-predicate <record-type>)               -> <predicate>
    ; (record-accessor <record-type <field-name>)    -> <accessor>
    ; (record-modifier <record-type <field-name>)    -> <modifier>
    ;   where
    ; (<constructor> <initial-value> ...)         -> <record>
    ; (<predicate> <value>)                       -> <boolean>
    ; (<accessor> <record>)                       -> <value>
    ; (<modifier> <record> <value>)         -> <unspecific>
    
    ; Record types are implemented using vector-like records.  The first
    ; slot of each record contains the record's type, which is itself a
    ; record.
    
    (define (record-type record)
      (record-ref record 0))
    
    ;----------------
    ; Record types are themselves records, so we first define the type for
    ; them.  Except for problems with circularities, this could be defined as:
    ;  (define-record-type :record-type
    ;    (make-record-type name field-tags)
    ;    record-type?
    ;    (name record-type-name)
    ;    (field-tags record-type-field-tags))
    ; As it is, we need to define everything by hand.
    
    (define :record-type (make-record 3))
    (record-set! :record-type 0 :record-type)	; Its type is itself.
    (record-set! :record-type 1 ':record-type)
    (record-set! :record-type 2 '(name field-tags))
    
    ; Now that :record-type exists we can define a procedure for making more
    ; record types.
    
    (define (make-record-type name field-tags)
      (let ((new (make-record 3)))
        (record-set! new 0 :record-type)
        (record-set! new 1 name)
        (record-set! new 2 field-tags)
        new))
    
    ; Accessors for record types.
    
    (define (record-type-name record-type)
      (record-ref record-type 1))
    
    (define (record-type-field-tags record-type)
      (record-ref record-type 2))
    
    ;----------------
    ; A utility for getting the offset of a field within a record.
    
    (define (field-index type tag)
      (let loop ((i 1) (tags (record-type-field-tags type)))
        (cond ((null? tags)
               (error "record type has no such field" type tag))
              ((eq? tag (car tags))
               i)
              (else
               (loop (+ i 1) (cdr tags))))))
    
    ;----------------
    ; Now we are ready to define RECORD-CONSTRUCTOR and the rest of the
    ; procedures used by the macro expansion of DEFINE-RECORD-TYPE.
    
    (define (record-constructor type tags)
      (let ((size (length (record-type-field-tags type)))
            (arg-count (length tags))
            (indexes (map (lambda (tag)
                            (field-index type tag))
                          tags)))
        (lambda args
          (if (= (length args)
                 arg-count)
              (let ((new (make-record (+ size 1))))
                (record-set! new 0 type)
                (for-each (lambda (arg i)
    			(record-set! new i arg))
                          args
                          indexes)
                new)
              (error "wrong number of arguments to constructor" type args)))))
    
    (define (record-predicate type)
      (lambda (thing)
        (and (record? thing)
             (eq? (record-type thing)
                  type))))
    
    (define (record-accessor type tag)
      (let ((index (field-index type tag)))
        (lambda (thing)
          (if (and (record? thing)
                   (eq? (record-type thing)
                        type))
              (record-ref thing index)
              (error "accessor applied to bad value" type tag thing)))))
    
    (define (record-modifier type tag)
      (let ((index (field-index type tag)))
        (lambda (thing value)
          (if (and (record? thing)
                   (eq? (record-type thing)
                        type))
              (record-set! thing index value)
              (error "modifier applied to bad value" type tag thing)))))
))

;;;=========================================================
;;;
;;; Quasisyntax in terms of syntax-case.
;;;
;;;=========================================================
;;;
;;; To make nested unquote-splicing behave in a useful way,
;;; the R5RS-compatible extension of quasiquote in appendix B
;;; of the following paper is here ported to quasisyntax:
;;;
;;; Alan Bawden - Quasiquotation in Lisp
;;; http://citeseer.ist.psu.edu/bawden99quasiquotation.html
;;;
;;; The algorithm converts a quasisyntax expression to an
;;; equivalent with-syntax expression.
;;; For example:
;;;
;;; (quasisyntax (set! #,a #,b))
;;;   ==> (with-syntax ((t0 a)
;;;                     (t1 b))
;;;         (syntax (set! t0 t1)))
;;;
;;; (quasisyntax (list #,@args))
;;;   ==> (with-syntax (((t ...) args))
;;;         (syntax (list t ...)))
;;;
;;; Note that quasisyntax is expanded first, before any
;;; ellipses act.  For example:
;;;
;;; (quasisyntax (f ((b #,a) ...))
;;;   ==> (with-syntax ((t a))
;;;         (syntax (f ((b t) ...))))
;;;
;;; so that
;;;
;;; (let-syntax ((test-ellipses-over-unsyntax
;;;               (lambda (e)
;;;                 (let ((a (syntax a)))
;;;                   (with-syntax (((b ...) (syntax (1 2 3))))
;;;                     (quasisyntax
;;;                      (quote ((b #,a) ...))))))))
;;;   (test-ellipses-over-unsyntax))
;;;
;;;     ==> ((1 a) (2 a) (3 a))

(define-library (core quasisyntax)
  (export quasisyntax unsyntax unsyntax-splicing) 
  (import (for (core primitives)  run expand)
          (for (core let)         run expand) 
          (for (core derived)     run expand)
          (for (core with-syntax) run expand)
          (for (core number)      run expand) 
          (for (core vector)      run expand) 
          (for (core intrinsics)  run expand))
  (begin
  
  (define-syntax quasisyntax
    (lambda (e)
      
      ;; Expand returns a list of the form
      ;;    [template[t/e, ...] (replacement ...)]
      ;; Here template[t/e ...] denotes the original template
      ;; with unquoted expressions e replaced by fresh
      ;; variables t, followed by the appropriate ellipses
      ;; if e is also spliced.
      ;; The second part of the return value is the list of
      ;; replacements, each of the form (t e) if e is just
      ;; unquoted, or ((t ...) e) if e is also spliced.
      ;; This will be the list of bindings of the resulting
      ;; with-syntax expression.
      
      (define (expand x level)
        (syntax-case x (quasisyntax unsyntax unsyntax-splicing)
          ((quasisyntax e)
           (with-syntax (((k _)     x) ;; original identifier must be copied
                         ((e* reps) (expand (syntax e) (+ level 1))))
             (syntax ((k e*) reps))))                                  
          ((unsyntax e)
           (= level 0)
           (with-syntax (((t) (generate-temporaries '(t))))
             (syntax (t ((t e))))))
          (((unsyntax e ...) . r)
           (= level 0)
           (with-syntax (((r* (rep ...)) (expand (syntax r) (syntax->datum 0)))
                         ((t ...)        (generate-temporaries (syntax (e ...)))))
             (syntax ((t ... . r*)
                      ((t e) ... rep ...)))))
          (((unsyntax-splicing e ...) . r)
           (= level 0)
           (with-syntax (((r* (rep ...)) (expand (syntax r) 0))
                         ((t ...)        (generate-temporaries (syntax (e ...)))))
             (with-syntax ((((t ...) ...) (syntax ((t (... ...)) ...))))
               (syntax ((t ... ... . r*)
                        (((t ...) e) ... rep ...))))))
          ((k . r)
           (and (> level 0)
                (identifier? (syntax k))
                (or (free-identifier=? (syntax k) (syntax unsyntax))
                    (free-identifier=? (syntax k) (syntax unsyntax-splicing))))
           (with-syntax (((r* reps) (expand (syntax r) (- level 1))))
             (syntax ((k . r*) reps))))
          ((h . t)
           (with-syntax (((h* (rep1 ...)) (expand (syntax h) level))
                         ((t* (rep2 ...)) (expand (syntax t) level)))
             (syntax ((h* . t*)
                      (rep1 ... rep2 ...)))))
          (#(e ...)                                                               
           (with-syntax ((((e* ...) reps)
                          (expand (vector->list (syntax #(e ...))) level)))
             (syntax (#(e* ...) reps))))
          (other
           (syntax (other ())))))
      
      (syntax-case e ()
        ((_ template)
         (with-syntax (((template* replacements) (expand (syntax template) 0)))
           (syntax
            (with-syntax replacements (syntax template*))))))))
  
  (define-syntax unsyntax
    (lambda (e)
      (syntax-violation 'unsyntax "Invalid expression" e)))
  
  (define-syntax unsyntax-splicing
    (lambda (e)
      (syntax-violation 'unsyntax "Invalid expression" e)))
  ))

(define-library (core quasiquote)
  (export quasiquote unquote unquote-splicing)
  (import (for (core primitives)  run expand)
          (for (core let)         run expand) 
          (for (core derived)     run expand)
          (for (core with-syntax) expand)
          (for (core quasisyntax) expand)
          (for (core number)      run expand) 
          (for (core list)        run expand)
          (for (core intrinsics)  run expand))
  (begin
  
  ;; Optimised version copied from portable syntax-case (Dybvig)
  
  (define-syntax quasiquote
    (let ()
      (define (quasi p lev)
        (syntax-case p (unquote quasiquote)
          ((unquote p)
           (if (= lev 0)
               (syntax ("value" p))
               (quasicons (syntax ("quote" unquote)) (quasi (syntax (p)) (- lev 1)))))
          ((quasiquote p) (quasicons (syntax ("quote" quasiquote)) (quasi (syntax (p)) (+ lev 1))))
          ((p . q)
           (syntax-case (syntax p) (unquote unquote-splicing)
             ((unquote p ...)
              (if (= lev 0)
                  (quasilist* (syntax (("value" p) ...)) (quasi (syntax q) lev))
                  (quasicons
                   (quasicons (syntax ("quote" unquote)) (quasi (syntax (p ...)) (- lev 1)))
                   (quasi (syntax q) lev))))
             ((unquote-splicing p ...)
              (if (= lev 0)
                  (quasiappend (syntax (("value" p) ...)) (quasi (syntax q) lev))
                  (quasicons
                   (quasicons (syntax ("quote" unquote-splicing)) (quasi (syntax (p ...)) (- lev 1)))
                   (quasi (syntax q) lev))))
             (_ (quasicons (quasi (syntax p) lev) (quasi (syntax q) lev)))))
          (#(x ...) (quasivector (vquasi (syntax (x ...)) lev)))
          (p (syntax ("quote" p)))))
      (define (vquasi p lev)
        (syntax-case p ()
          ((p . q)
           (syntax-case (syntax p) (unquote unquote-splicing)
             ((unquote p ...)
              (if (= lev 0)
                  (quasilist* (syntax (("value" p) ...)) (vquasi (syntax q) lev))
                  (quasicons
                   (quasicons (syntax ("quote" unquote)) (quasi (syntax (p ...)) (- lev 1)))
                   (vquasi (syntax q) lev))))
             ((unquote-splicing p ...)
              (if (= lev 0)
                  (quasiappend (syntax (("value" p) ...)) (vquasi (syntax q) lev))
                  (quasicons
                   (quasicons
                    (syntax ("quote" unquote-splicing))
                    (quasi (syntax (p ...)) (- lev 1)))
                   (vquasi (syntax q) lev))))
             (_ (quasicons (quasi (syntax p) lev) (vquasi (syntax q) lev)))))
          (() (syntax ("quote" ())))))
      (define (quasicons x y)
        (with-syntax ((x x) (y y))
          (syntax-case (syntax y) ()
            (("quote" dy)
             (syntax-case (syntax x) ()
               (("quote" dx) (syntax ("quote" (dx . dy))))
               (_ (if (null? (syntax dy)) (syntax ("list" x)) (syntax ("list*" x y))))))
            (("list" . stuff) (syntax ("list" x . stuff)))
            (("list*" . stuff) (syntax ("list*" x . stuff)))
            (_ (syntax ("list*" x y))))))
      (define (quasiappend x y)
        (syntax-case y ()
          (("quote" ())
           (cond
             ((null? x) (syntax ("quote" ())))
             ((null? (cdr x)) (car x))
             (else (with-syntax (((p ...) x)) (syntax ("append" p ...))))))
          (_
           (cond
             ((null? x) y)
             (else (with-syntax (((p ...) x) (y y)) (syntax ("append" p ... y))))))))
      (define (quasilist* x y)
        (let f ((x x))
          (if (null? x)
              y
              (quasicons (car x) (f (cdr x))))))
      (define (quasivector x)
        (syntax-case x ()
          (("quote" (x ...)) (syntax ("quote" #(x ...))))
          (_
           (let f ((y x) (k (lambda (ls) (quasisyntax ("vector" (unsyntax-splicing ls))))))
             (syntax-case y ()
               (("quote" (y ...)) (k (syntax (("quote" y) ...))))
               (("list" y ...) (k (syntax (y ...))))
               (("list*" y ... z) (f (syntax z) (lambda (ls) (k (append (syntax (y ...)) ls)))))
               (else (quasisyntax ("list->vector" (unsyntax x)))))))))
      (define (emit x)
        (syntax-case x ()
          (("quote" x) (syntax 'x))
          (("list" x ...) (quasisyntax (list (unsyntax-splicing (map emit (syntax (x ...)))))))
          ;; could emit list* for 3+ arguments if implementation supports list*
          (("list*" x ... y)
           (let f ((x* (syntax (x ...))))
             (if (null? x*)
                 (emit (syntax y))
                 (quasisyntax (cons (unsyntax (emit (car x*))) (unsyntax (f (cdr x*))))))))
          (("append" x ...) (quasisyntax (append (unsyntax-splicing (map emit (syntax (x ...)))))))
          (("vector" x ...) (quasisyntax (vector (unsyntax-splicing (map emit (syntax (x ...)))))))
          (("list->vector" x) (quasisyntax (list->vector (unsyntax (emit (syntax x))))))
          (("value" x) (syntax x))))
      (lambda (x)
        (syntax-case x ()
          ;; convert to intermediate language, combining introduced (but not
          ;; unquoted source) quote expressions where possible and choosing
          ;; optimal construction code otherwise, then emit Scheme code
          ;; corresponding to the intermediate language forms.
          ((_ e) (emit (quasi (syntax e) 0)))))))
  
  (define-syntax unquote
    (lambda (e)
      (syntax-violation 'unquote "Invalid expression" e)))
  
  (define-syntax unquote-splicing
    (lambda (e)
      (syntax-violation 'unquote-splicing "Invalid expression" e)))
  ))

(define-library (core values)
  (export values call-with-values)
  (import (core primitives)
          (core apply)
          (core bool)
          (core let)
          (core list)
          (core intrinsics))
  (begin
    (define *values-tag* (list 'values))
    
    (define (values . ls)
      (if (and (pair? ls) (null? (cdr ls)))
          (car ls)
          (cons *values-tag* ls)))
    
    (define (call-with-values producer consumer)
      (let ((res (producer)))
        (if (and (pair? res) (eq? *values-tag* (car res)))
            (apply consumer (cdr res))
            (consumer res))))))

(define-library (core let-values)
  (export let-values let*-values define-values)
  (import (for (core primitives)   expand run)
          (for (core syntax-rules) expand run)
          (for (core values)       expand run)
          (for (core let)          expand run)
          (for (core intrinsics)   expand run))
  (begin

  (define-syntax let-values
    (syntax-rules ()
      ((let-values (?binding ...) ?body0 ?body1 ...)
       (let-values "bind" (?binding ...) () (begin ?body0 ?body1 ...)))
      
      ((let-values "bind" () ?tmps ?body)
       (let ?tmps ?body))
      
      ((let-values "bind" ((?b0 ?e0) ?binding ...) ?tmps ?body)
       (let-values "mktmp" ?b0 ?e0 () (?binding ...) ?tmps ?body))
      
      ((let-values "mktmp" () ?e0 ?args ?bindings ?tmps ?body)
       (call-with-values 
         (lambda () ?e0)
         (lambda ?args
           (let-values "bind" ?bindings ?tmps ?body))))
      
      ((let-values "mktmp" (?a . ?b) ?e0 (?arg ...) ?bindings (?tmp ...) ?body)
       (let-values "mktmp" ?b ?e0 (?arg ... x) ?bindings (?tmp ... (?a x)) ?body))
      
      ((let-values "mktmp" ?a ?e0 (?arg ...) ?bindings (?tmp ...) ?body)
       (call-with-values
         (lambda () ?e0)
         (lambda (?arg ... . x)
           (let-values "bind" ?bindings (?tmp ... (?a x)) ?body))))))

  (define-syntax let*-values
    (syntax-rules ()
      ((let*-values () ?body0 ?body1 ...)
       (begin ?body0 ?body1 ...))

      ((let*-values (?binding0 ?binding1 ...) ?body0 ?body1 ...)
       (let-values (?binding0)
         (let*-values (?binding1 ...) ?body0 ?body1 ...)))))


  (define-syntax define-values
    (syntax-rules ()
      ((define-values () expr)
       (define dummy
         (call-with-values (lambda () expr)
                           (lambda args #f))))
      ((define-values (var) expr)
       (define var expr))
      ((define-values (var0 var1 ... varn) expr)
       (begin
         (define var0
           (call-with-values (lambda () expr)
                             list))
         (define var1
           (let ((v (cadr var0)))
             (set-cdr! var0 (cddr var0))
             v)) ...
         (define varn
           (let ((v (cadr var0)))
             (set! var0 (car var0))
             v))))
      ((define-values (var0 var1 ... . varn) expr)
       (begin
         (define var0
           (call-with-values (lambda () expr)
                             list))
         (define var1
           (let ((v (cadr var0)))
             (set-cdr! var0 (cddr var0))
             v)) ...
         (define varn
           (let ((v (cdr var0)))
             (set! var0 (car var0))
             v))))
      ((define-values var expr)
       (define var
         (call-with-values (lambda () expr)
                           list)))))
  
  )) ; core let-values

(define-library (core string)
  (import (core primitives))
  (import (core char))
  (import (primitives
          digit-value
          string-ci<=? string-ci<?
          string-ci=? string-ci>=?
          string-ci>? string-downcase
          string-foldcase string-upcase))
  (export 
          char-downcase char-foldcase
          char-lower-case? char-numeric?
          char-upcase char-upper-case?
          char-whitespace? digit-value
          string-ci<=? string-ci<?
          string-ci=? string-ci>=?
          string-ci>? string-downcase
          string-foldcase string-upcase))

(define-library (scheme case-lambda)
  (export case-lambda)
  (import (for (core case-lambda) expand run)))

(define-library (scheme char)
    (import (core string)) 
    (import (core char))
    (export 
        char-alphabetic? char-ci<=? char-ci<?
        char-ci=? char-ci>=? char-ci>?
        char-downcase char-foldcase
        char-lower-case? char-numeric?
        char-upcase char-upper-case?
        char-whitespace? digit-value
        string-ci<=? string-ci<?
        string-ci=? string-ci>=?
        string-ci>? string-downcase
        string-foldcase string-upcase))

(define-library (scheme complex)
    (import (core math))
    (export angle imag-part magnitude make-polar make-rectangular real-part))

(define-library (scheme cxr)
    (import (core primitives))
    (import (core list))
    (export caaar caadr cadar caddr cdaar cdadr cddar cdddr
            caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
            cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr)
    (begin 
      (define (caaar x) (car (car (car x))))
      (define (caadr x) (car (car (cdr x))))
      (define (cadar x) (car (cdr (car x))))
      (define (caddr x) (car (cdr (cdr x))))
      (define (cdaar x) (cdr (car (car x))))
      (define (cdadr x) (cdr (car (cdr x))))
      (define (cddar x) (cdr (cdr (car x))))
      (define (cdddr x) (cdr (cdr (cdr x))))
      (define (caaaar x) (car (car (car (car x)))))
      (define (caaadr x) (car (car (car (cdr x)))))
      (define (caadar x) (car (car (cdr (car x)))))
      (define (caaddr x) (car (car (cdr (cdr x)))))
      (define (cadaar x) (car (cdr (car (car x)))))
      (define (cadadr x) (car (cdr (car (cdr x)))))
      (define (caddar x) (car (cdr (cdr (car x)))))
      (define (cadddr x) (car (cdr (cdr (cdr x)))))
      (define (cdaaar x) (cdr (car (car (car x)))))
      (define (cdaadr x) (cdr (car (car (cdr x)))))
      (define (cdadar x) (cdr (car (cdr (car x)))))
      (define (cdaddr x) (cdr (car (cdr (cdr x)))))
      (define (cddaar x) (cdr (cdr (car (car x)))))
      (define (cddadr x) (cdr (cdr (car (cdr x)))))
      (define (cdddar x) (cdr (cdr (cdr (car x)))))
      (define (cddddr x) (cdr (cdr (cdr (cdr x)))))
))

(define-library (scheme eval)
    (import (core primitives))
    (export environment eval))

(define-library (scheme file)
    (import (primitives call-with-input-file
                        delete-file
                        open-binary-input-file
                        open-input-file
                        with-input-from-file
                        call-with-output-file
                        file-exists?
                        open-binary-output-file
                        open-output-file
                        with-output-to-file))
    (export call-with-input-file
            delete-file
            open-binary-input-file
            open-input-file
            with-input-from-file
            call-with-output-file
            file-exists?
            open-binary-output-file
            open-output-file
            with-output-to-file))

(define-library (scheme inexact)
  (import (core math))
  (export acos asin atan cos exp finite? infinite? log nan? sin sqrt tan))

(define-library (scheme lazy)
    (import (core primitives))
    (import (core let))
    (import (core control))
    (import (core bool))
    (import (core list))
    (import (core intrinsics))
    (import (for (core syntax-rules) expand))
    (export delay
            force
            promise?
            delay-force
            make-promise)
    (begin 
      
      (define *promise-tag* (list 'promise))
      (define (promise done? proc)
        (cons (cons done? proc) *promise-tag*))
      (define (make-promise x)
         (if (promise? x) x (delay x)))

      (define (promise? x)
        (and (pair? x) (eq? *promise-tag* (cdr x))))
      (define (promise-done? x) (car (car x)))
      (define (promise-value x) (cdr (car x)))

      (define (promise-update! new old)
        (set-car! (car old) (promise-done? new))
        (set-cdr! (car old) (promise-value new))
        (set-car! new (car old)))

      (define (force promise)
        (if (promise-done? promise)
            (promise-value promise)
            (let ((promise* ((promise-value promise))))
              (if (not (promise-done? promise))
                (promise-update! promise* promise))
              (force promise))))
      
      (define-syntax delay
        (syntax-rules ()
          ((delay expression)
           (delay-force (make-promise #t expression)))))
      
      (define-syntax delay-force
        (syntax-rules ()
          ((delay-force expression)
           (make-promise #f (lambda () expression)))))
))

(define-library (scheme load)
    (import (core primitives))
    (export load))

(define-library (scheme process-context)
    (import (primitives command-line
                        exit
                        get-environment-variable
                        get-environment-variables
                        emergency-exit))
    (export command-line
            exit
            get-environment-variable
            get-environment-variables
            emergency-exit))

(define-library (scheme read)
    (import (primitives read))
    (export read))

(define-library (scheme repl)
    (import (except (core primitives) eval environment))
    (import (scheme eval))
    (export interaction-environment)
    (begin
      (define (interaction-environment)
        (environment '(scheme base)))))

(define-library (scheme time)
    (import (primitives current-jiffy
                        current-second
                        jiffies-per-second))
    (export current-jiffy
            current-second
            jiffies-per-second))

(define-library (scheme write)
    (import (primitives display
                        write
                        write-shared
                        write-simple))
    (export display
            write
            write-shared
            write-simple))

(define-library (scheme base)
    (import (for (except (core primitives) _ ... environment eval load) run expand)
            (for (core intrinsics)              expand run)
            (for (core let)                     expand run)
            (for (core control)                 expand run)
            (for (core records)                 expand run)
            (for (core derived)                 expand run)
            (for (core quasiquote)              expand run)
            (for (core let-values)              expand run)
            (for (core syntax-rules)            expand run) 
            (for (only (core primitives) _ ... set!) expand)
            (for (core number)                  expand run)
            (for (core bool)                    expand run)
            (for (core list)                    expand run)
            (for (core vector)                  expand run)
            (for (core char)                    expand run)
            (for (core cond-expand)             expand run)
            (for (core values)                  expand run)
            (for (core apply)                   expand run)
            (for (core math)                    expand run)
            (for (core exception)               expand run)
            (for (core syntax-error)            expand run)
            (for (core dynamic)                 expand run)
            (scheme case-lambda)
            (scheme char)
            (scheme complex)
            (scheme cxr)
            (scheme eval)
            (scheme file)
            (scheme inexact)
            (scheme lazy)
            (scheme load)
            (scheme process-context)
            (scheme read)
            (scheme repl)
            (scheme time)
            (scheme write))
    (export 
          * + - ... / < <= = => > >= _ abs and append apply assoc assq
          assv begin binary-port?  boolean=?  boolean?  bytevector
          bytevector-append bytevector-copy bytevector-copy! bytevector-length
          bytevector-u8-ref bytevector-u8-set!  bytevector?  caar cadr
          call-with-current-continuation call-with-port call-with-values call/cc
          car case cdar cddr cdr ceiling char->integer char-ready?  char<=?
          char<?  char=?  char>=?  char>?  char?  close-input-port
          close-output-port close-port complex?  cond cond-expand
          cons current-error-port current-input-port current-output-port
          define define-record-type define-syntax define-values
          denominator do
          dynamic-wind else eof-object?  equal?  error error-object-message
          even?  exact-integer-sqrt exact?  features
          floor floor-remainder
          flush-output-port gcd get-output-string if include-ci inexact?
          input-port?  integer?  lcm let let*-values let-values letrec* list
          list->vector list-ref list-tail make-bytevector make-parameter
          make-vector max memq min negative?  not number->string numerator
          open-input-bytevector open-output-bytevector or output-port?
          parameterize peek-u8 positive?  quasiquote quotient raise-continuable
          rationalize read-bytevector!  read-error?  read-string reverse
          set!  set-cdr!  string string->number string->utf8 string-append
          eof-object eq?  eqv?  error-object-irritants error-object?  exact
          exact-integer?  expt file-error?  floor-quotient floor/ for-each
          get-output-bytevector guard include inexact input-port-open?
          integer->char lambda length let* let-syntax
          letrec letrec-syntax
          list->string list-copy list-set!  list?  make-list make-string map
          member memv modulo newline null?  number?  odd?  open-input-string
          open-output-string output-port-open?  pair?  peek-char port?
          procedure?  quote raise rational?  read-bytevector read-char read-line
          read-u8 real? remainder round set-car!  square string->list string->symbol
          string->vector string-copy string-copy!  string-for-each string-map
          string-set!  string<?  string>=?  string?  symbol->string symbol?
          syntax-rules truncate truncate-remainder u8-ready?  unquote
          utf8->string vector vector->string vector-copy vector-fill!
          vector-length vector-ref vector?  with-exception-handler
          write-char write-u8 string-fill!  string-length string-ref string<=?
          string=?  string>?  substring symbol=?  syntax-error textual-port?
          truncate-quotient truncate/ unless unquote-splicing values
          vector->list vector-append vector-copy!  vector-for-each vector-map
          vector-set!  when write-bytevector write-string zero?))

(define-library (scheme r5rs)
  (import
   (rename (scheme base)
           (exact inexact->exact)
           (inexact exact->inexact))
   (scheme cxr)
   (scheme char)
   (scheme inexact)
   (scheme complex)
   (scheme read)
   (scheme write)
   (scheme file)
   (scheme lazy)
   (scheme eval)
   (scheme repl)
   (scheme load))
  (export
   - ... * / + < <= = => > >= _ abs acos and angle append apply asin assoc assq
   assv atan begin boolean?
   caaaar caaadr caadar caaddr
   cadaar cadadr caddar cadddr
   cdaaar cdaadr cdadar cdaddr
   cddaar cddadr cdddar cddddr
   caaar caadr cadar caddr
   cdaar cdadr cddar cdddr
   caar cadr cdar cddr
   call-with-current-continuation call-with-input-file call-with-output-file
   call-with-values car case cdr ceiling char->integer char-alphabetic?
   char-ci<? char-ci<=? char-ci=? char-ci>? char-ci>=? char-downcase
   char-lower-case? char-numeric? char-ready? char-upcase
   char-upper-case? char-whitespace? char? char<? char<=? char=? char>?
   char>=? close-input-port close-output-port complex? cond cons cos
   current-input-port current-output-port define define-syntax delay
   denominator display do dynamic-wind else eof-object? eq? equal? eqv?
   eval even? exact->inexact exact? exp expt floor for-each force gcd if
   imag-part inexact->exact inexact? input-port? integer->char integer?
   interaction-environment lambda lcm length let let-syntax let* letrec
   letrec-syntax list list->string list->vector list-ref list-tail list?
   load log magnitude make-polar make-rectangular make-string make-vector
   map max member memq memv min modulo negative? newline not
   ; TODO null-environment
   null? number->string number? numerator odd?
   open-input-file open-output-file or output-port? pair? peek-char
   positive? procedure? quasiquote quote quotient rational? rationalize
   read read-char real-part real? remainder reverse round
   ; TODO scheme-report-environment
   set-car! set-cdr! set! sin sqrt string
   string->list string->number string->symbol string-append string-ci<?
   string-ci<=? string-ci=? string-ci>? string-ci>=? string-copy
   string-fill! string-length string-ref string-set! string? string<?
   string<=? string=? string>? string>=? substring symbol->string symbol?
   syntax-rules tan truncate values vector vector->list vector-fill!
   vector-length vector-ref vector-set! vector? with-input-from-file
   with-output-to-file write write-char zero?))
