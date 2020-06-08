;;; loki expander
;;;
;;; This library was been ported from the reference implementation
;;; of a R6RS macro expander and library support in SRFI-72, to R7RS.
;;;
;;; Original License:
;;;=================================================================================
;;;
;;; R6RS Macros and R6RS libraries:
;;;
;;;   Copyright (c) 2006 Andre van Tonder
;;;
;;;   Copyright statement at http://srfi.schemers.org/srfi-process.html
;;;
;;;=================================================================================
;;;
;;; HOOKS:
;;; ------
;;;
;;; For compiler and REPL integration, see the procedures
;;;
;;;   - ex:expand-file       : Use this to expand a file containing libraries and/or
;;;                            toplevel programs before loading into an r5rs-type system
;;;                            or feeding result to an r5rs-type compiler.
;;;                            Suitable for separate compilation.
;;;
;;; COMPILATION:
;;; ------------
;;;    
;;; EX:REPL evaluates a sequence of library definitions, commands, and top-level 
;;; import forms in the interactive environment.  The semantics for 
;;; evaluating libraries in and importing bindings into the interactive 
;;; environment is consistent with the ERR5RS proposal at
;;; http://scheme-punks.cyber-rush.org/wiki/index.php?title=ERR5RS:Libraries.
;;; Bindings in the interactive environment persist between invocations 
;;; of REPL. 
;;;
;;; FORMAT OF EXPANDED CODE:
;;; ------------------------
;;;
;;; We expand internal and library definitions, as well as letrec and letrec*
;;; completely to lambda and set! 
;;; It would be very easy to abstract or change, but we don't bother for now
;;; until implementors other than Larceny show a serious interest.
;;;
;;; SIZE OF OBJECT CODE:
;;; --------------------
;;;
;;; The minimal runtime prerequisites has been separated into a small
;;; include file runtime.scm, which is all that needs to be present for
;;; executing an expanded program that does not contain runtime
;;; uses the exports of (rnrs syntax-case) or (rnrs eval).
;;; See examples.scm for demonstrations of this.
;;;
;;; Expanded libraries may contain identifier environment information
;;; and visit code that could adversely affect the runtime binary size.
;;; This is not a big problem, for several reasons:
;;; First, note that this information is only present in libraries that
;;; define macros.
;;; Second, the size of the environments saved in the object code can
;;; usually be reduced dramatically by using 'only' imports.
;;; Third, the environments, as well as the visit code, can be discarded
;;; completely from the runtime image of a fully expanded program not
;;; using (rnrs syntax-case) or (rnrs eval) at runtime.  It is very
;;; easy to write a little build script that does this.
;;;
;;; The only reason for including this information now in the object code
;;; of a library is to support separate compilation, so one can expand a
;;; library in one session and use macros from the /expanded/ library to
;;; expand another library or program in a new session.  The customization
;;; to get rid of separate compilation, if desired, would be trivial.
(define-library (loki expander)
(import (scheme base))
(import (scheme file))
(import (scheme write))
(import (scheme cxr))
(import (scheme process-context))
(import (loki compat))
(import (loki shared))
(import (loki runtime))
(import (loki util))
(import (loki reader))
(export ex:make-variable-transformer
        ex:identifier?              
        ex:bound-identifier=?       
        ex:free-identifier=?        
        ex:generate-temporaries     
        ex:datum->syntax            
        ex:syntax->datum            
        ex:environment              
        ex:environment-bindings     
        ex:eval                     
        ex:load                     
        ex:syntax-violation         
    
        ex:expand-file              
        ex:expand-sequence          
        ex:expand-datum-sequence          

        ex:invalid-form             
        ex:register-macro!          
        ex:syntax-rename            
        ex:dotted-length            
        ex:dotted-butlast           
        ex:dotted-last              
        ex:free=?)
(begin


;; Direct exports:

(define ex:make-variable-transformer #f)
(define ex:identifier?               #f)
(define ex:bound-identifier=?        #f)
(define ex:free-identifier=?         #f)
(define ex:generate-temporaries      #f)
(define ex:datum->syntax             #f)
(define ex:syntax->datum             #f)
(define ex:environment               #f)
(define ex:environment-bindings      #f)
(define ex:eval                      #f)
(define ex:load                      #f)
(define ex:syntax-violation          #f)

;; System exports:

(define ex:expand-file               #f)
(define ex:expand-sequence           #f)
(define ex:expand-datum-sequence     #f)

;; Indirect exports:

(define ex:invalid-form              #f)
(define ex:register-macro!           #f)
(define ex:syntax-rename             #f)
(define ex:dotted-length             #f)
(define ex:dotted-butlast            #f)
(define ex:dotted-last               #f)
(define ex:free=?                    #f)

;; Single-character symbol prefixes.
(define ex:guid-prefix "&")
(define ex:free-prefix "~")
(define ex:library-prefix "#")

(letrec-syntax
     ;; A trivial but extremely useful s-expression matcher.
     ;; Implements a subset of Wright's matcher's patterns.
     ;; Includes additional (syntax id) pattern that matches
     ;; if input is identifier? and free=? to 'id.

     ((match
      (syntax-rules ()
        ((match (op arg ...) clause ...)
         (let ((x (op arg ...)))
           (match x clause ...)))
        ((match x)
         (ex:invalid-form x))
        ((match x (pat e ...) clause ...)
         (matcher "base" pat "done" x (e ...) (lambda () (match x clause ...))))))

     (matcher
      (syntax-rules (- ___ ? syntax)
        ((matcher "base" () k arg ...)
         (matcher k (lambda (x sk fk) (if (null? x) (sk) (fk))) () arg ...))
        ((matcher "base" - k arg ...)
         (matcher k (lambda (x sk fk) (sk)) () arg ...))
        ((matcher "base" (syntax id) k arg ...)
         (matcher k
                  (lambda (x sk fk)
                    (if (ex:free=? x 'id) (sk) (fk)))
                  ()
                  arg ...))
        ((matcher "base" (? pred? p) k arg ...)
         (matcher "base" p "predicate" pred? k arg ...))
        ((matcher "predicate" code vars pred? k arg ...)
         (matcher k
                  (lambda (x sk fk)
                    (if (pred? x)
                        (code x sk fk)
                        (fk)))
                  vars
                  arg ...))
        ((matcher "base" (p1 ___ tailp ...) k arg ...)
         (matcher "base" p1 "ellipses" (tailp ...) k arg ...))
        ((matcher "ellipses" code vars (tailp ...) k arg ...)
         (matcher k
                  (lambda (x sk fk)
                    (let loop ((x x)
                               (result '()))
                      (define (match-tail)
                        (match x
                          ((tailp ...)
                           (apply sk (if (null? result)
                                         (map (lambda (ignore) '()) 'vars)
                                         (apply map list (reverse result)))))
                          (- (fk))))
                      (cond ((null? x) (match-tail))
                            ((pair? x)
                             (code (car x)
                                   (lambda car-vars
                                     (loop (cdr x) (cons car-vars result)))
                                   match-tail))
                            (else (fk)))))
                  vars
                  arg ...))
        ((matcher "base" (p1 . p2) k arg ...)
         (matcher "base" p1 "pair" p2 k arg ...))
        ((matcher "pair" car-code car-vars p2 k arg ...)
         (matcher "base" p2 "pair-done" car-code car-vars k arg ...))
        ((matcher "pair-done" cdr-code (cdr-var ...) car-code (car-var ...) k arg ...)
         (matcher k
                  (lambda (x sk fk)
                    (if (pair? x)
                        (car-code (car x)
                                  (lambda (car-var ...)
                                    (cdr-code (cdr x)
                                              (lambda (cdr-var ...)
                                                (sk car-var ... cdr-var ...))
                                              fk))
                                  fk)
                        (fk)))
                  (car-var ... cdr-var ...)
                  arg ...))
        ((matcher "base" #(p ___) k arg ...)
         (matcher "base" (p ___) "vector" k arg ...))
        ((matcher "vector" list-code vars k arg ...)
         (matcher k
                  (lambda (x sk fk)
                    (if (vector? x)
                        (list-code (vector->list x)
                                   sk
                                   fk)
                        (fk)))
                  vars
                  arg ...))
        ((matcher "base" id k arg ...)
         (matcher k (lambda (x sk fk) (sk x)) (id) arg ...))
        ((matcher "done" code vars x (e ...) fk)
         (code x (lambda vars e ...) fk)))))

  (let* (;;==========================================================================
         ;;
         ;; Dynamic parameters:
         ;;
         ;;==========================================================================

         ;; toplevel REPL bindings to be initialized later
         (*toplevel-env*     #f)
         ;; current lexical environment to be initialized later
         (*usage-env*        #f)
         ;; current phase
         (*phase*            0)
         ;; current color for painting identifiers upon renaming to be initialized
         (*color*            #f)
         ;; global table mapping <binding name> of keyword to <macro> object
         (*macro-table*      '())
         ;; maps <symbolic key> of reflected environment to actual <environment>
         (*env-table*        '())
         ;; current library name as list of symbols or '() for toplevel
         (*current-library*  '())
         ;; car of this records bindings already referenced in current body
         ;; for detecting when later definitions may violate lexical scope
         (*used*             (list '()))
         ;; history trace for error reporting
         (*trace*            '())
         ;; whether expanded library introduces identifiers via syntax
         ;; expressions - if not, save lots of space by not including
         ;; env-table in object code
         (*syntax-reflected* #f))

    (define (id-library id)
      (or (id-maybe-library id)
          *current-library*))

    (define (bound-identifier=? x y)
      (check x identifier? 'bound-identifier=?)
      (check y identifier? 'bound-identifier=?)
      (and (eq? (id-name x)
                (id-name y))
           (equal? (id-colors x)
                   (id-colors y))))

    ;; As required by r6rs, when this returns, the result is #t
    ;; if and only if the two identifiers resolve to the same binding.
    ;; It also treats unbound identifiers specially.
    ;; As allowed by R6RS, included phase checking of arguments.
    ;; An out of phase error is raised if the comparison succeeds but
    ;; either argument is out of phase.  This is sufficient to ensure
    ;; that literals such as ... in syntax-case are used in the correct phase.
    ;; For more dicussion on this choice, see the readme and the examples file.

    (define (free-identifier=? x y)
      (check x identifier? 'free-identifier=?)
      (check y identifier? 'free-identifier=?)
      (let  ((bx (binding x))
             (by (binding y)))
        (let ((result (if bx
                          (and by
                               (eq? (binding-name bx)
                                    (binding-name by)))
                          (and (not by)
                               (eq? (id-name x)
                                    (id-name y))))))
          (and result
               bx
               (begin (check-binding-level x bx)
                      (check-binding-level y by)))
          ;; A later definition in the same body can only change
          ;; #t to #f, so only record usage in that case.
          (and result
               (register-use! x bx)
               (register-use! y by))
          result)))

    ;; For internal use

    (define (free=? x symbol)
      (and (identifier? x)
           (let  ((bx (binding x)))
             (let ((result
                    (and bx
                         (eq? (binding-name bx) symbol))))
               (and result
                    bx
                    (check-binding-level x bx))
               (and result
                    (register-use! x bx))
               result))))

    ;;==========================================================================
    ;;
    ;; Infrastructure for generated names:
    ;;
    ;;==========================================================================

    ;; Generate-guid returns a fresh symbol that has a globally
    ;; unique external representation and is read-write invariant.
    ;; Your local gensym will probably not satisfy both conditions.
    ;; Prefix makes it disjoint from all builtins.
    ;; Uniqueness is important for incremental and separate expansion.

    (define generate-guid
      (let ((token (ex:unique-token))
            (ticks 0))
        (lambda (symbol)
          (set! ticks (+ ticks 1))
          (string->symbol
           (string-append ex:guid-prefix
                          (symbol->string symbol)
                          "~"
                          token
                          "~"
                          (number->string ticks))))))

    ;; Used to generate user program toplevel names.
    ;; Prefix makes it disjoint from all builtins.
    ;; Prefix makes it disjoint from output of generate-guid.
    ;; Must be read-write invariant.

    (define (make-free-name symbol)
      (string->symbol (string-append ex:free-prefix (symbol->string symbol))))

    ;;=========================================================================
    ;;
    ;; Colors to paint identifiers with:
    ;;
    ;;=========================================================================

    ;; Returns <color> ::= globally unique symbol

    (define (generate-color)
      (generate-guid 'c))

    ;;=========================================================================
    ;;
    ;; Bindings:
    ;;
    ;;=========================================================================

    ;; <binding> ::= (variable         <binding-name> (<level> ...) <mutable?>  <library-name>)
    ;;            |  (macro            <binding-name> (<level> ...) #f          <library-name>)
    ;;            |  (pattern-variable <binding-name> (<level> ...) <dimension> <library-name>)
    ;;            |  #f  (out of context binding from another library)
    ;; <mutable> ::= #t | #f
    ;; <dimension> ::= 0 | 1 | 2 | ...
    ;; <binding-name> ::= <symbol> uniquely identifying binding.
    ;; <binding-name> is used for free-identifier=? comparison.
    ;; For variable and pattern variable bindings, it is the same
    ;; as the symbol emitted for the binding in the object code.
    ;; For macro bindings, it is the key for looking up the transformer
    ;; in the global macro table.

    (define (make-binding type name levels content library)
      (list type name levels content library))

    (define (binding-type b)           (car b))
    (define (binding-name b)           (cadr b))
    (define (binding-levels b)         (caddr b))
    (define (binding-mutable? b)       (cadddr b))
    (define (binding-dimension b)      (cadddr b))
    (define (binding-library b)        (car (cddddr b)))
    (define (binding-mutable-set! b x) (set-car! (cdddr b) x))

    ;; Looks up binding first in usage environment and
    ;; then in attached transformer environments.
    ;; Toplevel forward references are treated specially.
    ;; Returns <binding> | #f if unbound.

    (define (binding id)
      (let ((name (id-name id)))
        (let loop ((env    *usage-env*)
                   (envs   (id-transformer-envs id))
                   (colors (id-colors id)))
          (or (env-lookup (cons name colors) env)
              (and (pair? envs)
                   (loop (env-reify (car envs))
                         (cdr envs)
                         (cdr colors)))))))

    ;;=========================================================================
    ;;
    ;; Mapping in environment: ((<name> <color> ...) . <binding>)
    ;;
    ;;=========================================================================

    ;; Generates a local mapping at the current meta-level
    ;; that can be added to the usage environment.

    (define (make-local-mapping type id content)
      (cons (cons (id-name id)
                  (id-colors id))
            (make-binding type
                          (generate-guid (id-name id))
                          (list (source-level id))
                          content
                          *current-library*)))

    ;; Toplevel binding forms use as binding name the free name
    ;; so that source-level forward references will work in REPL.
    ;; If identifier is macro-generated, bind it with a fresh name.
    ;; This ensures that generated toplevel defines are not visible
    ;; from toplevel source code, thus approximating the behaviour
    ;; of generated internal definitions.

    (define (make-toplevel-mapping type id content)
      (if (null? (id-colors id))
          (cons (cons (id-name id)
                      (id-colors id))
                (make-binding type
                              (make-free-name (id-name id))
                              '(0)
                              content
                              *current-library*))
          (make-local-mapping type id content)))

    ;; Generates a library export binding at the current meta-level.

    ;;=========================================================================
    ;;
    ;; Infrastructure for binding levels:
    ;;
    ;;=========================================================================

    (define (source-level id)
      (- *phase* (id-displacement id)))

    (define (check-binding-level id binding)
      (if binding
          (or (memv (source-level id)
                    (binding-levels binding))
              (syntax-violation
               "invalid reference"
               (string-append "Attempt to use binding of " (symbol->string (id-name id))
                              " in library (" (list->string (id-library id) " ")
                              ") at invalid level " (number->string (source-level id))
                              ".  Binding is only available at levels: "
                              (list->string (binding-levels binding) " "))
               id))
          (or (and (null? (id-library id))
                   (= *phase* 0))
              (syntax-violation
               "invalid reference"
               (string-append "No binding available for " (symbol->string (id-name id))
                              " in library (" (list->string (id-library id) " ") ")")

               id))))

    ;;=========================================================================
    ;;
    ;; Environments:
    ;;
    ;;=========================================================================

    ;; An environment is a list of frames.
    ;;
    ;;   <environment> ::= (<frame> ...)
    ;;   <frame>       ::= (list ((<key> . <value>) ...))
    ;;
    ;; Keys must be comparable with equal? and unique in each frame.
    ;; Frames can be added, or the leftmost frame can be destructively
    ;; updated in the case of binding constructs such as bodies where
    ;; definitions are incrementally discovered.

    (define (make-null-env) '())
    (define (make-unit-env) (env-extend '() (make-null-env)))

    ;; Adds a new frame containing mappings to env.

    (define (env-extend mappings env)
      (cons (list mappings) env))

    ;; Destructively extends the leftmost frame in env.

    (define (env-extend! mappings env)
      (let ((frame (car env)))
        (set-car! frame (append mappings (car frame)))))

    ;; Returns <object> | #f

    (define (env-lookup key env)
      (and (pair? env)
           (or (let ((probe (assoc key (caar env))))
                 (and probe
                      (or (cdr probe)
                          (syntax-violation
                           #f "Out of context reference to identifier" (car key)))))
               (env-lookup key (cdr env)))))

    ;; Is id already bound in leftmost frame?

    (define (duplicate? id env)
      (assoc (cons (id-name id)
                   (id-colors id))
             (caar env)))

    ;; Returns a single-symbol <key> representing an
    ;; environment that can be included in object code.

    (define (env-reflect env)
      (cond ((and (not (null? *env-table*))      ; +++
                  (eq? env (cdar *env-table*)))  ; +++
             (caar *env-table*))                 ; +++
            (else
             (let ((key (generate-guid 'env)))
               (set! *env-table*
                     (cons (cons key env)
                           *env-table*))
               key))))

    ;; The inverse of the above.

    (define (env-reify key-or-env)
      (if (symbol? key-or-env)
          (cdr (assq key-or-env *env-table*))
          key-or-env))

    (define (compress env-table)
      (let ((frame-table '())
            (count 0))
        (for-each (lambda (entry)
                    (for-each (lambda (frame)
                                (if (not (assq frame frame-table))
                                    (begin
                                      (set! frame-table (cons (cons frame count) frame-table))
                                      (set! count (+ 1 count)))))
                              (cdr entry)))
                  env-table)
        (cons (map (lambda (env-entry)
                     (cons (car env-entry)
                           (map (lambda (frame)
                                  (cdr (assq frame frame-table)))
                                (cdr env-entry))))
                   env-table)
              (map (lambda (frame-entry)
                     (cons (cdr frame-entry)
                           (list (map (lambda (mapping)
                                        (cons (car mapping)
                                              (let ((binding (cdr mapping)))
                                                (case (binding-type binding)
                                                  ;; Pattern variable bindings can never be
                                                  ;; used in client, so don't waste space.
                                                  ;; Should really do the same with all local
                                                  ;; bindings, but there are usually much less
                                                  ;; of them, so don't bother for now.
                                                  ((pattern-variable) #f) ; +++
                                                  (else binding)))))
                                      (caar frame-entry)))))
                   frame-table))))

    (define (uncompress compressed-env-table)
      (if (null? compressed-env-table) '()
        (map (lambda (env-entry)
               (cons (car env-entry)
                     (map (lambda (frame-abbrev)
                            (cdr (assv frame-abbrev (cdr compressed-env-table))))
                          (cdr env-entry))))
             (car compressed-env-table))))

    ;;=========================================================================
    ;;
    ;; Syntax-reflect and syntax-rename:
    ;;
    ;; This is the basic building block of the implicit renaming mechanism for
    ;; maintaining hygiene.  Syntax-reflect generates the expanded code for
    ;; (syntax id), including the expand-time environment in the
    ;; external representation.  It expands to syntax-rename, which performs
    ;; the implicit renaming when this expanded code is eventually run.
    ;; The displacement computations calculate the difference between the
    ;; usage phase and the transformer phase.
    ;;
    ;;=========================================================================

    (define (syntax-reflect id)
      (let ((source (id-source id)))
        (set! *syntax-reflected* #t)
        `(ex:syntax-rename ',(id-name id)
                           ',(id-colors id)
                           ',(cons (env-reflect *usage-env*)
                                   (id-transformer-envs id))
                           ,(- (- *phase* (id-displacement id)) 1)
                           ',(id-library id)
                           ,(source-filename source)
                           ,(source-line source)
                           ,(source-column source))))

    (define (syntax-rename name colors transformer-envs transformer-phase source-library filename line column)
      (make-identifier name
                       (cons *color* colors)
                       transformer-envs
                       (- *phase* transformer-phase)
                       source-library
                       (make-source filename line column)))

    ;;=====================================================================
    ;;
    ;; Capture and sexp <-> syntax conversions:
    ;;
    ;;=====================================================================

    (define (add-context-to-identifiers tid datum)
      (check tid identifier? 'add-context-to-identifiers)
      (sexp-map (lambda (leaf)
                  (cond ((annotation-type? 'identifier leaf)
                         (make-identifier (annotation-expression leaf)
                                          (id-colors tid)
                                          (id-transformer-envs tid)
                                          (id-displacement tid)
                                          (id-maybe-library tid)
                                          (annotation-source leaf)))
                        (else leaf)))
                datum))

    (define (datum->syntax tid datum)
      (check tid identifier? 'datum->syntax)
      (sexp-map (lambda (leaf)
                  (cond ((symbol? leaf)
                         (make-identifier leaf
                                          (id-colors tid)
                                          (id-transformer-envs tid)
                                          (id-displacement tid)
                                          (id-maybe-library tid)
                                          (id-source tid)))
                        (else leaf)))
                datum))

    (define (syntax->datum exp)
      (sexp-map (lambda (leaf)
                  (cond ((identifier? leaf) (id-name leaf))
                        ((annotation? leaf) (annotation-expression leaf))
                        ((symbol? leaf)
                         (assertion-violation 'syntax->datum "A symbol is not a valid syntax object" leaf))
                        (else leaf)))
                exp))

    ;; Fresh identifiers:

    (define (generate-temporaries ls)
      (check ls list? 'generate-temporaries)
      (map (lambda (ignore)
             (make-identifier 'temp
                              (list (generate-color))
                              (list (make-null-env))
                              *phase*
                              #f
                              (make-source "<temp>" 1 0)))
           ls))

    ;; For use internally as in the explicit renaming system.

    (define (rename type symbol)
      (make-identifier symbol
                       (list *color*)
                       (list (env-extend
                              (list (cons (cons symbol '())
                                          (make-binding type symbol '(0) #f '())))
                              (make-null-env)))
                       *phase*
                       #f
                       (make-source "<unknown>" 1 0)))

    ;;=========================================================================
    ;;
    ;; Macro objects:
    ;;
    ;;=========================================================================

    ;; Expanders are system macros that fully expand
    ;; their arguments to core Scheme, while
    ;; transformers and variable transformers are
    ;; user macros.

    ;; <type> ::= expander | transformer | variable-transformer

    (define (make-macro type proc) (cons type proc))
    (define macro-type car)
    (define macro-proc cdr)
    (define (macro? m)
      (and (member (macro-type m) '(expander transformer variable-transformer))
           (procedure? (macro-proc m))))

    (define (make-expander proc)             (make-macro 'expander proc))
    (define (make-transformer proc)          (make-macro 'transformer proc))
    (define (make-variable-transformer proc) (make-macro 'variable-transformer proc))
    

    (define (make-user-macro procedure-or-macro)
      (cond
        ((macro? procedure-or-macro)
          procedure-or-macro)
        ((procedure? procedure-or-macro)
          (make-transformer procedure-or-macro))
        (else
          (assertion-violation #f "Invalid user macro" procedure-or-macro))))

    ;; Returns <macro>.

    (define (binding->macro binding t)
      (cond ((assq (binding-name binding) *macro-table*) => cdr)
            (else
             (syntax-violation
              #f "Reference to macro keyword out of context" t))))

    ;; Registering macro.

    (define (register-macro! binding-name procedure-or-macro)
      (set! *macro-table* (cons (cons binding-name (make-user-macro procedure-or-macro))
                                *macro-table*)))

    ;; Register macros in library

    (define (visit-library! library)
      (for-all (lambda (def)
                 (let ((name (car def)) (macro (cdr def)))
                    (if (macro? macro)
                      (register-macro! name macro)
                      (register-macro! name (make-transformer (ex:runtime-eval macro))))))
               (ex:library-syntax-defs library)))

    ;; Calls a macro with a new color.

    (define (invoke-macro macro t)
      (set! *color* (generate-color))
      ((macro-proc macro) t))

    ;;=========================================================================
    ;;
    ;; Expander dispatch:
    ;;
    ;;=========================================================================

    (define (expand t)
      (fluid-let ((*trace* (cons t *trace*)))
        (let ((binding (operator-binding t)))
          (cond (binding (case (binding-type binding)
                           ((macro)
                            (let ((macro (binding->macro binding t)))
                              (let ((expanded-once (invoke-macro macro t)))
                                (case (macro-type macro)
                                  ((expander) expanded-once)
                                  (else
                                   (expand expanded-once))))))
                           ((variable)
                            (if (list? t)
                                (cons (binding-name binding)
                                      (map expand (cdr t)))
                                (binding-name binding)))
                           ((pattern-variable)
                           (begin
                            (syntax-violation #f "Pattern variable used outside syntax template" t)))))
                ((list? t)       (map expand t))
                ((identifier? t) (make-free-name (id-name t)))
                ((annotation? t) (annotation-expression t))
                ((pair? t)       (syntax-violation #f "Invalid procedure call syntax" t))
                ((symbol? t)     (syntax-violation #f "Symbol may not appear in syntax object" t))
                (else t)))))

    ;; Only expands while t is a user macro invocation.
    ;; Used by expand-lambda to detect internal definitions.

    (define (head-expand t)
      (fluid-let ((*trace* (cons t *trace*)))
        (let ((binding (operator-binding t)))
          (cond (binding (case (binding-type binding)
                           ((macro)
                            (let ((macro (binding->macro binding t)))
                              (case (macro-type macro)
                                ((expander) (values t binding))
                                (else
                                 (head-expand (invoke-macro macro t))))))
                           (else (values t binding))))
                (else (values t binding))))))

    ;; Returns binding of identifier in operator position | #f if none.
    ;; Singleton identifiers are also considered operators here for
    ;; the purpose of discovering identifier macros and variables.
    ;; Checks level and registers as a use.

    (define (operator-binding t)
      (let ((operator (if (pair? t) (car t) t)))
        (and (identifier? operator)
             (let ((binding (binding operator)))
               (check-binding-level operator binding)
               (register-use! operator binding)
               binding))))

    ;;=========================================================================
    ;;
    ;; Quote, if, set!, expression begin, expression let[rec]-syntax, and, or:
    ;;
    ;;=========================================================================

    (define (expand-quote exp)
      (match exp
        ((- datum) (syntax->datum exp))))

    (define (expand-if exp)
      (match exp
        ((- e1 e2 e3) `(if ,(expand e1) ,(expand e2) ,(expand e3)))
        ((- e1 e2)    `(if ,(expand e1) ,(expand e2)))))

    (define (expand-set! exp)
      (match exp
        ((- (? identifier? id) e)
         (let ((binding (binding id)))
           (check-binding-level id binding)
           (register-use! id binding)
           (case (binding-type binding)
             ((macro)
              (let ((macro (binding->macro binding id)))
                (case (macro-type macro)
                  ((variable-transformer)
                   (expand (invoke-macro macro exp)))
                  (else
                   (syntax-violation
                    'set! "Keyword being set! is not a variable transformer" exp id)))))
             ((variable)
              (or (eq? (binding-library binding) *current-library*)
                  (syntax-violation
                   'set! "Directly or indirectly imported variable cannot be assigned" exp id))
              (binding-mutable-set! binding #t)
              `(set! ,(binding-name binding)
                     ,(expand e)))
             ((pattern-variable)
              (syntax-violation 'set! "Pattern variable used outside syntax template" exp id)))))))

    ;; Expression begin.

    (define (expand-begin exp)
      (match exp
        ((- exps ___)
         (scan-sequence 'expression-sequence
                        #f
                        exps
                        (lambda (forms no-syntax-definitions no-bound-variables)
                          `(begin ,@(map cdr forms)))))))

    ;; Expression let(rec)-syntax:

    (define (expand-local-syntax exp)
      (expand-begin `(,(rename 'macro 'begin) ,exp)))

    ;; Define and and or as primitives  so we can import them into the
    ;; toplevel without spoiling the and and or of the library language.

    (define (expand-and exp)
      (match exp
        ((and) #t)
        ((and e) (expand e))
        ((and e es ___)
         `(if ,(expand e)
              ,(expand `(,and ,@es))
              #f))))

    (define (expand-or exp)
      (match exp
        ((or) #t)
        ((or e) (expand e))
        ((or e es ___)
         `(let ((x ,(expand e)))
            (if x x ,(expand `(,or ,@es)))))))

    ;;=========================================================================
    ;;
    ;; Lambda:
    ;;
    ;;=========================================================================

    (define (expand-lambda exp)
      (match exp
        ((- (? formals? formals) body ___)
         (fluid-let ((*usage-env*
                      (env-extend (map (lambda (formal)
                                         (make-local-mapping 'variable formal #f))
                                       (flatten formals))
                                  *usage-env*)))
           (let ((formals (dotted-map (lambda (formal) (binding-name (binding formal))) formals)))
             ;; Scan-sequence expects the caller to have prepared
             ;; the frame to which to destructively add bindings.
             ;; Lambda bodies need a fresh frame.
             (fluid-let ((*usage-env* (env-extend '() *usage-env*)))
               (scan-sequence 'lambda
                              make-local-mapping
                              body
                              (lambda (forms syntax-definitions bound-variables)
                                `(lambda ,formals
                                   ,@(if (null? bound-variables)                ; +++
                                         (emit-body forms 'set!)    ; +++
                                         `(((lambda ,bound-variables
                                              ,@(emit-body forms 'set!))
                                            ,@(map (lambda (ignore) 'void)
                                                   bound-variables)))))))))))))

    (define (formals? s)
      (or (null? s)
          (identifier? s)
          (and (pair? s)
               (identifier? (car s))
               (formals? (cdr s))
               (not (dotted-memp (lambda (x)
                                   (bound-identifier=? x (car s)))
                                 (cdr s))))))

    ;;=========================================================================
    ;;
    ;; Bodies and sequences:
    ;;
    ;;=========================================================================

    ;; R6RS splicing of internal let-syntax and letrec-syntax
    ;; requires that we remember the bindings visible in each
    ;; form for later expansion of deferred right hand sides
    ;; and expressions.  This is done by attaching
    ;; the environment to the expression.
    ;; We call the resulting data structure a wrap.
    ;; Wraps are only used internally in processing of bodies,
    ;; and are never seen by user macros.

    (define (make-wrap env exp)
      (cons env exp))
    (define wrap-env car)
    (define wrap-exp cdr)

    ;; The continuation k is evaluated in the body environment.  This is
    ;; used for example by expand-library to obtain the correct bindings of
    ;; exported identifiers.
    ;;
    ;; <body-type> ::= toplevel | library | program | lambda | expression-sequence
    ;;
    ;; All but TOPLEVEL are as in r6rs.
    ;; TOPLEVEL is meant for the REPL.
    ;; At TOPLEVEL, we may have a sequence of expressions, definitions, macros,
    ;; import declarations, libraries and programs wrapped in (program ---).
    ;; Redefinitions are allowed at toplevel.

    (define (scan-sequence body-type make-map body-forms k)

      ;; Each <form> ::= (<symbol | #f> #t <wrap>)   (deferred rhs)
      ;;              |  (<symbol | #f> #f <s-expr>) (undeferred rhs)
      ;; Returns ((<symbol | #f> . <s-expr>) ...)

      (define (expand-deferred forms)
        (map (lambda (form)
               (cons (car form)
                     (let ((deferred? (cadr form))
                           (exp       (caddr form)))
                       (if deferred?
                           (fluid-let ((*usage-env* (wrap-env exp)))
                             (expand (wrap-exp exp)))
                           exp))))
             forms))

      (let ((common-env *usage-env*))

        ;; Add new frame for keeping track of bindings used
        ;; so we can detect redefinitions violating lexical scope.
        (add-fresh-used-frame!)

        (let loop ((ws (map (lambda (e) (make-wrap common-env e))
                            body-forms))
                   (forms           '())
                   (syntax-defs     '())
                   (bound-variables '()))
          (cond
           ((null? ws)
            (check-expression-body body-type forms body-forms)
            ;; Add denotations used in this frame to those of parent.
            ;; This is just for the optional reporting of shadowing errors.
            (merge-used-with-parent-frame!)
            (k (reverse (expand-deferred forms))
               (reverse syntax-defs)
               bound-variables))
           (else
            (fluid-let ((*usage-env* (wrap-env (car ws))))
              (call-with-values
                  (lambda () (head-expand (wrap-exp (car ws))))
                (lambda (form operator-binding)
                  (let ((type (and operator-binding (binding-name operator-binding))))
                    (check-expression-sequence body-type type form)
                    (check-toplevel            body-type type form)
                    (case type
                      ((program)
                       (loop (cdr ws)
                             (cons (list #f #f (expand-program form)) forms)
                             syntax-defs
                             bound-variables))
                      ((define-library)
                       (loop (cdr ws)
                             (cons (list #f #f (expand-library form)) forms)
                             syntax-defs
                             bound-variables))
                      ((define)
                       (call-with-values
                           (lambda () (parse-definition form #f))
                         (lambda (id rhs)
                           (check-valid-definition id common-env body-type form forms type)
                           (env-extend! (list (make-map 'variable id #f)) common-env)
                           (loop (cdr ws)
                                 (cons (list (binding-name (binding id))
                                             #t
                                             (make-wrap *usage-env* rhs))
                                       forms)
                                 syntax-defs
                                 (cons (binding-name (binding id)) bound-variables)))))
                      ((define-syntax)
                       (call-with-values
                           (lambda () (parse-definition form #t))
                         (lambda (id rhs)
                           (check-valid-definition id common-env body-type form forms type)
                           (let ((mapping (make-map 'macro id #f)))
                             (env-extend! (list mapping) common-env)
                             (let ((rhs (fluid-let ((*phase* (+ 1 *phase*)))
                                          (expand rhs))))
                               (register-macro! (binding-name (cdr mapping)) (make-transformer (ex:runtime-eval rhs)))
                               (loop (cdr ws)
                                     forms
                                     (cons (cons (binding-name (binding id)) rhs) syntax-defs)
                                     bound-variables))))))
                      ((begin)
                       (or (list? form)
                           (invalid-form form))
                       (loop (append (map (lambda (exp)
                                            (make-wrap *usage-env* exp))
                                          (cdr form))
                                     (cdr ws))
                             forms
                             syntax-defs
                             bound-variables))
                      ((let-syntax letrec-syntax)
                       (call-with-values
                           (lambda () (parse-local-syntax form))
                         (lambda (formals rhs body)
                           (let* ((original-env *usage-env*)
                                  (usage-diff   (map (lambda (formal)
                                                       (make-local-mapping 'macro formal #f))
                                                     formals))
                                  (extended-env (env-extend usage-diff original-env))
                                  (rhs-expanded
                                   (fluid-let ((*phase* (+ 1 *phase*))
                                               (*usage-env*
                                                (case type
                                                  ((let-syntax)    original-env)
                                                  ((letrec-syntax) extended-env))))
                                     (map expand rhs)))
                                  (macros (map (lambda (e) (ex:runtime-eval e)) rhs-expanded)))
                             (for-each (lambda (mapping macro)
                                         (register-macro! (binding-name (cdr mapping)) (make-transformer macro)))
                                       usage-diff
                                       macros)
                             (loop (append (map (lambda (form) (make-wrap extended-env form))
                                                body)
                                           (cdr ws))
                                   forms
                                   syntax-defs
                                   bound-variables)))))
                      (else
                       (loop (cdr ws)
                             (cons (list #f #t (make-wrap *usage-env* form))
                                   forms)
                             syntax-defs
                             bound-variables))))))))))))

    (define (emit-body body-forms define-or-set)
      (map (lambda (body-form)
             (if (symbol? (car body-form))
                 `(,define-or-set ,(car body-form) ,(cdr body-form))
                 (cdr body-form)))
           body-forms))

    (define (parse-definition exp syntax-def?)
      (match exp
        ((- (? identifier? id))
         (values id (rename 'variable 'void)))
        ((- (? identifier? id) e)
         (values id e))
        ((- ((? identifier? id) . (? formals? formals)) body ___)
         (and syntax-def?
              (invalid-form exp))
         (values id `(,(rename 'macro 'lambda) ,formals ,@body)))))

    (define (parse-local-syntax t)
      (match t
        ((- ((x e) ___) body ___)
         (or (formals? x)
             (invalid-form t))
         (values x e body))))

    (define (check-expression-sequence body-type type form)
      (and (eq? body-type 'expression-sequence)
           (memq type '(import program library define define-syntax))
           (syntax-violation type "Invalid form in expression sequence" form)))

    (define (check-toplevel body-type type form)
      (and (not (eq? body-type 'toplevel))
           (memq type '(import program library))
           (syntax-violation type "Expression may only occur at toplevel" form)))

    (define (check-valid-definition id common-env body-type form forms type)
      (and (not (eq? body-type 'toplevel))
           (duplicate? id common-env)
           (syntax-violation type "Redefinition of identifier in body" form id))
      (check-used id body-type form)
      (and (not (memq body-type `(toplevel program library)))
           (not (null? forms))
           (not (symbol? (car (car forms))))
           (syntax-violation type "Definitions may not follow expressions in a body" form)))

    (define (check-expression-body body-type forms body-forms)
      (and (eq? body-type 'lambda)
           (or (null? forms)
               (symbol? (caar forms)))
           (syntax-violation body-type "Body must be nonempty and end with an expression" body-forms)))

    ;;=========================================================================
    ;;
    ;; Syntax-case:
    ;;
    ;;=========================================================================

    (define (expand-syntax-case exp)
      (define (literal? x)
        (and (identifier? x)
             (not (or (free=? x '_)
                      (free=? x '...)))))
      (match exp
        ((- e ((? literal? literals) ___) clauses ___)
         (let ((input (generate-guid 'input)))
           `(let ((,input ,(expand e)))
              ,(process-clauses clauses input literals))))))

    (define (process-clauses clauses input literals)
      (define (literal? pattern)
        (and (identifier? pattern)
             (memp (lambda (x)
                     (bound-identifier=? x pattern))
                   literals)))

      (define (process-match input pattern sk fk)
        (if (not (symbol? input))
            (let ((temp (generate-guid 'temp)))
              `(let ((,temp ,input))
                 ,(process-match temp pattern sk fk)))
            (match pattern
              ((syntax _)         sk)
              ((syntax ...)       (syntax-violation 'syntax-case "Invalid use of ellipses" pattern))
              (()                 `(if (null? ,input) ,sk ,fk))
              ((? literal? id)    `(if (and (ex:identifier? ,input)
                                            (ex:free-identifier=? ,input ,(syntax-reflect id)))
                                       ,sk
                                       ,fk))
              ((? identifier? id) `(let ((,(binding-name (binding id)) ,input)) ,sk))
              ((p (syntax ...))
               (let ((mapped-pvars (map (lambda (pvar) (binding-name (binding pvar)))
                                        (map car (pattern-vars p 0)))))
                 (if (and (identifier? p)                                   ; +++
                          (= (length mapped-pvars) 1))                      ; +++
                     `(if (list? ,input)                                    ; +++
                          (let ((,(car mapped-pvars) ,input))               ; +++
                            ,sk)                                            ; +++
                          ,fk)                                              ; +++
                     (let ((columns (generate-guid 'cols))
                           (rest    (generate-guid 'rest)))
                       `(ex:map-while (lambda (,input)
                                        ,(process-match input
                                                        p
                                                        `(list ,@mapped-pvars)
                                                        #f))
                                      ,input
                                      (lambda (,columns ,rest)
                                        (if (null? ,rest)
                                            (apply (lambda ,mapped-pvars ,sk)
                                                   (if (null? ,columns)
                                                       ',(map (lambda (ignore) '()) mapped-pvars)
                                                       (apply map list ,columns)))
                                            ,fk)))))))
              ((p (syntax ...) . tail)
               (let ((tail-length (dotted-length tail)))
                 `(if (>= (ex:dotted-length ,input) ,tail-length)
                      ,(process-match `(ex:dotted-butlast ,input ,tail-length)
                                      `(,p ,(cadr pattern))
                                      (process-match `(ex:dotted-last ,input ,tail-length)
                                                     tail
                                                     sk
                                                     fk)
                                      fk)
                      ,fk)))
              ((p1 . p2)
               `(if (pair? ,input)
                    ,(process-match `(car ,input)
                                    p1
                                    (process-match `(cdr ,input) p2 sk fk)
                                    fk)
                    ,fk))
              (#(ps ___)
               `(if (vector? ,input)
                    ,(process-match `(vector->list ,input)
                                    ps
                                    sk
                                    fk)
                    ,fk))
              ((? symbol? -)
               (syntax-violation 'syntax-case "Symbol object may not appear in pattern" pattern))
              (other
                #t
               `(if (equal? ,input ',(syntax->datum other)) ,sk ,fk)))))

      (define (pattern-vars pattern level)
        (match pattern
          ((p (syntax ...) . tail) (append (pattern-vars p (+ level 1))
                                           (pattern-vars tail level)))
          ((p1 . p2)               (append (pattern-vars p1 level)
                                           (pattern-vars p2 level)))
          (#(ps ___)               (pattern-vars ps level))
          ((syntax ...)            '())
          ((syntax _)              '())
          ((? literal? -)          '())
          ((? identifier? id)      (list (cons id level)))
          (-                       '())))

      (define (process-clause clause input fk)
        (match clause
          ((pattern . rest)
           (let ((pvars    (pattern-vars pattern 0)))
             (check-set? (map car pvars)
                         bound-identifier=?
                         (lambda (dup)
                           (syntax-violation 'syntax-case "Repeated pattern variable" clause dup)))
             (let ((mappings (map (lambda (pvar)
                                    (make-local-mapping 'pattern-variable (car pvar) (cdr pvar)))
                                  pvars)))
               (fluid-let ((*usage-env* (env-extend mappings *usage-env*)))
                
                 (process-match input
                                pattern
                                (match rest
                                  ((template)
                                   (expand template))
                                  ((fender template)
                                   `(if ,(expand fender)
                                        ,(expand template)
                                        ,fk))
                                  (- (syntax-violation 'syntax-case "Invalid clause" clause)))
                                fk)))))))

      ;; process-clauses

      (match clauses
        (()
         `(ex:invalid-form ,input))
        ((clause clauses ___)
         (let ((fail  (generate-guid 'fail)))
           `(let ((,fail (lambda () ,(process-clauses clauses input literals))))
              ,(process-clause clause input `(,fail)))))))
              

    ;;=========================================================================
    ;;
    ;; Syntax:
    ;;
    ;;=========================================================================

    (define (expand-syntax form)
      (match form
        ((- template)
         (let ((t (process-template template 0 #f)))
           t))))

    (define (process-template template dim ellipses-quoted?)
      (match template
        ((syntax ...)
         (if (not ellipses-quoted?)
             (syntax-violation 'syntax "Invalid occurrence of ellipses in syntax template" template))
         (syntax-reflect template))
        ((? identifier? id)
         (let ((binding (binding id)))
           (cond ((and binding
                       (eq? (binding-type binding) 'pattern-variable)
                       (binding-dimension binding))
                  => (lambda (pdim)
                       (if (<= pdim dim)
                           (begin
                             (check-binding-level id binding)
                             (register-use! id binding)
                             (binding-name binding))
                           (syntax-violation 'syntax "Template dimension error (too few ...'s?)" id))))
                 (else
                   (syntax-reflect id)))))
        (((syntax ...) p)
         (process-template p dim #t))
        ((? (lambda (_) (not ellipses-quoted?))
            (t (syntax ...) . tail))
         (let* ((head (segment-head template)) 
                (vars
                 (map (lambda (mapping)
                        (let ((id      (car mapping))
                              (binding (cdr mapping)))
                          (check-binding-level id binding)
                          (register-use! id binding)
                          (binding-name binding)))
                      (free-meta-variables head (+ dim 1) '() 0))))
           (if (null? vars)
               (syntax-violation 'syntax "Too many ...'s" template)
               (let* ((x (process-template head (+ dim 1) ellipses-quoted?))
                      (gen (if (equal? (list x) vars)   ; +++
                               x                        ; +++
                               (if (= (length vars) 1) 
                                   `(map (lambda ,vars ,x)
                                         ,@vars)
                                   `(if (= ,@(map (lambda (var) 
                                                    `(length ,var))
                                                  vars))
                                        (map (lambda ,vars ,x)
                                             ,@vars)
                                        (ex:syntax-violation 
                                         'syntax 
                                         "Pattern variables denoting lists of unequal length preceding ellipses"
                                         ',(syntax->datum template) 
                                         (list ,@vars))))))
                      (gen (if (> (segment-depth template) 1)
                               `(apply append ,gen)
                               gen)))
                 (if (null? (segment-tail template))   ; +++
                     gen                               ; +++
                     `(append ,gen ,(process-template (segment-tail template) dim ellipses-quoted?)))))))
        ((t1 . t2)
         `(cons ,(process-template t1 dim ellipses-quoted?)
                ,(process-template t2 dim ellipses-quoted?)))
        (#(ts ___)
         `(list->vector ,(process-template ts dim ellipses-quoted?)))
        (other
         `(quote ,(expand other)))))
    
    (define (free-meta-variables template dim free deeper)
      (match template
        ((? identifier? id)
         (if (memp (lambda (x) (bound-identifier=? (car x) id)) free)
             free
             (let ((binding (binding id)))
               (if (and binding
                        (eq? (binding-type binding) 'pattern-variable)
                        (let ((pdim (binding-dimension binding)))
                          (and (> pdim 0) 
                               (not (>= deeper pdim))
                               (<= (- pdim deeper) 
                                   dim))))
                   (cons (cons id binding) free)
                   free))))
        ((t (syntax ...) . rest)
         (free-meta-variables t 
                              dim 
                              (free-meta-variables (segment-tail template) dim free deeper)
                              (+ deeper (segment-depth template))))  
        ((t1 . t2)
         (free-meta-variables t1 dim (free-meta-variables t2 dim free deeper) deeper))
        (#(ts ___) 
         (free-meta-variables ts dim free deeper))
        (- free)))
 
    ;; Count the number of `...'s in PATTERN.

    (define (segment-depth pattern)
      (match pattern
        ((p (syntax ...) . rest)
         (+ 1 (segment-depth (cdr pattern))))
        (- 0)))
      
    ;; All but the last ellipses
    
    (define (segment-head pattern)
      (let ((head
             (let recur ((pattern pattern))
               (match pattern
                 ((h (syntax ...) (syntax ...) . rest)
                  (cons h (recur (cdr pattern))))
                 ((h (syntax ...) . rest)
                  (list h))))))
        (match head 
          ((h (syntax ...) . rest)
           head)
          (- (car head)))))   

    ;; Get whatever is after the `...'s in PATTERN.

    (define (segment-tail pattern)
      (let loop ((pattern (cdr pattern)))
        (match pattern
          (((syntax ...) . tail)
           (loop tail))
          (- pattern))))

    ;;=========================================================================
    ;;
    ;; Detecting violations of lexical scope.
    ;;
    ;;=========================================================================

    ;; This is r6rs-optional.
    ;; For avoiding giving lexically invalid semantics to body
    ;; sequences according to the following semantics described in r6rs:
    ;; A definition in the sequence of forms must not define any
    ;; identifier whose binding is used to determine the meaning of the
    ;; undeferred portions of the definition or any definition that precedes
    ;; it in the sequence of forms.
    ;; This implementation treats a possble violation of the restriction
    ;; as a syntax violation.

    ;; The parameter *used* keeps track of bindings used so we can
    ;; detect redefinitions violating lexical scope in body sequences.
    ;; The car of *used* contains bindings used in current frame.

    (define (add-fresh-used-frame!)
      (set! *used* (cons '() *used*)))

    (define (register-use! id binding)
      (set! *used* (cons (cons (cons id binding)
                               (car *used*))
                         (cdr *used*))))

    (define (merge-used-with-parent-frame!)
      (set! *used* (cons (append (car  *used*)
                                 (cadr *used*))
                         (cddr *used*))))

    (define (check-used id body-type form)
      (and (not (eq? body-type 'toplevel))
           ;; The car contains bindings for current frame and nested frames
           (let* ((already-used (car *used*))
                  ;; This destructively changes *used* and must follow previous
                  (binding (binding id)))
             (if (memp (lambda (mapping)
                         (and (eq? binding (cdr mapping))
                              (bound-identifier=? id (car mapping))))
                       already-used)
                 (syntax-violation
                  'definition
                  "Definition of identifier that may have already affected meaning of undeferred portions of body"
                  form
                  id)))))

    ;;==========================================================================
    ;;
    ;; Libraries:
    ;;
    ;;==========================================================================

    (define (expand-program t)
      (match t
        ((program name exps ___) (expand-library-or-program t 'program))))

    (define (expand-library t)
      (expand-library-or-program t 'library))

    ;; <library-type> ::= library | program

    (define (expand-library-or-program t library-type)
      (match t
        ((keyword name declarations ___)
         (let ((name (syntax->datum (scan-library-name name))))
           (call-with-values
               (lambda () (scan-declarations declarations))
             (lambda (imported-libraries imports exports body-forms)
               (fluid-let ((*usage-env*        (make-unit-env))
                           (*current-library*  name)
                           (*syntax-reflected* #f))       ; +++ space

                 (import-libraries-for-expand imported-libraries (map not imported-libraries) 0)
                 (env-import! keyword imports *usage-env*)

                 (let ((initial-env-table *env-table*))   ; +++ space
                   (scan-sequence library-type
                                  make-local-mapping
                                  body-forms
                                  (lambda (forms syntax-definitions bound-variables)
                                    (let* ((exports
                                            (map (lambda (mapping)
                                                   (cons (id-name (car mapping))
                                                         (let ((binding (binding (cadr mapping))))
                                                           (or binding
                                                               (syntax-violation
                                                                'library "Unbound export" t (cadr mapping)))
                                                           ;(if (binding-mutable? binding)
                                                           ;    (syntax-violation
                                                           ;     'library "Attempt to export mutable variable" t (cadr mapping)))
                                                           binding)))
                                                 exports))
                                            (library (ex:make-library
                                                   name
                                                   (if *syntax-reflected*
                                                     (compress (drop-tail *env-table* initial-env-table))
                                                     '())
                                                   exports
                                                   imported-libraries
                                                   (current-builds imported-libraries)
                                                   syntax-definitions
                                                   bound-variables
                                                   (emit-body forms 'set!)
                                                   (generate-guid 'build))))
                                        (ex:register-library! library)
                                        (case library-type
                                          ((library) #f)
                                          ((program) `(ex:import-library ',name))))))))))))))

    (define (env-import! keyword imports env)
      (env-extend! (map (lambda (import)
                          (cons (cons (car import)
                                      (id-colors keyword))
                                (cdr import)))
                        imports)
                   env))

    (define (current-builds imported-libraries)
      (map (lambda (lib-entry)
             (ex:library-build (ex:lookup-library (car lib-entry))))
           imported-libraries))

    (define (import-libraries-for-expand imports builds phase)
      (ex:import-libraries-for
       imports
       builds
       phase
       (lambda (library phase imported)
         (if (and (>= phase 0)
                  (not (ex:library-visited? library)))
             (begin
               (set! *env-table* (append (uncompress (ex:library-envs library)) *env-table*))
               (visit-library! library)
               (ex:library-visited?-set! library #t)))
         (if (and (>= phase 1)
                  (not (ex:library-invoked? library)))
             (begin 
               (ex:invoke-library! library)
               (ex:library-invoked?-set! library #t))))
       'expand))

    (define (scan-declarations declarations)
        (let loop ((declarations declarations)
                   (imported-libraries '())
                   (imports '())
                   (exports '())
                   (body-forms '()))
            (if (null? declarations)
                (let ()
                    (check-set? exports
                        (lambda (x y)
                            (eq? (id-name (car x))
                                 (id-name (car y))))
                        (lambda (dup) (syntax-violation 'export "Duplicate export" exports dup)))
                        (values imported-libraries imports exports body-forms))
            (match (car declarations)
                (((syntax import) specs ___)
                    (call-with-values
                        (lambda () (scan-imports specs imported-libraries imports))
                        (lambda (imported-libraries imports)
                          (loop (cdr declarations) imported-libraries imports exports body-forms))))
                (((syntax export) sets ___)
                    (loop (cdr declarations)
                          imported-libraries
                          imports
                          (append exports (scan-exports sets))
                          body-forms))
                (((syntax begin) forms ___)
                    (loop (cdr declarations)
                          imported-libraries
                          imports
                          exports
                          (append body-forms forms)))
                (- 
                    (syntax-violation 'define-library "Invalid declaration" (car declarations)))))))

    ;; Returns ((<rename-identifier> <identifier> <level> ...) ...)

    (define (scan-exports sets)
      (apply append (map scan-export-set sets)))

    (define (scan-export-set set)
      (match set
        ((? identifier? x)
         `((,x ,x 0)))
        (((syntax rename) ((? identifier? xs) (? identifier? ys)) ___)
         (map (lambda (x y) `(,y ,x 0)) xs ys))
        (- (syntax-violation 'export "Invalid export set" set))))

    ;; Returns
    ;;    (values ((<library reference> <level> ...) ....)
    ;;            ((<local name> . <binding>) ...))
    ;; with no repeats.

    (define (scan-imports specs imported-libraries imports)
      (if (null? specs)
          (values imported-libraries (unify-imports imports))
          (call-with-values
              (lambda () (scan-import-spec (car specs)))
            (lambda (library-ref levels more-imports)
              (scan-imports (cdr specs)
                    ;; library-ref = #f if primitives spec
                    (if library-ref
                        (cons (cons library-ref levels)
                              imported-libraries)
                        imported-libraries)
                    (append more-imports imports))))))

    ;; Returns (values <library reference> | #f
    ;;                 (<level> ...)
    ;;                 ((<local name> . <binding>) ...)
    ;; where <level> ::= <integer>
    ;; #f is returned for library name in case of primitives.

    (define (scan-import-spec spec)

      (call-with-values
          (lambda () (scan-levels spec))
        (lambda (levels import-set)
          (let loop ((import-set import-set)
                     (adjuster (lambda (set) set)))

            (define (check-presence names mappings from)
              (for-each (lambda (name)
                          (or (assq name mappings)
                              (syntax-violation from
                                                (string-append "Identifier not in set: "
                                                               (list->string (map car mappings) " "))
                                                import-set
                                                name)))
                        names))

            (match import-set
              (((syntax primitives) (? identifier? xs) ___)
               (values #f
                       levels
                       (map (lambda (mapping)
                              (cons (car mapping) (make-binding 'variable (cdr mapping) levels #f '())))
                            (adjuster (map (lambda (name) (cons name name))
                                           (syntax->datum xs))))))
              (((syntax only) set (? identifier? xs) ___)
               (let ((args (syntax->datum xs)))
                 (loop set
                       (compose adjuster (lambda (mappings)
                                           (check-presence args mappings 'only)
                                           (filter (lambda (mapping)
                                                     (memq (car mapping) args))
                                                   mappings))))))
              (((syntax except) set (? identifier? xs) ___)
               (let ((args (syntax->datum xs)))
                 (loop set
                       (compose adjuster (lambda (mappings)
                                           (check-presence args mappings 'except)
                                           (filter (lambda (mapping)
                                                     (not (memq (car mapping) args)))
                                                   mappings))))))
              (((syntax prefix) set (? identifier? pre))
               (loop set
                     (compose adjuster (lambda (mappings)
                                         (map (lambda (mapping)
                                                (cons (string->symbol
                                                       (string-append
                                                        (symbol->string (syntax->datum pre))
                                                        (symbol->string (car mapping))))
                                                      (cdr mapping)))
                                              mappings)))))
              (((syntax rename) set ((? identifier? xs) (? identifier? ys)) ___)
               (let ((args (syntax->datum (cddr import-set))))
                 (loop set
                       (compose adjuster
                                (lambda (mappings)
                                  (check-presence (map car args) mappings 'rename)
                                  (map (lambda (mapping)
                                         (cons (cond ((assq (car mapping) args) => cadr)
                                                     (else (car mapping)))
                                               (cdr mapping)))
                                       mappings))))))
              (((syntax primitives) . -) (invalid-form import-set))
              (((syntax only)       . -) (invalid-form import-set))
              (((syntax except)     . -) (invalid-form import-set))
              (((syntax prefix)     . -) (invalid-form import-set))
              (((syntax rename)     . -) (invalid-form import-set))
              (-
               (let ((library-ref (library-ref import-set)))
                 (if library-ref
                     (let* ((library (load-library (syntax->datum library-ref)))
                            (exports (ex:library-exports library))
                            (imports
                             (map (lambda (mapping)
                                    (cons (car mapping)
                                          (let ((binding (cdr (assq (cdr mapping) exports))))
                                            (make-binding (binding-type binding)
                                                          (binding-name binding)
                                                          (compose-levels levels (binding-levels binding))
                                                          (binding-mutable? binding)
                                                          (binding-library binding)))))
                                  (adjuster (map (lambda (name) (cons name name))
                                                 (map car exports))))))
                       (values (syntax->datum library-ref)
                               levels
                               imports))
                     (syntax-violation 'import "Invalid import set" import-set)))))))))

    (define (scan-levels spec)
      (match spec
        (((syntax for) set levels ___)
         (let ((levels
                (map (lambda (level)
                       (match level
                         ((syntax run)                   0)
                         ((syntax expand)                1)
                         (((syntax meta) (? integer-syntax? n)) (annotation-expression n))
                         (- (syntax-violation 'for "Invalid level in for spec" spec level))))
                     levels)))
           (check-set? levels = (lambda (dup) (syntax-violation 'for "Repeated level in for spec" spec dup)))
           (values levels set)))
        (- (values '(0) spec))))

    (define (compose-levels levels levels*)
      (apply unionv
             (map (lambda (level)
                    (map (lambda (level*)
                           (+ level level*))
                         levels*))
                  levels)))

    ;; Argument is of the form ((<local name> <binding>) ...)
    ;; where combinations (<local name> (binding-name <binding>)) may be repeated.
    ;; Return value is of the same format but with no repeats and
    ;; where union of (binding-levels <binding>)s has been taken for any original repeats.
    ;; An error is signaled if same <local name> occurs with <binding>s
    ;; whose (binding-name <binding>)s are different.

    (define (unify-imports imports)
      (let ((seen '()))
        (let loop ((imports imports))
          (if (null? imports)
              seen
              (let* ((mapping (car imports))
                     (probe (assq (car mapping) seen)))
                (if probe
                    (begin
                      (or (eq? (binding-name (cdr mapping))
                               (binding-name (cdr probe)))
                          (syntax-violation
                           'import
                           (string-append "Different bindings for identifier imported from libraries ("
                                          (list->string (binding-library (cdr mapping)) " ")
                                          ") and ("
                                          (list->string (binding-library (cdr probe)) " ") ")")
                           (car mapping)))
                      (set-cdr! probe
                                (make-binding (binding-type (cdr probe))
                                              (binding-name (cdr probe))
                                              (unionv (binding-levels (cdr probe))
                                                      (binding-levels (cdr mapping)))
                                              (binding-mutable? (cdr probe))
                                              (binding-library (cdr probe)))))
                    (set! seen (cons mapping seen)))
                (loop (cdr imports)))))))

    (define (library-name-part? p)
        (or (identifier? p) (integer-syntax? p)))

    (define (scan-library-name e) (library-ref-helper e))

    (define (library-ref e)
      (library-ref-helper
       (match e
         (((syntax define-library) name) name)
         (((syntax define-library) . -)  (invalid-form e))
         (- e))))

    (define (library-ref-helper e)
      (match e
        (((? library-name-part? ids) ___) ids)
        (- (syntax-violation 'library "Invalid library reference" e))))

    ;;==========================================================================
    ;;
    ;; Debugging facilities:
    ;;
    ;;==========================================================================

    (define (syntax-violation who message form . maybe-subform)
      (let* ((subform (cond ((null? maybe-subform) #f)
                           ((and (pair? maybe-subform)
                                 (null? (cdr maybe-subform)))
                            (car maybe-subform))
                           (else (assertion-violation 'syntax-violation
                                                      "Invalid subform in syntax violation"
                                                      maybe-subform))))
            (form-source (syntax->closest-source form))
            (subform-source (syntax->closest-source subform)))
        
        (raise-loki-error (or form-source subform-source) 
            (call-with-string-output-port (lambda (out)
                ; TODO use *trace*
                (if who (display who out))
                (if who (display " - " out))
                (display message out)
                (display "\n" out)
                (display form out))))))

    ;;==========================================================================
    ;;
    ;;  Eval and environment:
    ;;
    ;;==========================================================================

    (define eval-template
      (make-identifier 'eval-template
                       '()
                       '()
                       0
                       `(anonymous)
                       (make-source "<eval>" 1 0)))

    (define (make-environment imported-libraries env)
      (cons imported-libraries env))
    (define environment-imported-libraries car)
    (define environment-env                cdr)

    (define (environment . import-specs)
      (fluid-let ((*usage-env* (make-unit-env)))
        (env-import! eval-template (make-library-language) *usage-env*)
        (call-with-values
            (lambda () 
              (fluid-let ((*phase* 0))
                (scan-imports
                 (map (lambda (spec)
                        (datum->syntax eval-template spec))
                      import-specs) '() '())))
          (lambda (imported-libraries imports)
            (make-environment imported-libraries
                                   (let ((env (make-unit-env)))
                                     (env-import! eval-template imports env)
                                     env))))))

    (define (loki-eval exp env)
      (fluid-let ((*usage-env* (environment-env env)))
        (let ((exp (datum->syntax eval-template exp))
              (imported-libraries (environment-imported-libraries env)))
          (import-libraries-for-expand (environment-imported-libraries env) (map not imported-libraries) 0)
          (ex:import-libraries-for-run (environment-imported-libraries env) (map not imported-libraries) 0)
          (ex:runtime-eval (expand-begin
                 ;; wrap in expression begin so no definition can occur as required by r6rs
                 `(,(rename 'macro 'begin) ,exp))))))

    ;;==========================================================================
    ;;
    ;; Library reflection:
    ;;
    ;;=========================================================================

    (define (environment-bindings env)
      (map format-mapping
           (caar (environment-env env))))

    (define (format-mapping mapping)
      `((name ,(caar mapping))
        (type ,(binding-type (cdr mapping)))
        (from ,(binding-library (cdr mapping)))
        (levels ,(binding-levels (cdr mapping)))))

    ;;=====================================================================
    ;;
    ;; Utilities:
    ;;
    ;;=====================================================================

    (define (flatten l)
      (cond ((null? l) l)
            ((pair? l) (cons (car l)
                             (flatten (cdr l))))
            (else (list l))))

    (define (sexp-map f s)
      (cond ((null? s) '())
            ((pair? s) (cons (sexp-map f (car s))
                             (sexp-map f (cdr s))))
            ((vector? s)
             (apply vector (sexp-map f (vector->list s))))
            (else (f s))))

    (define (dotted-memp proc ls)
      (cond ((null? ls) #f)
            ((pair? ls) (if (proc (car ls))
                            ls
                            (dotted-memp proc (cdr ls))))
            (else (and (proc ls)
                       ls))))

    (define (dotted-map f lst)
      (cond ((null? lst) '())
            ((pair? lst) (cons (f (car lst))
                               (dotted-map f (cdr lst))))
            (else (f lst))))

    ;; Returns 0 also for non-list a la SRFI-1 protest.

    (define (dotted-length dl)
      (cond ((null? dl) 0)
            ((pair? dl) (+ 1 (dotted-length (cdr dl))))
            (else 0)))

    (define (dotted-butlast ls n)
      (let recurse ((ls ls)
                    (length-left (dotted-length ls)))
        (cond ((< length-left n) (assertion-violation 'dotted-butlast "List too short" ls n))
              ((= length-left n) '())
              (else
               (cons (car ls)
                     (recurse (cdr ls)
                              (- length-left 1)))))))

    (define (dotted-last ls n)
      (let recurse ((ls ls)
                    (length-left (dotted-length ls)))
        (cond ((< length-left n) (assertion-violation 'dotted-last "List too short" ls n))
              ((= length-left n) ls)
              (else
               (recurse (cdr ls)
                        (- length-left 1))))))

    (define (check-set? ls = fail)
      (or (null? ls)
          (if (memp (lambda (x)
                      (= x (car ls)))
                    (cdr ls))
              (fail (car ls))
              (check-set? (cdr ls) = fail))))

    (define (unionv . sets)
      (cond ((null? sets) '())
            ((null? (car sets))
             (apply unionv (cdr sets)))
            (else
             (let ((rest (apply unionv
                                (cdr (car sets))
                                (cdr sets))))
               (if (memv (car (car sets)) rest)
                   rest
                   (cons (car (car sets)) rest))))))
    
    (define (drop-tail list tail)
      (cond ((null? list)    '())
            ((eq? list tail) '())
            (else
             (cons (car list)
                   (drop-tail (cdr list) tail)))))

    (define (list->string e separator)
      (define (tostring x)
        (cond ((symbol? x)
               (symbol->string x))
              ((number? x)
               (number->string x))
              (else
               (assertion-violation 'list->string "Invalid argument" e))))
      (if (null? e)
          ""
          (string-append
           (tostring (car e))
           (apply string-append
                  (map (lambda (x)
                         (string-append separator (tostring x)))
                       (cdr e))))))

    (define (compose f g)
      (lambda (x) (f (g x))))

    (define (check x p? from)
      (or (p? x)
          (syntax-violation from "Invalid argument" x)))

    (define (invalid-form exp)
      (syntax-violation #f "Invalid form" exp))

    ;; Puts parameters to a consistent state for the toplevel
    ;; Old state is restored afterwards so that things will be
    ;; reentrant. 

    (define with-toplevel-parameters
      (lambda (thunk)
        (fluid-let ((*trace*            '())
                    (*current-library*  '())
                    (*phase*            0)
                    (*used*             (list '()))
                    (*color*            (generate-color))
                    (*usage-env*        *toplevel-env*)
                    (*syntax-reflected* #f))
          (thunk))))
    
    (define (expand-toplevel-sequence forms)
      (scan-sequence 'toplevel
                     make-toplevel-mapping
                     (source->syntax toplevel-template forms)
                     (lambda (forms syntax-definitions bound-variables)
                       (emit-body forms 'define))))

    (define (library-name-part->string p)
      (if (symbol? p) (symbol->string p)
                      (number->string p)))

    (define (library-name->filename name)
      (or (find file-exists?
            (map (lambda (dir)
                (string-append 
                    (string-join (cons dir (reverse (map library-name-part->string name))) "/") ".sld"))
                ex:library-dirs))
          (syntax-violation #f (string-join (cons "File not found for library: "
                                              (reverse (map library-name-part->string name))) " ") name)))

    (define (load-library name)
        (or
            (ex:lookup-library/false name)
            (begin (loki-load (library-name->filename name))
                   (ex:lookup-library name))))
    
    (define (loki-load filename)
      (with-toplevel-parameters
       (lambda ()
         (for-each (lambda (exp)
                     (for-each (lambda (exp)
                                 (ex:runtime-eval exp))
                               (expand-toplevel-sequence (list exp))))
                   (read-file filename)))))
      
    ;; This may be used as a front end for the compiler.
    ;; It expands a file consisting of a possibly empty sequence
    ;; of libraries optionally followed by a <toplevel program>.
    ;; The result is a sequence of vanilla r5rs-like toplevel
    ;; definitions and expressions.

    (define (expand-file filename)
      (with-toplevel-parameters
       (lambda ()
         (expand-toplevel-sequence (normalize (read-file filename))))))

    (define (expand-sequence forms)
      (with-toplevel-parameters
       (lambda ()
            (expand-toplevel-sequence (normalize forms)))))

    (define (expand-datum-sequence forms)
        (expand-sequence (datum->syntax eval-template forms)))

    ;; Keeps (<library> ...) the same.
    ;; Converts (<library> ... . <toplevel program>)
    ;; to (<library> ... (program . <toplevel program>))

    (define (normalize exps)
      (define (error)
        (let ((newline (string #\newline)))
          (syntax-violation
           'expand-file
           (string-append
            "File should be of the form:" newline
            "      <library>*" newline
            "    | <library>* <toplevel program>")
           exps)))
      (let loop ((exps exps)
                 (state 'libraries)
                 (libraries '())
                 (imports '())
                 (toplevel '()))
        (if (null? exps)
          (let ((program (if (pair? toplevel)
                               `(,(datum->syntax toplevel-template 'program)
                                 (,(datum->syntax toplevel-template (generate-guid 'program)))
                                 ,@imports
                                 (,(datum->syntax toplevel-template 'begin) ,@toplevel))
                               '())))
            (append (reverse (cons program libraries))))
          (let ((exp (car exps)))
            (case state
              ((libraries)
                (if (pair? exp)
                  (case (unwrap-annotation (car exp))
                    ((define-library) (loop (cdr exps) 
                                            'libraries
                                            (cons exp libraries)
                                            imports
                                            toplevel))
                    ((import) (loop (cdr exps) 
                                    'imports
                                    libraries
                                    (cons exp imports)
                                    toplevel))
                    (else (loop (cdr exps)
                                'program
                                libraries
                                imports
                                (cons exp toplevel))))
                  (loop (cdr exps) 'program libraries imports (cons exp toplevel))))
               ((imports)
                  (case (unwrap-annotation (car exp))
                    ((import) (loop (cdr exps) 
                                    'imports
                                    libraries
                                    (cons exp imports)
                                    toplevel))
                    (else (loop (cdr exps)
                                'program
                                libraries
                                imports
                                (cons exp toplevel)))))
               ((program)
                  (loop (cdr exps) 'program libraries imports (cons exp toplevel))))))))

    (define (read-file fn)
        (let* ((p (open-input-file fn))
               (reader (make-reader p fn)))
          (let f ((x (read-annotated reader)))
            (if (and (annotation? x)
                     (eof-object? (annotation-expression x)))
                '()
                (cons x (f (read-annotated reader)))))))

    ;;==========================================================================
    ;;
    ;; Toplevel bootstrap:
    ;;
    ;;==========================================================================

    (define toplevel-template
      (make-identifier 'toplevel-template
                       '()
                       '()
                       0
                       #f
                       (make-source "<toplevel>" 1 0)))

    (define (source->syntax tid datum)
        (add-context-to-identifiers tid datum))

    ;;===================================================================
    ;;
    ;; Language for bootstrapping the REPL session and (environment ---):
    ;;
    ;;===================================================================

    (define library-language-names
      `(program define-library export import begin for run expand meta only
                except prefix rename primitives))

    (define (make-library-language)
      (map (lambda (name)
             (cons name (make-binding 'macro name '(0) #f '())))
           library-language-names))

    ;;===================================================================
    ;;
    ;; Bootstrap library containing macros defined in this expander.
    ;;
    ;;===================================================================

    (ex:register-library!
     (let ((primitive-macro-mapping
            `((lambda        . ,expand-lambda)
              (if            . ,expand-if)
              (set!          . ,expand-set!)
              (begin         . ,expand-begin)
              (syntax        . ,expand-syntax)
              (quote         . ,expand-quote)
              (let-syntax    . ,expand-local-syntax)
              (letrec-syntax . ,expand-local-syntax)
              (syntax-case   . ,expand-syntax-case)
              (and           . ,expand-and)
              (or            . ,expand-or)
              (define        . ,invalid-form)
              (define-syntax . ,invalid-form)
              (_             . ,invalid-form)
              (...           . ,invalid-form))))
       (ex:make-library
        '(core primitive-macros)
        ;; envs
        '()
        ;; exports
        (map (lambda (mapping)
               (cons (car mapping) (make-binding 'macro (car mapping) '(0) #f '())))
             primitive-macro-mapping)
        ;; imported-libraries
        '()
        ;; builds
        '()
        ;; syntax-defs
        (map (lambda (mapping)
               (cons (car mapping) (make-expander (cdr mapping))))
             primitive-macro-mapping)
        ;; bound-vars
        '()
        ;; forms
        '()
        ;; build
        'system)))

    ;; Initial environments:

    (set! *toplevel-env* (make-unit-env))
    (set! *usage-env*    *toplevel-env*)

    ;; Import only the minimal library language into the toplevel:

    (env-import! toplevel-template (make-library-language) *toplevel-env*)
    (register-macro! 'define-library (make-expander invalid-form))
    (register-macro! 'program (make-expander invalid-form))
    (register-macro! 'import  (make-expander invalid-form))

    ;;==========================================================================
    ;;
    ;; Exports:
    ;;
    ;;==========================================================================

    (set! ex:make-variable-transformer make-variable-transformer)
    (set! ex:identifier?               identifier?)
    (set! ex:bound-identifier=?        bound-identifier=?)
    (set! ex:free-identifier=?         free-identifier=?)
    (set! ex:generate-temporaries      generate-temporaries)
    (set! ex:datum->syntax             datum->syntax)
    (set! ex:syntax->datum             syntax->datum)
    (set! ex:environment               environment)
    (set! ex:environment-bindings      environment-bindings)
    (set! ex:eval                      loki-eval)
    (set! ex:load                      loki-load)
    (set! ex:syntax-violation          syntax-violation)
    (set! ex:expand-file               expand-file)
    (set! ex:expand-sequence           expand-sequence)
    (set! ex:expand-datum-sequence     expand-datum-sequence)

    (set! ex:invalid-form              invalid-form)
    (set! ex:register-macro!           register-macro!)
    (set! ex:syntax-rename             syntax-rename)
    (set! ex:dotted-length             dotted-length)
    (set! ex:dotted-butlast            dotted-butlast)
    (set! ex:dotted-last               dotted-last)
    (set! ex:free=?                    free=?)

    ;; Register the expander's primitive API surface with the runtime
    (ex:runtime-add-primitive 'ex:make-variable-transformer ex:make-variable-transformer)
    (ex:runtime-add-primitive 'ex:identifier? ex:identifier?)
    (ex:runtime-add-primitive 'ex:bound-identifier=? ex:bound-identifier=?)
    (ex:runtime-add-primitive 'ex:free-identifier=? ex:free-identifier=?)
    (ex:runtime-add-primitive 'ex:generate-temporaries ex:generate-temporaries)
    (ex:runtime-add-primitive 'ex:datum->syntax ex:datum->syntax)
    (ex:runtime-add-primitive 'ex:syntax->datum ex:syntax->datum)
    (ex:runtime-add-primitive 'ex:environment ex:environment)
    (ex:runtime-add-primitive 'ex:environment-bindings ex:environment-bindings)
    (ex:runtime-add-primitive 'ex:eval ex:eval)
    (ex:runtime-add-primitive 'ex:load ex:load)
    (ex:runtime-add-primitive 'ex:syntax-violation ex:syntax-violation)
    (ex:runtime-add-primitive 'ex:expand-file ex:expand-file)
    (ex:runtime-add-primitive 'ex:expand-sequence ex:expand-sequence)
    (ex:runtime-add-primitive 'ex:expand-datum-sequence ex:expand-datum-sequence)

    (ex:runtime-add-primitive 'ex:invalid-form ex:invalid-form)
    (ex:runtime-add-primitive 'ex:register-macro! ex:register-macro!)
    (ex:runtime-add-primitive 'ex:syntax-rename ex:syntax-rename)
    (ex:runtime-add-primitive 'ex:dotted-length ex:dotted-length)
    (ex:runtime-add-primitive 'ex:dotted-butlast ex:dotted-butlast)
    (ex:runtime-add-primitive 'ex:dotted-last ex:dotted-last)
    (ex:runtime-add-primitive 'ex:free=? ex:free=?)

    ;; Load the r7rs standard library into the expander
    (ex:expand-file "src/loki/r7rs.scm")

    ) ; let
  ) ; letrec-syntax
))
