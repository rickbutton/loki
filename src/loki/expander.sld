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
(import (scheme case-lambda))
(import (srfi 1))
(import (srfi 128))
(import (srfi 146 hash))
(import (loki shared))
(import (loki runtime))
(import (loki util))
(import (loki message))
(import (loki reader))
(import (loki compiler))
(import (loki path))
(import (lang core))

(export (rename identifier?               ex:identifier?)
        (rename bound-identifier=?        ex:bound-identifier=?)
        (rename free-identifier=?         ex:free-identifier=?)
        (rename generate-temporaries      ex:generate-temporaries)
        (rename datum->syntax             ex:datum->syntax)
        (rename syntax->datum             ex:syntax->datum)
        (rename environment               ex:environment)
        (rename environment-bindings      ex:environment-bindings)
        (rename loki-eval                 ex:eval)
        (rename loki-load                 ex:load)
        (rename syntax-violation          ex:syntax-violation)
        (rename loki-features             ex:features)
    
        (rename expand-file               ex:expand-file)
        (rename expand-datum-sequence     ex:expand-datum-sequence)

        (rename invalid-form              ex:invalid-form)
        (rename register-macro!           ex:register-macro!)
        (rename syntax-rename             ex:syntax-rename)
        (rename map-while                 ex:map-while)
        (rename dotted-length             ex:dotted-length)
        (rename dotted-butlast            ex:dotted-butlast)
        (rename dotted-last               ex:dotted-last)
        (rename free=?                    ex:free=?))
(begin


;; Single-character symbol prefixes.
(define free-prefix "~")
(define library-prefix "#")

;; A trivial but extremely useful s-expression matcher.
;; Implements a subset of Wright's matcher's patterns.
;; Includes additional (syntax id) pattern that matches
;; if input is identifier? and free=? to 'id.
(define-syntax match
  (syntax-rules ()
    ((match (op arg ...) clause ...)
     (let ((x (op arg ...)))
       (match x clause ...)))
    ((match x)
     (invalid-form x))
    ((match x (pat e ...) clause ...)
     (matcher "base" pat "done" x (e ...) (lambda () (match x clause ...))))))

(define-syntax matcher
  (syntax-rules (- ___ ? syntax)
    ((matcher "base" () k arg ...)
     (matcher k (lambda (x sk fk) (if (null? x) (sk) (fk))) () arg ...))
    ((matcher "base" - k arg ...)
     (matcher k (lambda (x sk fk) (sk)) () arg ...))
    ((matcher "base" (syntax id) k arg ...)
     (matcher k
              (lambda (x sk fk)
                (if (free=? x 'id) (sk) (fk)))
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
     (code x (lambda vars e ...) fk))))

;;==========================================================================
;;
;; Dynamic parameters:
;;
;;==========================================================================

;; toplevel REPL bindings to be initialized later
(define *toplevel-env*     #f)
;; current lexical environment to be initialized later
(define *usage-env*        #f)
;; current phase
(define *phase*            0)
;; current color for painting identifiers upon renaming to be initialized
(define *color*            #f)
;; global table mapping <binding name> of keyword to <macro> object
(define *macro-table*      '())
;; maps <symbolic key> of reflected environment to actual <environment>
(define *env-table*        '())
;; current library name as list of symbols or '() for toplevel
(define *current-library*  '())
;; car of this records bindings already referenced in current body
;; for detecting when later definitions may violate lexical scope
(define *used*             (list '()))
;; history trace for error reporting
(define *trace*            '())
;; the current module handler
(define *module-handler*   #f)
;; the current 'identifier' being bound
;; this is used to assign an "identifier" to lambdas
;; TODO - is this a hack?
(define *current-identifier-bind* #f)
;; whether expanded library introduces identifiers via syntax
;; expressions - if not, save lots of space by not including
;; env-table in object code
(define *syntax-reflected* #f)

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

;; Used to generate user program toplevel names.
;; Prefix makes it disjoint from all builtins.
;; Prefix makes it disjoint from output of generate-guid.
;; Must be read-write invariant.

(define (make-free-name symbol)
  (string->symbol (string-append free-prefix (symbol->string symbol))))

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

(define-record-type <binding>
  (make-binding type name levels content library)
  binding?
  (type binding-type)
  (name binding-name)
  (levels binding-levels)
  (content binding-content binding-content-set!)
  (library binding-library))

(define binding-mutable? binding-content)
(define binding-dimension binding-content)
(define binding-mutable-set! binding-content-set!)

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
                          " in library (" (join (id-library id) " ")
                          ") at invalid level " (number->string (source-level id))
                          ".  Binding is only available at levels: "
                          (join (binding-levels binding) " "))
           id))
      (or (and (null? (id-library id))
               (= *phase* 0))
          (syntax-violation
           "invalid reference"
           (string-append "No binding available for " (symbol->string (id-name id))
                          " in library (" (join (id-library id) " ") ")")

           id))))

;;=========================================================================
;;
;; Environments:
;;
;;=========================================================================

;; Frames can be added, or the leftmost frame can be destructively
;; updated in the case of binding constructs such as bodies where
;; definitions are incrementally discovered.

(define (make-null-env) '())
(define (make-unit-env) (env-extend '() (make-null-env)))

(define (make-null-frame) (hashmap (make-default-comparator)))

(define (mappings->frame mappings)
  (list (alist->hashmap! (make-null-frame) mappings)))
(define (mappings->frame! frame mappings)
  (set-car! frame (alist->hashmap! (car frame) mappings)))

;; Adds a new frame containing mappings to env.

(define (env-extend mappings env)
  (cons (mappings->frame mappings) env))

;; Destructively extends the leftmost frame in env.

(define (env-extend! mappings env)
  (let ((frame (car env)))
    (mappings->frame! frame mappings)))

(define (frame-lookup key frame)
  (hashmap-ref (car frame) key (lambda () #f) (lambda (value) (cons key value))))

;; Returns <object> | #f

(define (env-lookup key env)
  (and (pair? env)
       (or (let* ((frame (car env))
                  (probe (frame-lookup key frame)))
             (and probe
                  (or (cdr probe)
                      (syntax-violation
                       #f "Out of context reference to identifier" (car key)))))
           (env-lookup key (cdr env)))))

;; Is id already bound in leftmost frame?

(define (duplicate? id env)
  (let ((frame (car env)))
    (frame-lookup (cons (id-name id) (id-colors id))
                  frame)))

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

; TODO - make compress, uncompress work for fancier frames
(define (compress env-table)
  env-table
  #;(let ((frame-table '())
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
  compressed-env-table
  #;(if (null? compressed-env-table) '()
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
    (core::apply-anon ex:syntax-rename
                      (core::atomic (id-name id))
                      (core::atomic (id-colors id))
                      (core::atomic (cons (env-reflect *usage-env*)
                                          (id-transformer-envs id)))
                      (core::atomic (- (- *phase* (id-displacement id)) 1))
                      (core::atomic (id-library id))
                      (core::atomic (source-file source))
                      (core::atomic (source-line source))
                      (core::atomic (source-column source)))))

(define (syntax-rename name colors transformer-envs transformer-phase source-library file line column)
  (make-identifier name
                   (cons *color* colors)
                   transformer-envs
                   (- *phase* transformer-phase)
                   source-library
                   (make-source file line column)))

(define (binding->core::ref binding)
  (core::ref (binding-name binding)))

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
                   
(define (syntax->datum exp)
  (sexp-map (lambda (leaf)
              (cond ((identifier? leaf) (id-name leaf))
                    ((annotation? leaf) (annotation-expression leaf))
                    ((symbol? leaf)
                     (error "syntax->datum: A symbol is not a valid syntax object" leaf))
                    (else leaf)))
            exp))

(define (datum->syntax tid datum)
  (unless (identifier? tid)
    (error "datum->syntax: Invalid form" tid))
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

(define-record-type <macro>
  (make-macro type proc exp)
  macro?
  (type macro-type)
  (proc macro-proc)
  (exp macro-exp))

(define (make-expander proc)             (make-macro 'expander proc #f))
(define (make-transformer proc exp)          (make-macro 'transformer proc exp))

(define (make-user-macro procedure-or-macro)
  (cond
    ((macro? procedure-or-macro)
      procedure-or-macro)
    ((procedure? procedure-or-macro)
      (make-transformer procedure-or-macro #f))
    (else
      (error "Invalid user macro" procedure-or-macro))))

;; Returns <macro>.

(define (binding->macro binding t)
  (cond ((assq (binding-name binding) *macro-table*) => cdr)
        (else
         (raise (make-loki-error t "Reference to macro keyword out of context")))))

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
                  (register-macro! name (make-transformer (rt:runtime-run-expression macro) (compile-core-to-host-scheme (list macro)))))))
           (rt:library-syntax-defs library)))

;; Calls a macro with a new color.

(define (invoke-macro macro t)
  (set! *color* (generate-color))
  ((macro-proc macro) t))

(define (make-call-trace id k)
  k
  #;`(%trace ,(if id
                (string-append (symbol->string (id-name id))
                               " "
                               (source->string (id-source id)))
                'unknown)
             ,k))

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
                            (core::apply (binding->core::ref binding)
                                         (map expand (cdr t)))
                            (binding->core::ref binding)))
                       ((pattern-variable)
                       (begin
                        (syntax-violation #f "Pattern variable used outside syntax template" t)))))
            ((null? t)       (core::atomic '()))
            ((list? t)       (core::apply (expand (car t)) (map expand (cdr t))))
            ((identifier? t) (core::ref (make-free-name (id-name t))))
            ((pair? t)       (syntax-violation #f "Invalid procedure call syntax" t))
            ((symbol? t)     (syntax-violation #f "Symbol may not appear in syntax object" t))
            (else            (core::atomic (syntax->datum t)))))))

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
    ((quote datum) (core::atomic (syntax->datum datum)))))

(define (expand-if exp)
  (match exp
    ((- e1 e2 e3) (core::if (expand e1) (expand e2) (expand e3)))
    ((- e1 e2)    (core::if (expand e1) (expand e2) (core::anon-ref %void)))))

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
          (core::set! (binding->core::ref binding) (expand e)))
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
                      (core::let '() (map cdr forms)))))))

(define (scan-let-bindings bindings k) 
  (let loop ((bindings bindings)
             (formals '())
             (vals    '()))
    (if (null? bindings)
      (k formals vals)
      (match (car bindings)
        ((formal val)
          (loop (cdr bindings)
                (cons formal formals)
                (cons val vals)))))))

(define (let-names? names)
  (every identifier? names))

(define (check-let-binding-names names)
  (for-all (lambda (name)
             (unless (identifier? name) (syntax-violation 'let "Invalid let binding name" name)))
           names))

(define (expand-let exp)
  (match exp
    ((- ((names values) ___) body ___)
     (check-let-binding-names names)
     (scan-sequence 'expression-sequence
                    make-local-mapping
                    values
       (lambda (val-forms syntax-definitions bound-variables)
         (fluid-let ((*usage-env*
                      (env-extend (map (lambda (name) (make-local-mapping 'variable name #f)) names)
                                  *usage-env*)))
           (let ((bindings (map binding names)))
             ;; Scan-sequence expects the caller to have prepared
             ;; the frame to which to destructively add bindings.
             ;; Let bodies need a fresh frame.
             (fluid-let ((*usage-env* (env-extend '() *usage-env*)))
               (scan-sequence 'lambda
                              make-local-mapping
                              body
                              (lambda (body-forms syntax-definitions bound-variables)
                                (core::let
                                  (map (lambda (name binding val) (core::let-var (binding->core::ref binding) (cdr val)))
                                       names bindings val-forms)
                                  (emit-body body-forms emit-never-global))))))))))))

;; Expression let(rec)-syntax:

(define (expand-local-syntax exp)
  (expand-let `(,(rename 'macro 'let) () ,@(cdr exp))))

;; Define and and or as primitives so we can import them into the
;; toplevel without spoiling the and and or of the library language.

(define (expand-and exp)
  (match exp
    ((and) (core::atomic #t))
    ((and e) (expand e))
    ((and e es ___)
     (core::if (expand e)
                    (expand `(,and ,@es))
                    (core::atomic #f)))))

(define (expand-or exp)
  (match exp
    ((or) (core::atomic #t))
    ((or e) (expand e))
    ((or e es ___)
     (let ((x (generate-guid 'x)))
       (core::let
         (list (core::let-var (core::ref x) (expand e)))
         (list (core::if (core::ref x)
                        (core::ref x)
                        (expand `(,or ,@es)))))))))

(define (check-valid-include type file) 
 (unless (string? file)
        (syntax-violation type
          "Invalid include syntax, requires string literal" file)))
(define (resolve-include-path id path)
  (let* ((source (id-source id))
         (root (make-path (if source (source-file source) ""))))
    (path-join (path-parent root) path)))

(define (expand-include-file exp fold-case?)
  (match exp
    ((include) (syntax-violation (syntax->datum include) "Invalid include syntax" exp include))
    ((include file)
      (check-valid-include (syntax->datum include) file)
      (let ((content (read-file (resolve-include-path include (syntax->datum file)) fold-case?)))
        (expand `(,(rename 'macro 'begin) ,@(source->syntax include content)))))
    ((include file files ___)
        (expand `(,(rename 'macro 'begin) (,include ,file) (,include ,@files))))))

(define (expand-include exp) (expand-include-file exp #f))
(define (expand-include-ci exp) (expand-include-file exp #t))

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
       (let-values (((named-formals rest-formal) (scan-formals formals)))
         (let ((named-bindings (map (lambda (formal) (binding formal)) named-formals))
               (rest-binding (if rest-formal (binding rest-formal) #f))
               (id-bind *current-identifier-bind*))
           ;; Scan-sequence expects the caller to have prepared
           ;; the frame to which to destructively add bindings.
           ;; Lambda bodies need a fresh frame.
           (fluid-let ((*usage-env* (env-extend '() *usage-env*)))
             (scan-sequence 'lambda
                            make-local-mapping
                            body
                            (lambda (forms syntax-definitions bound-variables)
                              (core::lambda (map (lambda (formal binding) (binding->core::ref binding))
                                                      named-formals
                                                      named-bindings)
                                            (if rest-binding (binding->core::ref rest-binding) #f)
                                            (emit-body forms emit-never-global)))))))))))

(define (scan-formals formals)
  (let loop ((x formals)
             (named '())
             (rest #f) )
    (cond
      ((pair? x) (loop (cdr x) (cons (car x) named) rest))
      ((null? x) (values (reverse named) rest))
      (else (loop '() named x)))))

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

(define-record-type <wrap>
  (make-wrap env exp id)
  wrap?
  (env wrap-env)
  (exp wrap-exp)
  (id wrap-id))

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
                       (fluid-let ((*usage-env* (wrap-env exp))
                                   (*current-identifier-bind* (wrap-id exp)))
                         (expand (wrap-exp exp)))
                       exp))))
         forms))

  (let ((common-env *usage-env*))

    ;; Add new frame for keeping track of bindings used
    ;; so we can detect redefinitions violating lexical scope.
    (add-fresh-used-frame!)

    (let loop ((ws (map (lambda (e) (make-wrap common-env e #f))
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
                   (expand-program form)
                   (loop (cdr ws)
                         forms
                         syntax-defs
                         bound-variables))
                  ((define-library)
                   (expand-library form)
                   (loop (cdr ws)
                         forms
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
                                         (make-wrap *usage-env* rhs id))
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
                         (let ((rhs-expanded (fluid-let ((*phase* (+ 1 *phase*)))
                                      (expand rhs))))
                           (register-macro! (binding-name (cdr mapping)) (make-transformer (rt:runtime-run-expression rhs-expanded) rhs))
                           (loop (cdr ws)
                                 forms
                                 (cons (cons (binding-name (binding id)) rhs-expanded) syntax-defs)
                                 bound-variables))))))
                  ((begin)
                   (or (list? form)
                       (invalid-form form))
                   (loop (append (map (lambda (exp)
                                        (make-wrap *usage-env* exp #f))
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
                              (macros (map (lambda (e) (rt:runtime-run-expression e)) rhs-expanded)))
                         (for-each (lambda (mapping macro rhs)
                                     (register-macro! (binding-name (cdr mapping)) (make-transformer macro rhs)))
                                   usage-diff
                                   macros
                                   rhs)
                         (fluid-let ((*usage-env* extended-env))
                           (loop (cdr ws)
                                 (cons (list #f #f (expand-let `((,rename 'let) () ,@body))) forms)
                                 syntax-defs
                                 bound-variables))))))
                  (else
                   (loop (cdr ws)
                         (cons (list #f #t (make-wrap *usage-env* form #f))
                               forms)
                         syntax-defs
                         bound-variables))))))))))))

(define (emit-never-global g) 'define)
(define (emit-always-global g) 'define-global!)
(define (bound-variables->emit-global? bound-variables)
  (lambda (g)
    (if (member g bound-variables)
        'define-global!
        'define)))

(define (emit-body body-forms define-emitter)
  (map (lambda (body-form)
         (if (symbol? (car body-form))
             (let ((definer (define-emitter (car body-form)))
                   (ref (core::ref (car body-form))))
               (case definer
                 ((define) (core::define ref (cdr body-form)))
                 ((define-global!) (core::define-global! ref (cdr body-form)))
                 (else (error "invalid emit-body definer" definer))))
             (cdr body-form)))
       body-forms))

(define (parse-definition exp syntax-def?)
  (match exp
    ((- (? identifier? id))
     (values id (rename 'variable '%void)))
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
  (unless (eq? body-type 'toplevel)
    (let ((dup (duplicate? id common-env)))
      (if dup
       (begin
        (syntax-violation type "Redefinition of identifier in body" (binding id) id)))))
  (check-used id body-type form))

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
     (let ((input (core::ref (generate-guid 'input))))
       (core::let (list (core::let-var input (expand e)))
                  (list (process-clauses clauses input literals)))))))

(define (process-clauses clauses input literals)
  (define (literal? pattern)
    (and (identifier? pattern)
         (memp (lambda (x)
                 (bound-identifier=? x pattern))
               literals)))

  (define (process-match input pattern sk fk)
    (if (not (core::ref? input))
        (let ((temp (core::ref (generate-guid 'temp))))
          (core::let (list (core::let-var temp input))
                     (list (process-match temp pattern sk fk))))
        (match pattern
          ((syntax _)         sk)
          ((syntax ...)       (syntax-violation 'syntax-case "Invalid use of ellipses" pattern))
          (()                 (core::if (core::apply-anon %null? input) sk fk))
          ((? literal? id)
            (core::if (core::if (core::apply-anon ex:identifier? input)
                                (core::if (core::apply-anon ex:free-identifier=? input (syntax-reflect id))
                                          (core::atomic #t)
                                          (core::atomic #f))
                                (core::atomic #f))
                      sk
                      fk))
          ((? identifier? id) 
          (core::let (list (core::let-var (binding->core::ref (binding id)) input))
                                         (list sk)))
          ((p (syntax ...))
           (let* ((pvars (map car (pattern-vars p 0)))
                  (bindings (map binding pvars))
                  (refs (map binding->core::ref bindings)))
             (if (and (identifier? p)                               ; +++
                      (= (length refs) 1))                          ; +++
                 (let ((ref (car refs)))
                   (core::if (core::apply-anon list? input)
                             (core::let (list (core::let-var ref input))
                                        (list sk))
                             fk))
                 (let ((columns (core::ref (generate-guid 'cols)))
                       (rest    (core::ref (generate-guid 'rest))))
                   (core::apply-anon ex:map-while
                                (core::lambda (list input)
                                              #f
                                              (list (process-match input
                                                                   p
                                                                   (core::apply (core::anon-ref list) refs)
                                                                   (core::atomic #f))))
                                input
                                (core::lambda (list columns rest)
                                              #f
                                              (list (core::if (core::apply-anon %null? rest)
                                                              (core::apply-anon %apply
                                                                           (core::lambda refs #f (list sk))
                                                                           (core::if (core::apply-anon %null? columns)
                                                                                     (core::atomic (map (lambda (i) '()) pvars))
                                                                                     (core::apply-anon %apply
                                                                                                       (core::anon-ref map)
                                                                                                       (core::anon-ref list)
                                                                                                       columns)))
                                                              fk))))))))
          ((p (syntax ...) . tail)
           (let ((tail-length (core::atomic (dotted-length tail))))
             (core::if (core::apply-anon %gte (core::apply-anon ex:dotted-length input) tail-length)
                       (process-match (core::apply-anon ex:dotted-butlast input tail-length)
                                      `(,p ,(cadr pattern))
                                      (process-match (core::apply-anon ex:dotted-last input tail-length)
                                                     tail
                                                     sk
                                                     fk)
                                      fk)
                       fk)))
          ((p1 . p2)
           (core::if (core::apply-anon %pair? input)
                     (process-match (core::apply-anon %car input)
                                    p1
                                    (process-match (core::apply-anon %cdr input) p2 sk fk)
                                    fk)
                    fk))
          (#(ps ___)
           (core::if (core::apply-anon %vector? input)
                     (process-match (core::apply-anon vector->list input)
                                    ps
                                    sk
                                    fk)
                     fk))
          ((? symbol? -)
           (syntax-violation 'syntax-case "Symbol object may not appear in pattern" pattern))
          (other
            #t
           (core::if (core::apply-anon %equal?
                                       (core::apply-anon ex:syntax->datum input)
                                       (core::atomic (syntax->datum other)))
                     sk
                     fk)))))

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
                               (core::if (expand fender)
                                         (expand template)
                                         fk))
                              (- (syntax-violation 'syntax-case "Invalid clause" clause)))
                            fk)))))))

  ;; process-clauses

  (match clauses
    (()
     (core::apply-anon ex:invalid-form input))
    ((clause clauses ___)
     (let ((fail (core::ref (generate-guid 'fail))))
       (core::let (list (core::let-var fail (core::lambda '() #f (list (process-clauses clauses input literals)))))
                  (list (process-clause clause input (core::apply fail '()))))))))
          

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
                         (binding->core::ref binding))
                       (syntax-violation 'syntax "Template dimension error (too few ...'s?)" id))))
             (else
               (syntax-reflect id)))))
    (((syntax ...) p)
     (process-template p dim #t))
    ((? (lambda (_) (not ellipses-quoted?))
        (t (syntax ...) . tail))
     (let* ((head (segment-head template)) 
            (mappings
             (map (lambda (mapping)
                    (let ((id      (car mapping))
                          (binding (cdr mapping)))
                      (check-binding-level id binding)
                      (register-use! id binding)
                      mapping))
                  (free-meta-variables head (+ dim 1) '() 0)))
            (ids (map car mappings))
            (bindings (map cdr mappings))
            (refs (map binding->core::ref bindings)))
       (if (null? mappings)
           (syntax-violation 'syntax "Too many ...'s" template)
           (let* ((x (process-template head (+ dim 1) ellipses-quoted?))
                  (gen (if (and (core::ref? x) (equal? (list (core::ref-name x)) (map core::ref-name refs)))
                           x                              ; +++
                           (if (= (length mappings) 1) 
                               (core::apply (core::anon-ref map)
                                            (cons (core::lambda refs #f (list x))
                                                  refs))
                               (core::if (core::apply (core::anon-ref %number-eq)
                                                      (map (lambda (ref) (core::apply-anon length ref)) refs))
                                         (core::apply (core::anon-ref map)
                                                      (cons (core::lambda refs #f (list x))
                                                            refs))
                                         (core::apply (core::anon-ref syntax-violation)
                                                      (list (core::atomic 'syntax)
                                                            (core::atomic 
                                                              "Pattern variables denoting lists of unequal length preceding ellipses")
                                                            (core::atomic (syntax->datum template))
                                                            (core::apply (core::anon-ref list)
                                                                         refs)))))))
                  (gen (if (> (segment-depth template) 1)
                           (core::apply-anon %apply (core::anon-ref append) gen)
                           gen)))
             (if (null? (segment-tail template))   ; +++
                 gen                               ; +++
                 (core::apply (core::anon-ref append)
                              (list gen (process-template (segment-tail template) dim ellipses-quoted?))))))))
    ((t1 . t2)
     (core::apply (core::anon-ref %cons)
                  (list (process-template t1 dim ellipses-quoted?)
                        (process-template t2 dim ellipses-quoted?))))
    (#(ts ___)
     (core::apply (core::anon-ref list->vector)
                  (list (process-template ts dim ellipses-quoted?))))
    (other 
     (let ((out
     (expand other)))
     ;(unless (null? out) (error "out" other out))
     ;(core::atomic (syntax->datum out))
     out
     ))))

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
                                                            'library "Unbound export" (cadr mapping) t))
                                                       (if (binding-mutable? binding)
                                                           (syntax-violation
                                                            'library "Attempt to export mutable variable" t (cadr mapping)))
                                                       binding)))
                                             exports))
                                        (library (rt:make-library
                                               name
                                               (if *syntax-reflected*
                                                 (compress (drop-tail *env-table* initial-env-table))
                                                 '())
                                               exports
                                               imported-libraries
                                               (current-builds imported-libraries)
                                               syntax-definitions
                                               bound-variables
                                               (emit-body forms (bound-variables->emit-global? bound-variables))
                                               (generate-guid 'build))))
                                    (rt:register-library! library)
                                    (if *module-handler*
                                      (*module-handler* library (eq? library-type 'program))))))))))))))

(define (env-import! keyword imports env)
  (env-extend! (map (lambda (import)
                      (cons (cons (car import)
                                  (id-colors keyword))
                            (cdr import)))
                    imports)
               env))

(define (current-builds imported-libraries)
  (map (lambda (lib-entry)
         (rt:library-build (rt:lookup-library (car lib-entry))))
       imported-libraries))

(define (import-libraries-for-expand imports builds phase)
  (rt:import-libraries-for
   imports
   builds
   phase
   (lambda (library phase imported)
     (if (and (>= phase 0)
              (not (rt:library-visited? library)))
         (begin
           (set! *env-table* (append (uncompress (rt:library-envs library)) *env-table*))
           (visit-library! library)
           (rt:library-visited?-set! library #t)))
     (if (and (>= phase 1)
              (not (rt:library-invoked? library)))
         (begin 
           (rt:invoke-library! library)
           (rt:library-invoked?-set! library #t))))
   'expand))

(define (scan-declarations declarations)
    (let loop ((declarations declarations)
               (imported-libraries '())
               (imports '())
               (exports '())
               (body-forms '()))
      (define (scan-cond-expand-feature-clause feature-clause)
        (match feature-clause
          ((syntax else) #t)
          (((syntax and)) #t)
          (((syntax and) req1 req2 ___)
            (and (scan-cond-expand-feature-clause req1)
                 (scan-cond-expand-feature-clause req2)))
          (((syntax or)) #f)
          (((syntax or) req1 req2 ___)
            (or (scan-cond-expand-feature-clause req1)
                (scan-cond-expand-feature-clause req2)))
          (((syntax not) req) (not (scan-cond-expand-feature-clause req)))
          ((? identifier? feature-id) (feature? (syntax->datum feature-id)))))

      (define (scan-cond-expand-clauses clauses)
        (if (null? clauses)
          (loop (cdr declarations)
                imported-libraries
                imports
                exports
                body-forms)
          (let ((clause (car clauses)))
            (match clause
              ((feature-clause body-clause ___)
                (if (scan-cond-expand-feature-clause feature-clause)
                  (loop (append body-clause (cdr declarations))
                      imported-libraries
                      imports
                      exports
                      body-forms)
                  (scan-cond-expand-clauses (cdr clauses))))))))
        (define (scan-include-library-declarations op includes)
          (if (null? includes)
            (loop (cdr declarations)
                  imported-libraries
                  imports
                  exports
                  body-forms)
            (let* ((include (car includes))
                   (str (syntax->datum include)))
              (check-valid-include 'include-library-declarations str)
              (let ((content (read-file (resolve-include-path op str) #f)))
                (loop (append (source->syntax op content) (cdr declarations))
                      imported-libraries
                      imports
                      exports
                      body-forms)))))

        (if (null? declarations)
            (let ()
                (check-set? exports
                    (lambda (x y)
                        (eq? (id-name (car x))
                             (id-name (car y))))
                    (lambda (dup) (syntax-violation 'export "Duplicate export" dup exports)))
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
            (((syntax cond-expand) clauses ___)
                (scan-cond-expand-clauses clauses))
            (((syntax include) includes ___)
                (loop (cdr declarations)
                      imported-libraries
                      imports
                      exports
                      (append body-forms (apply append (scan-includes (caar declarations) includes #f)))))
            (((syntax include-ci) includes ___)
                (loop (cdr declarations)
                      imported-libraries
                      imports
                      exports
                      (append body-forms (apply append (scan-includes (caar declarations) includes #t)))))
            (((syntax include-library-declarations) includes ___)
                (scan-include-library-declarations (caar declarations) includes))
            (((syntax begin) forms ___)
                (loop (cdr declarations)
                      imported-libraries
                      imports
                      exports
                      (append body-forms forms)))
            (- 
                (syntax-violation 'define-library "Invalid declaration" (car declarations)))))))

;; Returns ((<rename-identifier> <identifier> <level> ...) ...)

(define (scan-includes op includes fold-case?)
  (if (null? includes)
    '()
    (let* ((include (car includes))
           (str (syntax->datum include)))
      (check-valid-include (syntax->datum op) str)
      (let ((content (read-file (resolve-include-path op str) fold-case?)))
        (cons (source->syntax op content) (scan-includes op (cdr includes) fold-case?))))))

(define (scan-exports sets)
  (apply append (map scan-export-set sets)))

(define (scan-export-set set)
  (match set
    ((? identifier? x)
     `((,x ,x 0)))
    (((syntax rename) (? identifier? x) (? identifier? y))
     `((,y ,x 0)))
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
                                                           (join (map car mappings) " "))
                                            name
                                            import-set)))
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
                        (exports (rt:library-exports library))
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
                                      (join (binding-library (cdr mapping)) " ")
                                      ") and ("
                                      (join (binding-library (cdr probe)) " ") ")")
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
    (- (raise (make-loki-error e "Invalid library reference.")))))

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
                       (else (error "syntax-violation: Invalid subform in syntax violation"
                                    maybe-subform))))
        (form-source (syntax->source form))
        (subform-source (syntax->source subform)))
    
    (raise (make-loki-error (or form-source subform-source)
        (call-with-string-output-port (lambda (out)
            ; TODO use *trace*
            (if who (display who out))
            (if who (display " - " out))
            (display message out)
            (display "\n" out)
            (display form out)))))))

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
      (rt:import-libraries-for-run (environment-imported-libraries env) (map not imported-libraries) 0)
      (let ((result (rt:runtime-run-program (expand-toplevel-sequence (list exp)))))
        (if (null? result) result (car result))))))

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

(define (map-while f lst k)
  (cond ((null? lst) (k '() '()))
        ((pair? lst)
         (let ((head (f (car lst))))
           (if head
               (map-while f
                          (cdr lst)
                          (lambda (answer rest)
                            (k (cons head answer)
                               rest)))
               (k '() lst))))
        (else  (k '() lst))))

(define (flatten l)
  (cond ((null? l) l)
        ((pair? l) (cons (car l)
                         (flatten (cdr l))))
        (else (list l))))

(define (sexp-map f s)
  (cond ((null? s) '())
        ((pair? s)
          (cons (sexp-map f (car s))
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
    (cond ((< length-left n) (error "dotted-butlast: List too short" ls n))
          ((= length-left n) '())
          (else
           (cons (car ls)
                 (recurse (cdr ls)
                          (- length-left 1)))))))

(define (dotted-last ls n)
  (let recurse ((ls ls)
                (length-left (dotted-length ls)))
    (cond ((< length-left n) (error "dotted-last: List too short" ls n))
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

(define (join e separator)
  (define (tostring x)
    (cond ((symbol? x)
           (symbol->string x))
          ((number? x)
           (number->string x))
          (else
           (error "join: Invalid argument." e))))
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
  (lambda (module-handler thunk)
    (fluid-let ((*trace*            '())
                (*current-library*  '())
                (*phase*            0)
                (*used*             (list '()))
                (*color*            (generate-color))
                (*usage-env*        *toplevel-env*)
                (*syntax-reflected* #f)
                (*module-handler*   module-handler))
      (thunk))))

(define (expand-toplevel-sequence forms)
  (scan-sequence 'toplevel
                 make-toplevel-mapping
                 (source->syntax toplevel-template forms)
                 (lambda (forms syntax-definitions bound-variables)
                   (emit-body forms emit-always-global))))

(define (library-name-part->string p)
  (if (symbol? p) (symbol->string p)
                  (number->string p)))

(define (library-name->path library-name)
  (let ((name (map library-name-part->string library-name)))
    (let ((options (map (lambda (dir)
                 (let ((absolute (path-join (current-working-path) dir)))
                   (path-with-suffix (apply path-join absolute name) "sld")))
               rt:library-dirs)))
      (or (find (lambda (path) (file-exists? (path->string path))) options)
          (syntax-violation #f (string-append "File not found for library: " (write-to-string name)) name)))))

(define (load-library name)
  (or
    (rt:lookup-library/false name)
    (begin (loki-load (library-name->path name))
           (rt:lookup-library name))))

(define (loki-load file)
  (expand-file (wrap-path file)
    (lambda (library invoke?)
      (rt:import-library (rt:library-name library)))))

;; This may be used as a front end for the compiler.
;; It expands a file consisting of a possibly empty sequence
;; of libraries optionally followed by a <toplevel program>.
;; The result is a sequence of vanilla r5rs-like toplevel
;; definitions and expressions.

(define (expand-file path module-handler)
  (with-toplevel-parameters module-handler
    (lambda ()
      (let ((content (read-file path #f)))
        (expand-toplevel-sequence (normalize content))))))

(define (expand-datum-sequence forms module-handler)
  (with-toplevel-parameters module-handler
   (lambda ()
        (expand-toplevel-sequence (normalize (datum->syntax eval-template forms))))))

;; Keeps (<library> ...) the same.
;; Converts (<library> ... . <toplevel program>)
;; to (<library> ... (program . <toplevel program>))

(define (normalize exps)
  (define (error)
    (let ((newline (string #\newline)))
      (syntax-violation
       'normalize
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
                             (,(datum->syntax toplevel-template 'begin) ,@(reverse toplevel)))
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

(define (read-file-from-reader reader)
  (let f ((x (read-annotated reader)))
    (if (and (annotation? x) (eof-object? (annotation-expression x)))
      '()
      (cons x (f (read-annotated reader))))))

(define (read-file fn fold-case?)
  (let* ((path (wrap-path fn))
         (str (path->string path))
         (p (open-input-file str))
         (reader (make-reader p str)))
    (reader-fold-case?-set! reader fold-case?)
    (let ((content (read-file-from-reader reader)))
      content)))

;;==========================================================================
;;
;; Intrinsic bootstrap:
;;
;;==========================================================================


(define intrinsic-template
  (make-identifier 'intrinsic-template
                   '()
                   '()
                   0
                   `(anonymous)
                   (make-source "<intrinsic>" 1 0)))

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
  `(program define-library export import cond-expand
    include-library-declarations
    include include-ci begin for run expand meta only
            except prefix rename primitives))

(define (make-library-language)
  (map (lambda (name)
         (cons name (make-binding 'macro name '(0) #f '())))
       library-language-names))

;;===================================================================
;;
;; Language features, for cond-expand.
;;
;;===================================================================

(define static-features '(
  loki
  wasm
  r7rs
  posix))
(define (loki-features) (list-copy static-features))
(define (feature? feature)
  (and (symbol? feature) (member feature static-features)))

;;===================================================================
;;
;; Bootstrap library containing macros defined in this expander.
;;
;;===================================================================

(rt:register-library!
 (let ((primitive-macro-mapping
        `((lambda        . ,expand-lambda)
          (if            . ,expand-if)
          (set!          . ,expand-set!)
          (begin         . ,expand-begin)
          (let           . ,expand-let)
          (syntax        . ,expand-syntax)
          (quote         . ,expand-quote)
          (let-syntax    . ,expand-local-syntax)
          (letrec-syntax . ,expand-local-syntax)
          (syntax-case   . ,expand-syntax-case)
          (and           . ,expand-and)
          (or            . ,expand-or)
          (include       . ,expand-include)
          (include-ci    . ,expand-include-ci)
          (define        . ,invalid-form)
          (define-syntax . ,invalid-form)
          (_             . ,invalid-form)
          (...           . ,invalid-form))))
   (rt:make-library
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

;;===================================================================
;;
;; Bootstrap library containing compiler intrinsics.
;;
;;===================================================================

(rt:register-library!
  (rt:make-library
   '(core intrinsics)
   ;; envs
   '()
   ;; exports
   (map (lambda (intrinsic)
          (cons intrinsic (make-binding 'variable intrinsic '(0) #f '())))
        compiler-intrinsics)
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


;; Initial environments:

(set! *toplevel-env* (make-unit-env))
(set! *usage-env*    *toplevel-env*)

;; Import only the minimal library language into the toplevel:

(env-import! toplevel-template (make-library-language) *toplevel-env*)
(register-macro! 'define-library (make-expander invalid-form))
(register-macro! 'program (make-expander invalid-form))
(register-macro! 'import  (make-expander invalid-form))

;; Register the expander's primitive API surface with the runtime
(rt:runtime-add-primitive 'ex:identifier? identifier?)
(rt:runtime-add-primitive 'ex:bound-identifier=? bound-identifier=?)
(rt:runtime-add-primitive 'ex:free-identifier=? free-identifier=?)
(rt:runtime-add-primitive 'ex:generate-temporaries generate-temporaries)
(rt:runtime-add-primitive 'ex:datum->syntax datum->syntax)
(rt:runtime-add-primitive 'ex:syntax->datum syntax->datum)
(rt:runtime-add-primitive 'ex:syntax->source syntax->source)
(rt:runtime-add-primitive 'ex:source-file source-file)
(rt:runtime-add-primitive 'ex:source-line source-line)
(rt:runtime-add-primitive 'ex:source-column source-column)
(rt:runtime-add-primitive 'ex:environment environment)
(rt:runtime-add-primitive 'ex:environment-bindings environment-bindings)
(rt:runtime-add-primitive 'ex:eval loki-eval)
(rt:runtime-add-primitive 'ex:load loki-load)
(rt:runtime-add-primitive 'ex:syntax-violation syntax-violation)
(rt:runtime-add-primitive 'ex:features loki-features)

(rt:runtime-add-primitive 'ex:invalid-form invalid-form)
(rt:runtime-add-primitive 'ex:register-macro! register-macro!)
(rt:runtime-add-primitive 'ex:syntax-rename syntax-rename)
(rt:runtime-add-primitive 'ex:map-while map-while)
(rt:runtime-add-primitive 'ex:dotted-length dotted-length)
(rt:runtime-add-primitive 'ex:dotted-butlast dotted-butlast)
(rt:runtime-add-primitive 'ex:dotted-last dotted-last)
(rt:runtime-add-primitive 'ex:free=? free=?)

))
