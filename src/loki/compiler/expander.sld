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
 (define-library (loki compiler expander)
   (import (scheme base))
   (import (scheme write))
   (import (scheme cxr))
   (import (scheme file))
   (import (scheme process-context))
   (import (scheme case-lambda))
   (import (srfi 1))
   (import (srfi 128))
   (import (srfi 146 hash))
   (import (loki util))
   (import (loki path))
   (import (loki fs))
   (import (loki compiler lang core))
   
   (import (loki core reader))
   (import (loki core syntax))
   
   (import (loki compiler runtime))
   (import (loki compiler intrinsics))
   (import (loki compiler binding))
   (import (loki compiler environment))
   (import (loki compiler macro))
   (import (loki compiler match))
   (import (loki compiler util))
   (import (loki compiler features))
   (import (loki compiler gensym))
   
   (export expand-module primitive-macro-mapping make-expander-environment)
   (begin
    
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
    ;; current module name as list of symbols
    (define *current-module*  #f)
    (define *load-module*  (lambda x (error "no *load-module* set, can't load dependencies during expand")))
    (define *load-macro*  (lambda x (error "no *load-macro* set, can't evalute macros")))
    (define *import-for-expand* 
      (lambda x (error "no *import-for-expand* set, can't evalute macros")))
    ;; car of this records bindings already referenced in current body
    ;; for detecting when later definitions may violate lexical scope
    (define *used*             (list '()))
    ;; history trace for error reporting
    (define *trace*            '())
    ;; whether expanded module introduces identifiers via syntax
    ;; expressions - if not, save lots of space by not including
    ;; env-table in object code
    (define *syntax-reflected* #f)
    ;; the current expression sequence counter
    ;; bindings store the value of this counter
    ;; when created, and it is compared to another
    ;; value when checking for use-before-define
    (define *sequence-counter* 0)
    ;; current color for painting lambdas, so that
    ;; we can identify valid forward references inside
    ;; lambdas vs invalid forward references
    (define *lambda-color* #f)
    ;; current id generator for context
    (define *gen-id* (lambda (id) (error "no *gen-id* set" id)))
    ;; current binding metadata map
    ;; we need to track some information for use throughout
    ;; the expansion phase for bindings
    (define *binding-metadata* #f)
    
    (define (id-module id)
      (or (id-maybe-module id)
          *current-module*))
    
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
    
    ;;=========================================================================
    ;;
    ;; Colors to paint things with:
    ;;
    ;;=========================================================================
    
    (define (generate-color)
      (generate-guid 'c))
    (define (generate-lambda-color)
      (generate-guid 'l))
    
    
    ;;=========================================================================
    ;;
    ;; Error handling
    ;;
    ;;=========================================================================
    
    
    
    ;;=========================================================================
    ;;
    ;; Mapping in environment: ((<name> <color> ...) . <binding>)
    ;;
    ;;=========================================================================
    
    ;; Generates a local mapping at the current meta-level
    ;; that can be added to the usage environment.
    
    (define (make-local-mapping type id attrs)
      (cons (cons (id-name id)
                  (id-colors id))
            (make-binding-meta type
                               (generate-guid (id-name id))
                               (list (source-level id))
                               attrs
                               *current-module*)))
    
    ;; Toplevel binding forms use as binding name the free name
    ;; so that source-level forward references will work in REPL.
    ;; If identifier is macro-generated, bind it with a fresh name.
    ;; This ensures that generated toplevel defines are not visible
    ;; from toplevel source code, thus approximating the behaviour
    ;; of generated internal definitions.
    
    (define (make-toplevel-name symbol)
      (string->symbol (string-append "~" (symbol->string symbol))))
    
    (define (make-toplevel-mapping type id attrs)
      (if (null? (id-colors id))
          (cons (cons (id-name id)
                      (id-colors id))
                (make-binding-meta type
                                   (make-toplevel-name (id-name id))
                                   '(0)
                                   attrs
                                   #f))
        (make-local-mapping type id attrs)))
    
    (define (binding-metadata-set! binding metadata)
      (set! *binding-metadata* (hashmap-set *binding-metadata* binding metadata)))
    (define (binding-metadata binding)
      (unless (hashmap-contains? *binding-metadata* binding)
        (binding-metadata-set! binding (empty-binding-metadata)))
      (hashmap-ref *binding-metadata* binding))
    (define (binding-attr binding name)
      (hashmap-ref (binding-metadata binding) name (lambda () #f)))
    (define (binding-attr! binding name)
      (hashmap-ref (binding-metadata binding) name (lambda () (error "binding doesn't have required attr" binding name))))
    (define (binding-attr-set! binding name value)
      (binding-metadata-set! binding (hashmap-set (binding-metadata binding) name value)))
    
    (define (make-binding-meta type name levels metadata module)
      (let ((b (make-binding type name levels module)))
        (binding-metadata-set! b metadata)
        b))
    
    (define (binding-mutable? binding) (binding-attr binding 'mutable?))
    (define (binding-mutable-set! binding mutable?) (binding-attr-set! binding 'mutable? mutable?))
    (define (binding-dimension binding) (binding-attr binding 'dimension))
    (define (binding-sequence-counter binding) (binding-attr binding 'sequence-counter))
    (define (binding-lambda-color binding) (binding-attr binding 'lambda-color))
    
    (define (attrs-from-context) (default-attrs *sequence-counter* *lambda-color*))
    (define (binding id) (binding-lookup id *usage-env*))
    
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
               'binder
               (string-append "Attempt to use binding of " (symbol->string (id-name id))
                              " in module (" (join (id-module id) " ")
                              ") at invalid level " (number->string (source-level id))
                              ".  Binding is only available at levels: "
                              (join (binding-levels binding) " "))
               id))
        (or (and (null? (id-module id))
                 (= *phase* 0))
            (syntax-violation
             'binder
             (string-append "No binding available for " (symbol->string (id-name id))
                            " in library (" (join (id-module id) " ") ")")
             
             id))))
    
    
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
        (core::apply-prim ex:syntax-rename
                          (core::constant (id-name id))
                          (core::constant (id-colors id))
                          (core::constant (cons (env-reflect *usage-env*)
                                                (id-transformer-envs id)))
                          (core::constant (- (- *phase* (id-displacement id)) 1))
                          (core::constant (id-module id))
                          (core::constant (source-file source))
                          (core::constant (source-line source))
                          (core::constant (source-column source)))))
    
    (define (syntax-rename name colors transformer-envs transformer-phase source-module file line column)
      (make-identifier name
                       (cons *color* colors)
                       transformer-envs
                       (- *phase* transformer-phase)
                       source-module
                       (make-source file line column)))
    
    ; FIXME
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
                                          (id-maybe-module tid)
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
                                          (make-binding-meta type symbol '(0) (attrs-from-context) #f)))
                              (make-null-env)))
                       *phase*
                       #f
                       (make-source "<unknown>" 1 0)))
    
    ;; Calls a macro with a new color.
    (define (invoke-macro macro t)
      (set! *color* (generate-color))
      ((macro-proc macro) t))
    
    (define (expand-macro t)
      (let* ((imports (datum->syntax toplevel-template '((import (loki core primitives)))))
             (expanded (expand t))
             (module (core::module `(macro ,(generate-guid 'm))
                                   #f
                                   '()
                                   '()
                                   '()
                                   '()
                                   '()
                                   (list expanded)
                                   (generate-guid 'build))))
        module))
    
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
                                     (let ((macro (binding-name->macro (binding-name binding) t)))
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
                                      (syntax-violation 'expand "Pattern variable used outside syntax template" t)))))
                         ((null? t)       (core::constant '()))
                         ((list? t)       (core::apply (expand (car t)) (map expand (cdr t))))
                         ((identifier? t)
                          (if *current-module*
                              (syntax-violation 'expand "Unbound identifier" t)
                            (core::ref (make-toplevel-name (id-name t)))))
                         ((pair? t)       (syntax-violation 'expand "Invalid procedure call syntax" t))
                         ((symbol? t)     (syntax-violation 'expand "Symbol may not appear in syntax object" t))
                         (else            (core::constant (syntax->datum t)))))))
    
    ;; Only expands while t is a user macro invocation.
    ;; Used by expand-lambda to detect internal definitions.
    
    (define (head-expand t)
      (fluid-let ((*trace* (cons t *trace*)))
                 (let ((binding (operator-binding t)))
                   (cond (binding (case (binding-type binding)
                                    ((macro)
                                     (let ((macro (binding-name->macro (binding-name binding) t)))
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
               (check-reference operator binding)
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
        ((quote datum) (core::constant (syntax->datum datum)))))
    
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
           (case (and binding (binding-type binding))
             ((macro)
              (let ((macro (binding-name->macro (binding-name binding) id)))
                (case (macro-type macro)
                  ((variable-transformer)
                   (expand (invoke-macro macro exp)))
                  (else
                   (syntax-violation
                    'set! "Keyword being set! is not a variable transformer" exp)))))
             ((variable)
              (binding-mutable-set! binding #t)
              (core::set! (binding->core::ref binding) (expand e)))
             ((pattern-variable)
              (syntax-violation 'set! "Pattern variable used outside syntax template" exp))
             (else
              (core::set! (core::ref (make-toplevel-name (id-name id))) (expand e))))))))
    
    ;; Expression begin.
    
    (define (expand-begin exp)
      (match exp
        ((- exps ___)
         (scan-sequence 'lambda
                        make-local-mapping
                        exps
                        (lambda (forms no-syntax-definitions no-bound-variables)
                          (core::letrec '() (emit-body forms emit-never-toplevel)))))))
    
    (define (check-let-binding-names names)
      (for-all (lambda (name)
                 (unless (identifier? name) (syntax-violation 'let "Invalid let binding name" name)))
               names))
    
    (define (expand-let exp)
      (match exp
        ((- ((names values) ___) body ___)
         (check-let-binding-names names)
         (let ((val-forms (map expand values)))
           (fluid-let ((*usage-env*
                        (env-extend (map (lambda (name) (make-local-mapping 'variable name (attrs-from-context))) names)
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
                                                    (core::letrec
                                                     (map (lambda (name binding val) (core::let-var (binding->core::ref binding) val))
                                                          names bindings val-forms)
                                                     (emit-body body-forms emit-never-toplevel)))))))))))
    
    ;; Expression let(rec)-syntax:
    
    (define (expand-local-syntax exp)
      (expand-let `(,(rename 'macro 'let) () ,@(cdr exp))))
    
    ;; Define and and or as primitives so we can import them into the
    ;; toplevel without spoiling the and and or of the library language.
    
    (define (expand-and exp)
      (match exp
        ((and) (core::constant #t))
        ((and e) (expand e))
        ((and e es ___)
         (core::if (expand e)
                   (expand `(,and ,@es))
                   (core::constant #f)))))
    
    (define (expand-or exp)
      (match exp
        ((or) (core::constant #t))
        ((or e) (expand e))
        ((or e es ___)
         (let ((x (generate-guid 'x)))
           (core::letrec
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

   (define (read-relative-include id fn fold-case?)
     (read-file (resolve-include-path id fn) fold-case?))

    (define (expand-include-file exp fold-case?)
      (match exp
        ((include) (syntax-violation (syntax->datum include) "Invalid include syntax" exp))
        ((include file)
         (check-valid-include (syntax->datum include) file)
         (let ((content (read-relative-include include (syntax->datum file) fold-case?)))
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
                                         (make-local-mapping 'variable formal (attrs-from-context)))
                                       (flatten formals))
                                  *usage-env*)))
                    (let-values (((named-formals rest-formal) (scan-formals formals)))
                      (let ((named-bindings (map (lambda (formal) (binding formal)) named-formals))
                            (rest-binding (if rest-formal (binding rest-formal) #f)))
                        ;; Scan-sequence expects the caller to have prepared
                        ;; the frame to which to destructively add bindings.
                        ;; Lambda bodies need a fresh frame.
                        (fluid-let ((*usage-env* (env-extend '() *usage-env*))
                                    (*sequence-counter* 0)
                                    (*lambda-color* (generate-lambda-color)))
                                   (scan-sequence 'lambda
                                                  make-local-mapping
                                                  body
                                                  (lambda (forms syntax-definitions bound-variables)
                                                    (core::lambda (map (lambda (formal binding) (binding->core::ref binding))
                                                                       named-formals
                                                                       named-bindings)
                                                                  (if rest-binding (binding->core::ref rest-binding) #f)
                                                                  (emit-body forms emit-never-toplevel)))))))))))
    
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
    
    ;; R6RS splicing of internal let-syntax and letrec-syntax
    ;; requires that we remember the bindings visible in each
    ;; form for later expansion of deferred right hand sides
    ;; and expressions.  This is done by attaching
    ;; the environment to the expression.
    ;; We call the resulting data structure a wrap.
    ;; Wraps are only used internally in processing of bodies,
    ;; and are never seen by user macros.
    (define-record-type <wrap>
      (make-wrap-record env exp id sequence-counter)
      wrap?
      (env wrap-env)
      (exp wrap-exp)
      (id wrap-id)
      (sequence-counter wrap-sequence-counter))
    (define (make-wrap env exp id) (make-wrap-record env exp id *sequence-counter*))
    
    (define (scan-sequence body-type make-map body-forms k)
      
      ;; Each <form> ::= (<binding | #f> #t <wrap>)   (deferred rhs)
      ;;              |  (<binding | #f> #f <s-expr>) (undeferred rhs)
      ;; Returns ((<binding | #f> . <s-expr>) ...)
      
      (define (expand-deferred forms)
        (map (lambda (form)
               (cons (car form)
                     (let ((deferred? (cadr form))
                           (exp       (caddr form)))
                       (if deferred?
                           (fluid-let ((*usage-env* (wrap-env exp))
                                       (*sequence-counter* (wrap-sequence-counter exp)))
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
            (k (expand-deferred (reverse forms))
               (reverse syntax-defs)
               bound-variables))
           (else
            (fluid-let ((*usage-env* (wrap-env (car ws)))
                        (*sequence-counter* (+ *sequence-counter* 1))
                        (*gen-id*           (make-gensym)))
                       (call-with-values
                        (lambda () (head-expand (wrap-exp (car ws))))
                        (lambda (form operator-binding)
                          (let ((type (and operator-binding (binding-name operator-binding))))
                            (check-expression-sequence body-type type form)
                            (check-toplevel            body-type type form)
                            (case type
                              ((primitive-let)
                               (loop (cdr ws)
                                     (cons (expand-let form) forms)
                                     syntax-defs
                                     bound-variables))
                              ((define)
                               (call-with-values
                                (lambda () (parse-definition form #f))
                                (lambda (id rhs)
                                  (check-valid-definition id common-env body-type form forms type)
                                  (env-extend! (list (make-map 'variable id (attrs-from-context))) common-env)
                                  (loop (cdr ws)
                                        (cons (list (binding id)
                                                    #t
                                                    (make-wrap *usage-env* rhs id))
                                              forms)
                                        syntax-defs
                                        (cons (cons id (binding id)) bound-variables)))))
                              ((define-syntax)
                               (call-with-values
                                (lambda () (parse-definition form #t))
                                (lambda (id rhs)
                                  (check-valid-definition id common-env body-type form forms type)
                                  (let ((mapping (make-map 'macro id (attrs-from-context))))
                                    (env-extend! (list mapping) common-env)
                                    (let ((rhs-expanded (fluid-let ((*phase* (+ 1 *phase*)))
                                                                   (expand-macro rhs))))
                                      (register-macro! (binding-name (cdr mapping)) (make-transformer (*load-macro* rhs-expanded)))
                                      (loop (cdr ws)
                                            forms
                                            ;FIXME
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
                                                              (make-local-mapping 'macro formal (attrs-from-context)))
                                                            formals))
                                         (extended-env (env-extend usage-diff original-env))
                                         (rhs-expanded
                                          (fluid-let ((*phase* (+ 1 *phase*))
                                                      (*usage-env*
                                                       (case type
                                                         ((let-syntax)    original-env)
                                                         ((letrec-syntax) extended-env))))
                                                     (map expand-macro rhs)))
                                         (macros (map *load-macro* rhs-expanded)))
                                    (for-each (lambda (mapping macro rhs)
                                                (register-macro! (binding-name (cdr mapping)) (make-transformer macro)))
                                              usage-diff
                                              macros
                                              rhs-expanded)
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
    
    (define (emit-never-toplevel g) #f)
    (define (emit-always-toplevel g) #t)
    (define (bound-variables->emit-toplevel-if-bound? bound-variables)
      (lambda (g) (any (lambda (n) (equal? (binding-name g) (binding-name (cdr n))))
                       bound-variables)))
    
    (define (emit-body body-forms is-toplevel?)
      (let loop ((body-forms body-forms)
                 (output '())
                 (vars '()))
        (if (null? body-forms)
            (if (null? vars)
                (reverse output)
              (list (core::letrec (reverse vars)
                                  (reverse output))))
          (let* ((body-form (car body-forms))
                 (binding-or-false (car body-form))
                 (ref (if (binding? binding-or-false) (binding->core::ref binding-or-false) #f))
                 (val (cdr body-form)))
            (if ref
                (if (is-toplevel? binding-or-false)
                    ; if global, directly emit core::define-global!
                  (loop (cdr body-forms)
                        (cons (core::define-global! ref val) output)
                        vars)
                  (if (core::atomic? val)
                      ;; if atomic, hoist up into the letrec, don't need
                    ;; any fancy data flow graphs to figure this one out
                    (loop (cdr body-forms) output (cons (core::let-var ref val) vars))
                    ;; a regular local (define name value)
                    ;; since core:: doesn't have local define, we hoist the definition
                    ;; to the top of the body, with value %void
                    (loop (cdr body-forms)
                          (cons (core::set! ref val) output)
                          (cons (core::let-var ref (core::anon-ref %void)) vars))))
              ; not a define, skip
              (loop (cdr body-forms) (cons val output) vars))))))
    
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
           (memq type '(import program define-library define define-syntax))
           (syntax-violation type "Invalid form in expression sequence" form)))
    
    (define (check-toplevel body-type type form)
      (and (not (eq? body-type 'toplevel))
           (memq type '(import program define-library))
           (syntax-violation type "Expression may only occur at toplevel" form)))
    
    (define (check-valid-definition id common-env body-type form forms type)
      (check-used id body-type form))
    
    (define (check-expression-body body-type forms body-forms)
      (and (eq? body-type 'lambda)
           (or (null? forms)
               (symbol? (caar forms)))
           (syntax-violation body-type "Body must be nonempty and end with an expression" body-forms)))
    
    (define (check-reference id binding)
      (and (binding? binding)
           (eq? (binding-type binding) 'variable)
           (eq? (binding-lambda-color binding)
                *lambda-color*)
           (>= (binding-sequence-counter binding)
               *sequence-counter*)
           (syntax-violation 'use-before-define "Use before define" id)))
    
    ;;=========================================================================
    ;;
    ;; Syntax-case:
    ;;
    ;;=========================================================================
    
    (define (expand-syntax-case exp)
      (define (literal? x) (identifier? x))
      (match exp
        ((- e ((? literal? literals) ___) clauses ___)
         (let ((input (core::ref (generate-guid 'input))))
           (core::letrec (list (core::let-var input (expand e)))
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
              (core::letrec (list (core::let-var temp input))
                            (list (process-match temp pattern sk fk))))
          (match pattern
            ((% free=? ...)       (syntax-violation 'syntax-case "Invalid use of ellipses" pattern))
            (()                 (core::if (core::apply-prim %null? input) sk fk))
            ((? literal? id)
             (core::if (core::if (core::apply-prim ex:identifier? input)
                                 (core::if (core::apply-prim ex:free-identifier=? input (syntax-reflect id))
                                           (core::constant #t)
                                           (core::constant #f))
                                 (core::constant #f))
                       sk
                       fk))
            ((% free=? _) sk)
            ((? identifier? id)
             (core::letrec (list (core::let-var (binding->core::ref (binding id)) input))
                           (list sk)))
            ((p (% free=? ...))
             (let* ((pvars (map car (pattern-vars p 0)))
                    (bindings (map binding pvars))
                    (refs (map binding->core::ref bindings)))
               (if (and (identifier? p)                               ; +++
                        (= (length refs) 1))                          ; +++
                 (let ((ref (car refs)))
                   (core::if (core::apply-prim list? input)
                             (core::letrec (list (core::let-var ref input))
                                           (list sk))
                             fk))
                 (let ((columns (core::ref (generate-guid 'cols)))
                       (rest    (core::ref (generate-guid 'rest))))
                   (core::apply-prim ex:map-while
                                     (core::lambda (list input)
                                                   #f
                                                   (list (process-match input
                                                                        p
                                                                        (core::apply (core::anon-ref list) refs)
                                                                        (core::constant #f))))
                                     input
                                     (core::lambda (list columns rest)
                                                   #f
                                                   (list (core::if (core::apply-prim %null? rest)
                                                                   (core::apply-prim %apply
                                                                                     (core::lambda refs #f (list sk))
                                                                                     (core::if (core::apply-prim %null? columns)
                                                                                               (core::constant (map (lambda (i) '()) pvars))
                                                                                               (core::apply-prim %apply
                                                                                                                 (core::anon-ref map)
                                                                                                                 (core::anon-ref list)
                                                                                                                 columns)))
                                                                   fk))))))))
            ((p (% free=? ...) . tail)
             (let ((tail-length (core::constant (dotted-length tail))))
               (core::if (core::apply-prim %gte (core::apply-prim ex:dotted-length input) tail-length)
                         (process-match (core::apply-prim ex:dotted-butlast input tail-length)
                                        `(,p ,(cadr pattern))
                                        (process-match (core::apply-prim ex:dotted-last input tail-length)
                                                       tail
                                                       sk
                                                       fk)
                                        fk)
                         fk)))
            ((p1 . p2)
             (core::if (core::apply-prim %pair? input)
                       (process-match (core::apply-prim %car input)
                                      p1
                                      (process-match (core::apply-prim %cdr input) p2 sk fk)
                                      fk)
                       fk))
            (#(ps ___)
             (core::if (core::apply-prim %vector? input)
                       (process-match (core::apply-prim vector->list input)
                                      ps
                                      sk
                                      fk)
                       fk))
            ((? symbol? -)
             (syntax-violation 'syntax-case "Symbol object may not appear in pattern" pattern))
            (other
             #t
             (core::if (core::apply-prim %equal?
                                         (core::apply-prim ex:syntax->datum input)
                                         (core::constant (syntax->datum other)))
                       sk
                       fk)))))
      
      (define (pattern-vars pattern level)
        (match pattern
          ((p (% free=? ...) . tail) (append (pattern-vars p (+ level 1))
                                             (pattern-vars tail level)))
          ((p1 . p2)               (append (pattern-vars p1 level)
                                           (pattern-vars p2 level)))
          (#(ps ___)               (pattern-vars ps level))
          ((% free=? ...)            '())
          ((% free=? _)            '())
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
                           (syntax-violation 'syntax-case "Repeated pattern variable" dup)))
             (let ((mappings (map (lambda (pvar)
                                    (make-local-mapping 'pattern-variable (car pvar) (dimension-attrs (cdr pvar))))
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
         (core::apply (core::anon-ref ex:syntax-violation)
                      (list (core::constant 'syntax-case) (core::constant "Invalid syntax-case match") input)))
        ((clause clauses ___)
         (let ((fail (core::ref (generate-guid 'fail))))
           (core::letrec (list (core::let-var fail (core::lambda '() #f (list (process-clauses clauses input literals)))))
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
        ((% free=? ...)
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
        (((% free=? ...) p)
         (process-template p dim #t))
        ((? (lambda (_) (not ellipses-quoted?))
            (t (% free=? ...) . tail))
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
                                                    (map (lambda (ref) (core::apply-prim length ref)) refs))
                                       (core::apply (core::anon-ref map)
                                                    (cons (core::lambda refs #f (list x))
                                                          refs))
                                       (core::apply (core::anon-ref ex:syntax-violation)
                                                    (list (core::constant 'syntax)
                                                          (core::constant
                                                           "Pattern variables denoting lists of unequal length preceding ellipses")
                                                          (core::apply (core::anon-ref list)
                                                                       refs)))))))
                    (gen (if (> (segment-depth template) 1)
                             (core::apply-prim %apply (core::anon-ref append) gen)
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
        (other (expand other))))
    
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
        ((t (% free=? ...) . rest)
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
        ((p (% free=? ...) . rest)
         (+ 1 (segment-depth (cdr pattern))))
        (- 0)))
    
    ;; All but the last ellipses
    
    (define (segment-head pattern)
      (let ((head
             (let recur ((pattern pattern))
               (match pattern
                 ((h (% free=? ...) (% free=? ...) . rest)
                  (cons h (recur (cdr pattern))))
                 ((h (% free=? ...) . rest)
                  (list h))))))
        (match head
          ((h (% free=? ...) . rest)
           head)
          (- (car head)))))
    
    ;; Get whatever is after the `...'s in PATTERN.
    
    (define (segment-tail pattern)
      (let loop ((pattern (cdr pattern)))
        (match pattern
          (((% free=? ...) . tail)
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
                  id)))))
    
    ;;==========================================================================
    ;;
    ;; Libraries:
    ;;
    ;;==========================================================================
    
    ; force-exports? - force all defines (and define-syntaxes) at the
    ;                  module's toplevel to be exported, useful for
    ;                  generating an anonymous module for an eval'd define
    ; TODO - prevent duplicate exports
    ;        duplicate exports of the same name
    ;        conflicting exports?
    (define (expand-module t with-loader additional-imports force-exports?)
      (with-toplevel-parameters with-loader
       (lambda ()
         (let ((t (normalize-forms additional-imports t)))
           (match t
             ((keyword name declarations ___)
              (let ((name (syntax->datum (scan-library-name name)))
                    (module-type (syntax->datum (car t))))
                (unless (memq module-type '(program define-library))
                  (syntax-violation 'module "invalid module type" (car t)))
                (call-with-values
                 (lambda () (scan-declarations declarations))
                 (lambda (imported-libraries imports exports body-forms)
                   (fluid-let ((*usage-env*        (make-unit-env))
                               (*current-module*  name)
                               (*syntax-reflected* #f))       ; +++ space
                              
                              (*import-for-expand* imported-libraries 0)
                              ; TODO - tree shake import names that aren't used, dumb to store
                              ;        all of that
                              (env-import! keyword imports *usage-env*)
                              
                              (with-reified-env-table
                               (lambda (reify-env-table)
                                 (scan-sequence
                                  module-type
                                  make-local-mapping
                                  body-forms
                                  (lambda (forms syntax-definitions bound-variables)
                                    (let* ((exports
                                            (if force-exports?
                                                (append exports
                                                        (map (lambda (bvar) `(,(car bvar) ,(car bvar) 0))
                                                             bound-variables))
                                              exports))
                                           (exports
                                            (map (lambda (mapping)
                                                   (cons (id-name (car mapping))
                                                         (let ((binding (binding (cadr mapping))))
                                                           (or binding
                                                               (syntax-violation
                                                                'module "Unbound export" (cadr mapping)))
                                                           binding)))
                                                 exports))
                                           (module (core::module
                                                    name
                                                    (if *syntax-reflected*
                                                        (reify-env-table)
                                                      #f)
                                                    exports
                                                    imports
                                                    imported-libraries
                                                    (current-builds imported-libraries)
                                                    syntax-definitions
                                                    (emit-body forms
                                                               (bound-variables->emit-toplevel-if-bound? bound-variables))
                                                    (generate-guid 'build))))
                                      module)))))))))))))))

    (define (current-builds imported-libraries)
     (map (lambda (lib-entry)
            (core::module-build (*load-module* (car lib-entry))))
          imported-libraries))

    ; imports = ((<name> <module-ref> <binding>) ...)
    (define (env-import! keyword imports env)
      (env-extend! (map (lambda (import)
                          (cons (cons (car import)
                                      (id-colors keyword))
                                (caddr import)))
                        imports)
                   env))
    
    (define (scan-declarations declarations)
      (let loop ((declarations declarations)
                 (imported-libraries '())
                 (imports '())
                 (exports '())
                 (body-forms '()))
        (define (scan-cond-expand-feature-clause feature-clause)
          (match feature-clause
            ((% free=? else) #t)
            (((% free=? and)) #t)
            (((% free=? and) req1 req2 ___)
             (and (scan-cond-expand-feature-clause req1)
                  (scan-cond-expand-feature-clause req2)))
            (((% free=? or)) #f)
            (((% free=? or) req1 req2 ___)
             (or (scan-cond-expand-feature-clause req1)
                 (scan-cond-expand-feature-clause req2)))
            (((% free=? not) req) (not (scan-cond-expand-feature-clause req)))
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
              (let ((content (read-relative-include op str #f)))
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
                          (lambda (dup) (syntax-violation 'export "Duplicate export" dup)))
              (values imported-libraries imports exports body-forms))
          (match (car declarations)
            (((% free=? import) specs ___)
             (call-with-values
              (lambda () (scan-imports specs imported-libraries imports))
              (lambda (imported-libraries imports)
                (loop (cdr declarations) imported-libraries imports exports body-forms))))
            (((% free=? export) sets ___)
             (loop (cdr declarations)
                   imported-libraries
                   imports
                   (append exports (scan-exports sets))
                   body-forms))
            (((% free=? cond-expand) clauses ___)
             (scan-cond-expand-clauses clauses))
            (((% free=? include) includes ___)
             (loop (cdr declarations)
                   imported-libraries
                   imports
                   exports
                   (append body-forms (apply append (scan-includes (caar declarations) includes #f)))))
            (((% free=? include-ci) includes ___)
             (loop (cdr declarations)
                   imported-libraries
                   imports
                   exports
                   (append body-forms (apply append (scan-includes (caar declarations) includes #t)))))
            (((% free=? include-library-declarations) includes ___)
             (scan-include-library-declarations (caar declarations) includes))
            (((% free=? begin) forms ___)
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
          (let ((content (read-relative-include op str fold-case?)))
            (cons (source->syntax op content) (scan-includes op (cdr includes) fold-case?))))))
    
    (define (scan-exports sets)
      (apply append (map scan-export-set sets)))
    
    (define (scan-export-set set)
      (match set
        ((? identifier? x)
         `((,x ,x 0)))
        (((% free=? rename) (? identifier? x) (? identifier? y))
         `((,y ,x 0)))
        (- (syntax-violation 'export "Invalid export set" set))))
    
    ;; Returns
    ;;    (values ((<module reference> <level> ...) ....)
    ;;            ((<local name> <module reference> <binding>) ...))
    ;; with no repeats.
    
    (define (scan-imports specs imported-libraries imports)
      (if (null? specs)
          (values imported-libraries (unify-imports imports))
        (call-with-values
         (lambda () (scan-import-spec (car specs)))
         (lambda (module-ref levels more-imports)
           (scan-imports (cdr specs)
                         ;; module-ref = #f if primitives spec
                         (if module-ref
                             (cons (cons module-ref levels)
                                   imported-libraries)
                           imported-libraries)
                         (append more-imports imports))))))
    
    ;; Returns (values <module reference> | #f
    ;;                 (<level> ...)
    ;;                 ((<local name> <module reference> <binding>) ...)
    ;; where <level> ::= <integer>
    ;; #f is returned for module name in case of primitives.
    
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
                                               (string-append "Identifier "
                                                              (symbol->string name)
                                                              " not in set: "
                                                              (join (map car mappings) " "))
                                               name
                                               )))
                       names))
           
           (match import-set
             (((% free=? primitives) (? identifier? xs) ___)
              (values #f
                      levels
                      (map (lambda (mapping)
                             (list (car mapping)
                                   #f
                                   (make-binding-meta 'variable (cdr mapping) levels (attrs-from-context) *current-module*)))
                           (adjuster (map (lambda (name) (cons name name))
                                          (syntax->datum xs))))))
             (((% free=? only) set (? identifier? xs) ___)
              (let ((args (syntax->datum xs)))
                (loop set
                      (compose adjuster (lambda (mappings)
                                          (check-presence args mappings 'only)
                                          (filter (lambda (mapping)
                                                    (memq (car mapping) args))
                                                  mappings))))))
             (((% free=? except) set (? identifier? xs) ___)
              (let ((args (syntax->datum xs)))
                (loop set
                      (compose adjuster (lambda (mappings)
                                          (check-presence args mappings 'except)
                                          (filter (lambda (mapping)
                                                    (not (memq (car mapping) args)))
                                                  mappings))))))
             (((% free=? prefix) set (? identifier? pre))
              (loop set
                    (compose adjuster (lambda (mappings)
                                        (map (lambda (mapping)
                                               (cons (string->symbol
                                                      (string-append
                                                       (symbol->string (syntax->datum pre))
                                                       (symbol->string (car mapping))))
                                                     (cdr mapping)))
                                             mappings)))))
             (((% free=? rename) set ((? identifier? xs) (? identifier? ys)) ___)
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
             (((% free=? primitives) . -) (invalid-form import-set))
             (((% free=? only)       . -) (invalid-form import-set))
             (((% free=? except)     . -) (invalid-form import-set))
             (((% free=? prefix)     . -) (invalid-form import-set))
             (((% free=? rename)     . -) (invalid-form import-set))
             (-
              (let ((module-ref (module-ref import-set)))
                (if module-ref
                    (let* ((module (*load-module* (syntax->datum module-ref)))
                           (exports (core::module-exports module))
                           (imports
                            (map (lambda (mapping)
                                   (list (car mapping)
                                         (syntax->datum module-ref)
                                         (let ((binding (cdr (assq (cdr mapping) exports))))
                                           (make-binding (binding-type binding)
                                                         (binding-name binding)
                                                         (compose-levels levels (binding-levels binding))
                                                         (binding-module binding)))))
                                 (adjuster (map (lambda (name) (cons name name))
                                                (map car exports))))))
                      (values (syntax->datum module-ref)
                              levels
                              imports))
                  (syntax-violation 'import "Invalid import set" import-set)))))))))
    
    (define (scan-levels spec)
      (match spec
        (((% free=? for) set levels ___)
         (let ((levels
                (map (lambda (level)
                       (match level
                         ((% free=? run)                   0)
                         ((% free=? expand)                1)
                         (((% free=? meta) (? integer-syntax? n)) (annotation-expression n))
                         (- (syntax-violation 'for "Invalid level in for spec" level))))
                     levels)))
           (check-set? levels = (lambda (dup) (syntax-violation 'for "Repeated level in for spec" dup)))
           (values levels set)))
        (- (values '(0) spec))))
    
    (define (compose-levels levels levels*)
      (apply unionv
             (map (lambda (level)
                    (map (lambda (level*)
                           (+ level level*))
                         levels*))
                  levels)))
    
    ;; Argument is of the form ((<local name> <module-ref> <binding>) ...)
    ;; where combinations (<local name> <module-ref> (binding-name <binding>)) may be repeated.
    ;; Return value is of the same format but with no repeats and
    ;; where union of (binding-levels <binding>)s has been taken for any original repeats.
    ;; An error is signaled if same <local name> occurs with <binding>s
    ;; whose (binding-name <binding>)s are different.
    
    (define (unify-imports imports)
      (let ((seen '()))
        (let loop ((imports imports))
          (if (null? imports)
              seen
            (let* ((import (car imports))
                   (local-name (car import))
                   (module-ref (cadr import))
                   (binding (caddr import))
                   (probe (assq local-name seen)))
              (if probe
                  (begin
                   (or (eq? (binding-name binding)
                            (binding-name (caddr probe)))
                       (syntax-violation
                        'import
                        (string-append "Different bindings for identifier imported from libraries ("
                                       (join (binding-module binding) " ")
                                       ") and ("
                                       (join (binding-module (caddr probe)) " ") ")")
                        local-name))
                   ; (<local-name> . (<module-ref> . (<binding> . ())))
                   ;  car
                   ;  set-car!
                   ;                  cdar
                   ;                  (set-car! (cdr list))
                   ;                                  cddar
                   ;                                  (set-car! (cddr list))
                   (set-car! (cddr probe)
                             (make-binding (binding-type (caddr probe))
                                           (binding-name (caddr probe))
                                           (unionv (binding-levels (caddr probe))
                                                   (binding-levels binding))
                                           (binding-module (caddr probe)))))
                (set! seen (cons import seen)))
              (loop (cdr imports)))))))
    
    (define (module-name-part? p)
      (or (identifier? p) (integer-syntax? p)))
    
    (define (scan-library-name e) (module-ref-helper e))
    
    (define (module-ref e)
      (module-ref-helper
       (match e
         (((% free=? define-library) name) name)
         (((% free=? define-library) . -)  (invalid-form e))
         (- e))))
    
    (define (module-ref-helper e)
      (match e
        (((? module-name-part? ids) ___) ids)
        (- (error "Invalid module reference." e))))
    
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
                       '()
                       (make-source "<eval>" 1 0)))
    
    (define (make-expander-environment with-loader import-specs)
      (with-toplevel-parameters with-loader (lambda ()
        (fluid-let ((*usage-env* (make-unit-env)))
          (env-import! eval-template (make-module-language) *usage-env*)
          (call-with-values
            (lambda ()
              (fluid-let ((*phase* 0))
                (scan-imports
                  (map (lambda (spec)
                               (datum->syntax eval-template spec))
                       import-specs) '() '())))
            (lambda (imported-libraries imports)
              (make-environment imports)))))))
    
        
    ;; Puts parameters to a consistent state for the toplevel
    ;; Old state is restored afterwards so that things will be
    ;; reentrant.
    (define with-toplevel-parameters
      (lambda (with-loader thunk)
        (with-loader (lambda (load-module load-macro import-for-expand)
                     (fluid-let ((*trace*              '())
                                 (*current-module*    #f)
                                 (*load-module*       load-module)
                                 (*load-macro*        load-macro)
                                 (*import-for-expand* import-for-expand)
                                 (*phase*              0)
                                 (*used*               (list '()))
                                 (*color*              (generate-color))
                                 (*usage-env*          *toplevel-env*)
                                 (*syntax-reflected*   #f)
                                 (*sequence-counter*   0)
                                 (*lambda-color*       (generate-lambda-color))
                                 (*binding-metadata*     (empty-binding-metadata)))
                                (thunk))))))

    (define (generate-anonymous-module imports body)
      `(,(datum->syntax toplevel-template 'program) (,(datum->syntax toplevel-template (generate-guid 'program)))
        ,@imports
        (,(datum->syntax toplevel-template 'begin) ,@body)))
    
    (define (normalize-forms initial-imports exps)
      (let ((exps (source->syntax toplevel-template exps)))
        (match exps
          ((((% free=? define-library) . rest))
           (car exps))
          ((((% free=? define-library) . rest) . rest2)
           (syntax-violation 'define-library "Invalid expressions outside of define-library" rest))
          (else
           (let loop ((imports '())
                      (exps exps))
             (if (null? exps)
                 (generate-anonymous-module (append initial-imports (reverse imports)) exps)
               (match (car exps)
                 (((% free=? import) . _)
                  (loop (cons (car exps) imports) (cdr exps)))
                 (else (generate-anonymous-module (append initial-imports (reverse imports)) exps)))))))))
    
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
                       '()
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
                       '()
                       (make-source "<toplevel>" 1 0)))
    
    (define (source->syntax tid datum)
      (add-context-to-identifiers tid datum))
    
    ;;===================================================================
    ;;
    ;; Language for bootstrapping the REPL session and (environment ---):
    ;;
    ;;===================================================================
    
    (define module-language-names
      `(program define-library export import cond-expand
        include-library-declarations
        include include-ci begin for run expand meta only
        except prefix rename primitives))
    
    (define (make-module-language)
      (map (lambda (name)
             (list name '(loki core module-primitives) (make-binding 'macro name '(0) '(loki core module-primitives))))
           module-language-names))

    (define primitive-macro-mapping
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
              (_             . ,(lambda (x) (syntax-violation 'primitive "Invalid use of primitive macro" x)))
              (...           . ,invalid-form)))

    
    ;; Initial environments:
    
    (set! *toplevel-env* (make-unit-env))
    (set! *usage-env*    *toplevel-env*)
    
    ;; Import only the minimal module language into the toplevel:
    
    (env-import! toplevel-template (make-module-language) *toplevel-env*)
    (register-macro! 'define-library (make-expander invalid-form))
    (register-macro! 'program (make-expander invalid-form))
    (register-macro! 'import  (make-expander invalid-form))
    
    ;; Register the expander's primitive API surface with the runtime
    (runtime-add-primitive 'ex:identifier? identifier?)
    (runtime-add-primitive 'ex:bound-identifier=? bound-identifier=?)
    (runtime-add-primitive 'ex:free-identifier=? free-identifier=?)
    (runtime-add-primitive 'ex:generate-temporaries generate-temporaries)
    (runtime-add-primitive 'ex:datum->syntax datum->syntax)
    (runtime-add-primitive 'ex:syntax->datum syntax->datum)
    (runtime-add-primitive 'ex:syntax->source syntax->source)
    (runtime-add-primitive 'ex:source-file source-file)
    (runtime-add-primitive 'ex:source-line source-line)
    (runtime-add-primitive 'ex:source-column source-column)
    (runtime-add-primitive 'ex:syntax-violation syntax-violation)
    (runtime-add-primitive 'ex:features loki-features)
    
    (runtime-add-primitive 'ex:invalid-form invalid-form)
    (runtime-add-primitive 'ex:register-macro! register-macro!)
    (runtime-add-primitive 'ex:syntax-rename syntax-rename)
    (runtime-add-primitive 'ex:map-while map-while)
    (runtime-add-primitive 'ex:dotted-length dotted-length)
    (runtime-add-primitive 'ex:dotted-butlast dotted-butlast)
    (runtime-add-primitive 'ex:dotted-last dotted-last)
    (runtime-add-primitive 'ex:free=? free=?)
    
    ))
