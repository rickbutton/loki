(define-library 
(p08_funcs2wat)
(import (scheme base))
(import (srfi 151))
(import (chibi match))
(import (util))
(import (shared))
(export p08_funcs2wat)
(begin

(define (func->type f) (cadr f))
(define (func->name f) (caddr f))
(define (func->bounds f) (cadddr f))
(define (func->frees f) (cadddr (cdr f)))
(define (func->locals f) (cadddr (cddr f)))
(define (func->body f) (cdddr (cdddr f)))
(define (funcs->mappings funcs) (map func->name funcs))

(define (char->utf8-byte-length c)
    (let ((int (char->integer c)))
        (cond
            ((<= int #x7f) 1)
            ((<= int #x7ff) 2)
            ((<= int #xffff) 3)
            ((<= int #x10ffff) 4)
            (else "unknown utf8 character"))))
(define (string->utf8-byte-length s)
    (apply + (map char->utf8-byte-length (string->list s))))

(define (rodata-string->length d) (string->utf8-byte-length d))
(define (rodata-string->wasm-data d) d)

(define (rodata-symbol->length d) 
    (string->utf8-byte-length (symbol->string d)))
(define (rodata-symbol->wasm-data d) (symbol->string d))

(define (rodata->length d)
    (cond 
        ((string? d) (rodata-string->length d))
        ((symbol? d) (rodata-symbol->length d))
        (else (error "unknown rodata"))))

(define (rodata->wasm-data d)
    (cond 
        ((string? d) (rodata-string->wasm-data d))
        ((symbol? d) (rodata-symbol->wasm-data d))
        (else (error "unknown rodata"))))

(define (rodatas->lengths rodatas) (map rodata->length rodatas))
(define (rodatas->wasm-datas rodatas) (map rodata->wasm-data rodatas))
(define (rodatas->wasm-data-section rodatas)
    (let ((section (apply string-append 
            (rodatas->wasm-datas rodatas))))
        (if (= (string-length section) 0) "" section)))
(define (rodatas->offsets rodatas)
    (let ((lengths (rodatas->lengths rodatas))
          (total 0))
        (map (lambda (l) 
            (let ((t total))
                (set! total (+ total l))
                t)) lengths)))

(define (compile-program rodatas-and-funcs)
(let* ((rodatas (car rodatas-and-funcs))
       (rodata-offsets (rodatas->offsets rodatas))
       (funcs (cdr rodatas-and-funcs))
       (mappings (funcs->mappings funcs)))

    (define (inline-comment comment) (make-comment comment))
    (define (inline-val v)
        (make-comment (string-append
            "val="
            ((cond
                ((number? v) number->string)
                ((boolean? v) (lambda (_) (if v "#t" "#f")))
                ((char? v) string)
                ((string? v) (lambda (s) s))
                ((symbol? v) symbol->string)
                ((null? v) (lambda (_) "()"))
                (else (raise "invalid value for inline comment")))
                v))))

    (define sfixnum? integer?)
    (define (compile-sfixnum x) `(call $$prim$make-number (i32.const ,x)))
    (define sboolean? boolean?)
    (define (compile-sboolean x) `(get_global ,(if x '$$iv$true '$$iv$false)))
    (define schar? char?)
    (define (compile-schar x) `(call $$prim$make-char (i32.const ,(char->integer x))))
    (define snull? null?)
    (define (compile-pair x) `(call $$prim$cons))
    (define (compile-snull) `(get_global $$iv$null))
    (define (compile-void) `(get_global $$iv$void))

    (define (compile-quote expr func)
        (if (and (pair? expr) (not (equal? (car expr) 'rodata)))
            `((call $$prim$cons
                ,@(compile-quote (car expr) func)
                ,@(compile-quote (cdr expr) func)))
            (compile-expr expr func)))

    (define (intrinsic->wasm-op i) `((call ,(intrinsic->name i))))
    (define (compile-intrinsic intrinsic args k func)
        `(,@(compile-func-body args func)
          ,@(intrinsic->wasm-op intrinsic)
          ,@(compile-expr k func)
          ,@(compile-apply 1)))

    (define (compile-if expr func)
        (let ((condition (cadr expr))
              (consequent (caddr expr))
              (alternate (cadddr expr)))
            `(,@(compile-expr condition func)
                (call $$prim$test)
                (if (result anyref)
                    (then ,@(compile-expr 
                        consequent func))
                    (else ,@(compile-expr 
                        alternate func))))))
            
    (define (compile-ref-rodata index rodata)
        `((table.get $$rodatatable (i32.const ,index))))

    (define (compile-make-rodata index rodata)
        (cond
            ((string? rodata)
                `(call $$prim$make-string
                    ,(inline-val rodata)
                    (i32.const ,(list-ref rodata-offsets index))
                    (i32.const ,(rodata-string->length rodata))))
            ((symbol? rodata)
                `(call $$prim$make-symbol
                    ,(inline-val rodata)
                    (i32.const ,(list-ref rodata-offsets index))
                    (i32.const ,(rodata-symbol->length rodata))))
            (else (raise "unknown rodata type"))))
    (define (compile-set-rodata index rodata)
        `((table.set $$rodatatable 
            (i32.const ,index)
            ,(compile-make-rodata index rodata))))

    (define (set!->variable s) (cadr s))
    (define (set!->expr s) (caddr s))
    (define (compile-set! expr func)
        `(,@(compile-expr (set!->expr expr) func)
          (call $$prim$set-slot ,(mapping->slot-ref (set!->variable expr) func))))
    ; end store

    ; scope / vars
    (define (mapping->slot-ref var func) 
        (let ((frees (func->frees func)))
            (if (member var frees)
                `(call $$prim$get-free (get_local $$close) 
                                  (i32.const ,(index var frees)))
                `(get_local ,(variable->value var)))))

    (define (compile-variable-ref var func) 
        (let ((frees (func->frees func))
              (bounds (func->bounds func)))
            (if (or (not (member var bounds)) (member var frees))
                `((call $$prim$get-slot ,(mapping->slot-ref var func)))
                `(,(mapping->slot-ref var func)))))

    (define (compile-makeclosure r func)
        (let* ((idx (index (cadr r) mappings))
               (body (func->body func))
               (bounds (caddr r))
               (frees (cadddr r)))
            `((call $$prim$make-closure 
                    ,(inline-comment (string-append "func=" (symbol->string (cadr r))))
                    (i32.const ,idx) 
                    (i32.const ,(length bounds))
                    (i32.const ,(length frees)))
               ,@(apply append (map 
                    (lambda (f i) `(
                        (i32.const ,i)
                        ,(mapping->slot-ref f func)
                        (call $$prim$set-free)))
                    frees (range 0 (length frees) 1))))))
            
    (define (number->funcsig-name n) 
            (string->symbol (string-append "$$fun$" (number->string n))))
    (define (compile-apply argc) 
        `((tee_local $$tmp)
          (get_local $$tmp)
          (call $$prim$get-closure-findex (i32.const ,argc))
          (return_call_indirect 
            (type ,(number->funcsig-name argc)) $$functable)))
    ; end scope / vars

    (define (compile-expr expr func)
        (match expr
            (('begin exprs ...)
                (compile-func-body exprs func))
            (('quote expr) (compile-quote expr func))
            (('makeclosure args ...) (compile-makeclosure expr func))
            (('set! args ...) (compile-set! expr func))
            (('set-rodata index rodata) (compile-set-rodata index rodata))
            (('rodata index rodata) (compile-ref-rodata index rodata))
            (('if args ...) (compile-if expr func))
            (('intrinsic intrinsic args ... k) (compile-intrinsic intrinsic args k func))
            (('void) (compile-void))
            ((? variable?) (compile-variable-ref expr func))
            ((f es ...)
                (let ((compiled-args (compile-func-body es func)))
                    `(,@compiled-args
                      ,@(compile-expr f func)
                      ,@(compile-apply (length es)))))
            ((? sfixnum?) (list (compile-sfixnum expr)))
            ((? sboolean?) (list (compile-sboolean expr)))
            ((? schar?) (list (compile-schar expr)))
            ((? snull?) (list (compile-snull)))
            (else (list `(failed ,expr)))))

    (define (compile-func-body body func)
        (apply append (map (lambda (e) 
            (compile-expr e 
                func)) body)))

    ; func

    (define (op-match op) (lambda (v) (and (list? v) (eq? (car v) op))))

    (define (set!? x) (and (list? x) (equal? (car x) 'set!)))

    

    (define (func->wparams f)
        (let ((bounds (func->bounds f)))
            (map (lambda (p) `(param ,(variable->value p) anyref)) bounds)))

    (define (func->wlocals f)
        (let ((locals (func->locals f)))
            (map (lambda (l) `(local ,(variable->value l) anyref)) locals)))

    (define (func->prelude f)
        (let ((locals (func->locals f)))
            (define (local->init l)
                `(set_local ,(variable->value l) (call $$prim$make-slot)))
            (map local->init locals)))

    (define (compile-func func) 
        (let* ((body   (func->body func)) 
               (compiled-body (compile-func-body body func))
               (locals (func->locals func))
               (prelude (func->prelude func))
               (type (func->type func)))
            (cond
                ((eq? type 'close)
                    `((func 
                        ,(func->name func) 
                        ,@(func->wparams func) 
                        (param $$close anyref) 
                        (result anyref) 
                        ,@(func->wlocals func) 
                        (local $$tmp anyref)
                        ,@prelude
                        ,@compiled-body)))
                ((eq? type 'open)
                    `((func 
                        ,(func->name func) 
                        ,@(func->wparams func) 
                        (result anyref) 
                        ,@(func->wlocals func) 
                        (local $$tmp anyref)
                        ,@prelude
                        ,@compiled-body)))
                ((eq? type 'start)
                    `((func 
                        ,(func->name func) 
                        ,@(func->wparams func) 
                        ,@(func->wlocals func) 
                        (local $$tmp anyref)
                        ,@prelude
                        ,@compiled-body)
                      (start ,(func->name func))))
                (else (raise "unknown function type")))))
                    

    (define (compile-funcs funcs) 
        (apply append
            (map (lambda (func) (compile-func func)) funcs)))

    (define (funcs->functable funcs) 
        `(table $$functable ,(length funcs) anyfunc))
    (define (funcs->funcelems funcs) 
        `(elem (i32.const 0) ,@(funcs->mappings funcs)))

    (let* ((cfuncs (compile-funcs funcs)))
        `(module
            (type $$fun$0 (func (param anyref) (result anyref)))
            (type $$fun$1 (func (param anyref) (param anyref) (result anyref)))
            (type $$fun$2 (func (param anyref) (param anyref) (param anyref) (result anyref)))
            (type $$fun$3 (func (param anyref) (param anyref) (param anyref) (param anyref) (result anyref)))

            (global $$iv$true (import "env" "$$iv$true") anyref)
            (global $$iv$false (import "env" "$$iv$false") anyref)
            (global $$iv$null (import "env" "$$iv$null") anyref)
            (global $$iv$void (import "env" "$$iv$void") anyref)

            (import "env" "memory" (memory 0))
            (import "env" "$$prim$make-number"        (func $$prim$make-number (param i32) (result anyref)))
            (import "env" "$$prim$add"                (func $$prim$add (param anyref anyref) (result anyref)))
            (import "env" "$$prim$sub"                (func $$prim$sub (param anyref anyref) (result anyref)))
            (import "env" "$$prim$le_s"               (func $$prim$le_s (param anyref anyref) (result anyref)))

            (import "env" "$$prim$test"               (func $$prim$test (param anyref) (result i32)))

            (import "env" "$$prim$make-char"          (func $$prim$make-char (param i32) (result anyref)))

            (import "env" "$$prim$make-slot"          (func $$prim$make-slot (result anyref)))
            (import "env" "$$prim$set-slot"           (func $$prim$set-slot (param anyref anyref)))
            (import "env" "$$prim$get-slot"           (func $$prim$get-slot (param anyref) (result anyref)))
            (import "env" "$$prim$cons"               (func $$prim$cons (param anyref anyref) (result anyref)))
            (import "env" "$$prim$car"                (func $$prim$car (param anyref) (result anyref)))
            (import "env" "$$prim$cdr"                (func $$prim$cdr (param anyref) (result anyref)))
            (import "env" "$$prim$concat-string"      (func $$prim$concat-string (param anyref anyref) (result anyref)))

            (import "env" "$$prim$make-closure"       (func $$prim$make-closure (param i32 i32 i32) (result anyref)))
            (import "env" "$$prim$set-free"           (func $$prim$set-free (param anyref i32 anyref) (result anyref)))
            (import "env" "$$prim$get-free"           (func $$prim$get-free (param anyref i32) (result anyref)))
            (import "env" "$$prim$get-closure-findex" (func $$prim$get-closure-findex (param anyref i32) (result i32)))
            (import "env" "$$prim$make-string"        (func $$prim$make-string (param i32 i32) (result anyref)))
            (import "env" "$$prim$make-symbol"        (func $$prim$make-symbol (param i32 i32) (result anyref)))
            (data (i32.const 0)
                ,(rodatas->wasm-data-section rodatas))


            ,(funcs->functable funcs)
            ,(funcs->funcelems funcs)
            (table $$rodatatable ,(length rodatas) anyref)

            (func $$main (result anyref) (call $$fentry))

            ,@cfuncs
            (export "main" (func $$main))
        ))))

(define (p08_funcs2wat rodatas-and-funcs) (compile-program rodatas-and-funcs))))
