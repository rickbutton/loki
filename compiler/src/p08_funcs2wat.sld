; TODO - missing the "free"/"bound" from mark-vars
(define-library 
(p08_funcs2wat)
(import (scheme base))
(import (srfi 151))
(import (chibi match))
(import (util))
(import (shared))
(export p08_funcs2wat)
(begin

(define (inline-comment comment) (make-comment comment))
(define (inline-val v)
    (make-comment (string-append
        "val="
        ((cond
            ((number? v) number->string)
            ((boolean? v) (lambda (_) (if v "#t" "#f")))
            ((char? v) string)
            ((string? v) (lambda (s) s))
            ((null? v) (lambda (_) "()"))
            (else (raise "invalid value for inline comment")))
            v))))

(define sfixnum? integer?)
(define (compile-sfixnum x) `(call $$prim$make-number (i32.const ,x)))
(define sboolean? boolean?)
(define (compile-sboolean x) `(get_global ,(if x '$$iv$true '$iv$false)))
(define schar? char?)
(define (compile-schar x) `(call $$prim$make-char (i32.const ,(char->integer x))))
(define snull? null?)
(define (compile-snull x) `(get_global $$iv$null))
(define (compile-pair x) `(call $$prim$cons))

(define (intrinsic->wasm-op i) `((call ,(intrinsic->name i))))
(define (compile-intrinsic intrinsic args k func mappings rodata-offsets)
    `(,@(compile-func-body args func mappings rodata-offsets)
      ,@(intrinsic->wasm-op intrinsic)
      ,@(compile-func-body-expr k func mappings rodata-offsets)
      ,@(compile-apply 1)))

(define (compile-if expr func mappings rodata-offsets)
    (let ((condition (cadr expr))
          (consequent (caddr expr))
          (alternate (cadddr expr)))
        `(,@(compile-func-body-expr condition func mappings rodata-offsets)
            (call $$prim$test)
            (if (result anyref)
                (then ,@(compile-func-body-expr 
                    consequent func mappings rodata-offsets))
                (else ,@(compile-func-body-expr 
                    alternate func mappings rodata-offsets))))))
        
(define (compile-constant c rodata-offsets)
    (let ((x (car (cdr c))))
        (cond
            ((sfixnum?  x) (compile-sfixnum x))
            ((sboolean? x) (compile-sboolean x))
            ((schar? x) (compile-schar x))
            ((sstring? x) (compile-sstring c rodata-offsets))
            ((snull? x) (compile-snull x)))))

(define (sstring? x) (string? x))
; TODO
(define (compile-sstring s rodata-offsets)
    (let ((str (cadr s)) (idx (caddr s)))
    `(call $$prim$make-string
        ,(inline-val str)
        (get_global $$rodata-id)
        (i32.const ,(list-ref rodata-offsets idx))
        (i32.const ,(string->utf8-byte-length str)))))

(define (string->utf8-byte-length s)
    (apply + (map char->utf8-byte-length (string->list s))))
(define (char->utf8-byte-length c)
    (let ((int (char->integer c)))
        (cond
            ((<= int #x7f) 1)
            ((<= int #x7ff) 2)
            ((<= int #xffff) 3)
            ((<= int #x10ffff) 4)
            (else "unknown utf8 character"))))

(define (rodata-string->length d) (string->utf8-byte-length d))
(define (rodata-string->wasm-data d) d)
(define (rodata->length d)
    (cond 
        ((string? d) (rodata-string->length d))
        (else (error "unknown rodata"))))

(define (rodata->wasm-data d)
    (cond 
        ((string? d) (rodata-string->wasm-data d))
        (else (error "unknown rodata"))))

(define (rodatas->lengths rodatas) (map rodata->length rodatas))
(define (rodatas->wasm-datas rodatas) (map rodata->wasm-data rodatas))
(define (rodatas->wasm-data-section rodatas)
    (let ((section (string-join (rodatas->wasm-datas rodatas) "")))
        (if (= (string-length section) 0) "" section)))

(define (rodatas->offsets rodatas)
    (let ((lengths (rodatas->lengths rodatas))
          (total 0))
        (map (lambda (l) 
            (let ((t total))
                (set! total (+ total l))
                t)) lengths)))

(define (set!->variable s) (cadr s))
(define (set!->expr s) (caddr s))
(define (compile-set! expr func mappings rodata-offsets)
    `(,@(compile-func-body-expr (set!->expr expr) func mappings rodata-offsets)
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

(define (compile-makeclosure r func mappings)
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
      (return_call_indirect (type ,(number->funcsig-name argc)))))
; end scope / vars

(define (compile-func-body-expr expr func mappings rodata-offsets)
    (match expr
        (('begin exprs ...)
            (compile-func-body exprs func mappings rodata-offsets))
        (('makeclosure args ...) (compile-makeclosure expr func mappings))
        (('set! args ...) (compile-set! expr func mappings rodata-offsets))
        (('if args ...) (compile-if expr func mappings rodata-offsets))
        (('intrinsic intrinsic args ... k) (compile-intrinsic intrinsic args k func mappings rodata-offsets))
        ((? variable?) (compile-variable-ref expr func))
        ((f es ...)
            (debug f)
            (debug es)
            (let ((compiled-args (compile-func-body es func mappings rodata-offsets)))
                `(,@compiled-args
                  ,@(compile-func-body-expr f func mappings rodata-offsets)
                  ,@(compile-apply (length es)))))
        ((? sfixnum?) (list (compile-sfixnum expr)))
        (else (list `(failed ,expr)))))

(define (compile-func-body body func mappings rodata-offsets)
    (apply append (map (lambda (e) 
        (compile-func-body-expr e 
            func mappings rodata-offsets)) body)))

; func

(define (op-match op) (lambda (v) (and (list? v) (eq? (car v) op))))

(define (set!? x) (and (list? x) (equal? (car x) 'set!)))

(define (func->type f) (cadr f))
(define (func->name f) (caddr f))
(define (func->bounds f) (cadddr f))
(define (func->frees f) (cadddr (cdr f)))
(define (func->locals f) (cadddr (cddr f)))
(define (func->body f) (cdddr (cdddr f)))

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

(define (compile-func func mappings rodata-offsets) 
    (let* ((body   (func->body func)) 
           (compiled-body (compile-func-body body func mappings rodata-offsets))
           (locals (func->locals func))
           (prelude (func->prelude func)))
        (if (eq? (func->type func) 'close)
            `(func ,(func->name func) 
                ,@(func->wparams func) 
                (param $$close anyref) 
                (result anyref) 
                ,@(func->wlocals func) 
                (local $$tmp anyref)
                ,@prelude
                ,@compiled-body)
            `(func ,(func->name func) 
                ,@(func->wparams func) 
                (result anyref) 
                ,@(func->wlocals func) 
                (local $$tmp anyref)
                ,@prelude
                ,@compiled-body))))

(define (compile-funcs funcs rodata-offsets) 
    (let ((mappings (funcs->mappings funcs)))
        (map (lambda (func) (compile-func func mappings rodata-offsets)) funcs)))

(define (funcs->table funcs) `(table ,(length funcs) anyfunc))
(define (funcs->mappings funcs) (map func->name funcs))
(define (funcs->elems funcs) `(elem (i32.const 0) ,@(funcs->mappings funcs)))
; end func

(define (compile-program funcs-and-rodatas)
    (let* ((funcs (car funcs-and-rodatas))
           (rodatas (cdr funcs-and-rodatas))
           (rodata-offsets (rodatas->offsets rodatas))
           (cfuncs (compile-funcs funcs rodata-offsets)))
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

            (import "env" "$$prim$make-closure"       (func $$prim$make-closure (param i32 i32 i32) (result anyref)))
            (import "env" "$$prim$set-free"           (func $$prim$set-free (param anyref i32 anyref) (result anyref)))
            (import "env" "$$prim$get-free"           (func $$prim$get-free (param anyref i32) (result anyref)))
            (import "env" "$$prim$get-closure-findex" (func $$prim$get-closure-findex (param anyref i32) (result i32)))
            ;(import "env" "$$prim$make-string"        (func $$prim$make-string (param i32 i32 i32) (result i32)))
            ;(data (get_global $$rodata-offset) ,(rodatas->wasm-data-section rodatas))

            ,(funcs->table funcs)
            ,(funcs->elems funcs)

            (func $$main (result anyref) (call $$fentry))

            ,@cfuncs
            (export "main" (func $$main))
        )))

(define (p08_funcs2wat funcs) (compile-program funcs))))
