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

; fixnum
(define wfixnum-shift 2)
(define wfixnum-mask  #b11)
(define wfixnum-tag  #b00)

(define (sfixnum? i) (integer? i))
(define (wfixnum? i) (eq? (bitwise-and i wfixnum-mask) wfixnum-tag))

(define (sfixnum->wfixnum i) (arithmetic-shift i wfixnum-shift))
(define (wfixnum->sfixnum i) (arithmetic-shift i (- 0 wfixnum-shift)))

(define (compile-sfixnum x)
    `(i32.const 
        ,(inline-val x)
        ,(sfixnum->wfixnum x)))
; end fixnum

; boolean
(define wboolean-mask #b0011111)
(define wboolean-tag #b0011111)
(define false-tag #b00011111)
(define true-tag #b10011111)

(define (sboolean? b) (boolean? b))
(define (wboolean? b) (eq? (bitwise-and b wboolean-mask) wboolean-tag))

(define (sboolean->wboolean b) (if b true-tag false-tag))
(define (wboolean->sboolean b) (if (eq? b true-tag) #t #f))

(define (compile-sboolean x)
    `(i32.const 
        ,(inline-val x)
        ,(sboolean->wboolean x)))
; end boolean

; char
(define wchar-mask #b01111111)
(define wchar-tag  #b00001111)
(define wchar-shift 8)

(define (schar? c) (char? c))
(define (wchar? b) (eq? (bitwise-and b wchar-mask) wchar-tag))

(define (schar->wchar c) (bitwise-ior (arithmetic-shift (char->integer c) wchar-shift) wchar-tag))
(define (wchar->schar c) (arithmetic-shift (char->integer c) (- 0 wchar-shift)))

(define (compile-schar x)
    `(i32.const 
        ,(inline-val x)
        ,(schar->wchar x)))
; end char

; null
(define null-tag   #b00101111)

(define (snull? n) (null? n))
(define (wnull? n) (eq? n null-tag))

(define (snull->wnull n) null-tag)
(define (wnull->snull n) '())

(define (compile-snull x)
    `(i32.const 
        ,(inline-val x)
        ,(snull->wnull x)))
; end null

; pair
(define (compile-pair x)
    `(call $$alloc_pair))
; end pair

; intrinsic

(define (intrinsic->wasm-op i)
    (let ((op (intrinsic->name i)))
        (cond
            ((eq? op '%%prim%add) '((i32.add)))
            ((eq? op '%%prim%sub) '((i32.sub)))
            ((eq? op '%%prim%car) '((call $$car)))
            ((eq? op '%%prim%cdr) '((call $$cdr)))
            ((eq? op '%%prim%cons) '((call $$alloc_pair)))
            ((eq? op '%%prim%le_s)
                `((i32.le_s)
                  (if (result i32)
                    (then ,(compile-sboolean #t))
                    (else ,(compile-sboolean #f)))))
            (else (error (string-append "invalid intrinsic " op))))))

(define (compile-intrinsic intrinsic args k func mappings rodata-offsets)
    `(,@(compile-func-body args func mappings rodata-offsets)
      ,@(intrinsic->wasm-op intrinsic)
      ,@(compile-func-body-expr k func mappings rodata-offsets)
      ,@(compile-apply 1)))
; end prim

; test
(define (compile-if expr func mappings rodata-offsets)
    (let ((condition (cadr expr))
          (consequent (caddr expr))
          (alternate (cadddr expr)))
        `(,@(compile-func-body-expr condition func mappings rodata-offsets)
          ,(compile-sboolean #f)
          (i32.ne)
          (if (result i32)
            (then ,@(compile-func-body-expr 
                consequent func mappings rodata-offsets))
            (else ,@(compile-func-body-expr 
                alternate func mappings rodata-offsets))))))
        
; end test

; constant
(define (compile-constant c rodata-offsets)
    (let ((x (car (cdr c))))
        (cond
            ((sfixnum?  x) (compile-sfixnum x))
            ((sboolean? x) (compile-sboolean x))
            ((schar? x) (compile-schar x))
            ((sstring? x) (compile-sstring c rodata-offsets))
            ((snull? x) (compile-snull x)))))
; end constant

; rodata

(define (sstring? x) (string? x))

(define (compile-sstring s rodata-offsets)
    (let ((str (cadr s)) (idx (caddr s)))
    `(call $$alloc_string 
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
; end rodata

; store
(define (set!->variable s) (cadr s))
(define (set!->expr s) (caddr s))
(define (compile-set! expr func mappings rodata-offsets)
    `(,@(compile-func-body-expr (set!->expr expr) func mappings rodata-offsets)
      (call $$set_slot ,(mapping->slot-ref (set!->variable expr) func))))
; end store

; scope / vars
(define (mapping->slot-ref var func) 
    (let ((frees (func->frees func)))
        (if (member var frees)
            `(call $$get_free (get_local $$close) 
                              (i32.const ,(index var frees)))
            `(get_local ,(variable->value var)))))

(define (compile-variable-ref var func) 
    (let ((frees (func->frees func))
          (bounds (func->bounds func)))
        (if (or (not (member var bounds)) (member var frees))
            `((call $$get_slot ,(mapping->slot-ref var func)))
            `(,(mapping->slot-ref var func)))))

(define (compile-makeclosure r func mappings)
    (let* ((idx (index (cadr r) mappings))
           (body (func->body func))
           (frees (cadddr r)))
        `((call $$alloc_close 
                ,(inline-comment (string-append "func=" (symbol->string (cadr r))))
                (i32.const ,idx) 
                (i32.const ,(length frees)))
           ,@(apply append (map 
                (lambda (f i) `(
                    (i32.const ,i)
                    ,(mapping->slot-ref f func)
                    (call $$store_free)))
                frees (range 0 (length frees) 1))))))
        
(define (number->funcsig-name n) 
        (string->symbol (string-append "$$fun$" (number->string n))))
(define (compile-apply argc) 
    `((tee_local $$tmp)
      (get_local $$tmp)
      (call $$get_close_func_index)
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
; TODO - need to compute locals so that we can include them in the prelude of each func
;        this is slightly harder because the instructions are not flattened yet anymore
;        so either I need to traverse the tree to make the list of non bound/free set!/refers
;        or i need to compute locals while building the func, possibly in liftlambda, because
;        I am already walking the tree anyway, might as well add another list in the func for locals

(define (func->wparams f)
    (let ((bounds (func->bounds f)))
        (map (lambda (p) `(param ,(variable->value p) i32)) bounds)))

(define (func->wlocals f)
    (let ((locals (func->locals f)))
        (map (lambda (l) `(local ,(variable->value l) i32)) locals)))

(define (func->prelude f)
    (let ((locals (func->locals f)))
        (define (local->init l)
            `(set_local ,(variable->value l) (call $$alloc_slot)))
        (map local->init locals)))

(define (compile-func func mappings rodata-offsets) 
    (let* ((body   (func->body func)) 
           (compiled-body (compile-func-body body func mappings rodata-offsets))
           (locals (func->locals func))
           (prelude (func->prelude func)))
        (if (eq? (func->type func) 'close)
            `(func ,(func->name func) 
                ,@(func->wparams func) 
                (param $$close i32) 
                (result i32) 
                ,@(func->wlocals func) 
                (local $$tmp i32)
                ,@prelude
                ,@compiled-body)
            `(func ,(func->name func) 
                ,@(func->wparams func) 
                (result i32) 
                ,@(func->wlocals func) 
                (local $$tmp i32)
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
            (type $$fun$0 (func (param i32) (result i32)))
            (type $$fun$1 (func (param i32) (param i32) (result i32)))
            (type $$fun$2 (func (param i32) (param i32) (param i32) (result i32)))
            (type $$fun$3 (func (param i32) (param i32) (param i32) (param i32) (result i32)))
            (global $$rodata-id (import "env" "$$rodata-id") i32)
            (global $$rodata-offset (import "env" "$$rodata-offset") i32)

            (import "env" "memory" (memory 0))
            (import "env" "$$alloc_slot"  (func $$alloc_slot (result i32)))
            (import "env" "$$set_slot"  (func $$set_slot (param i32 i32)))
            (import "env" "$$get_slot"      (func $$get_slot (param i32) (result i32)))
            (import "env" "$$alloc_pair"  (func $$alloc_pair (param i32 i32) (result i32)))
            (import "env" "$$car"         (func $$car (param i32) (result i32)))
            (import "env" "$$cdr"         (func $$cdr (param i32) (result i32)))
            (import "env" "$$alloc_close" (func $$alloc_close (param i32 i32) (result i32)))
            (import "env" "$$store_free"  (func $$store_free (param i32 i32 i32) (result i32)))
            (import "env" "$$get_free"    (func $$get_free (param i32 i32) (result i32)))
            (import "env" "$$get_close_func_index"    (func $$get_close_func_index (param i32) (result i32)))
            (import "env" "$$alloc_string"    (func $$alloc_string (param i32 i32 i32) (result i32)))

            (data (get_global $$rodata-offset) ,(rodatas->wasm-data-section rodatas))

            ,(funcs->table funcs)
            ,(funcs->elems funcs)

            (func $$main (result i32) (call $$fentry))

            ,@cfuncs
            (export "main" (func $$main))
        )))

(define (p08_funcs2wat funcs) (compile-program funcs))))
