; TODO - missing the "free"/"bound" from mark-vars
(define-library 
(p08_funcs2wat)
(import (scheme base))
(import (srfi 151))
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
(define (intrinsic->op x) (cadr x))

(define (compile-intrinsic i)
    (let ((op (intrinsic->op i)))
        (cond
            ((eq? op '%%prim%add) '(i32.add))
            ((eq? op '%%prim%sub) '(i32.sub))
            ((eq? op '%%prim%car) '(call $$car))
            ((eq? op '%%prim%cdr) '(call $$cdr))
            ((eq? op '%%prim%cons) '(call $$alloc_pair))

            ((eq? op '%%prim%le_s)
                `(inst
                    (i32.le_s)
                    (if (result i32)
                        (then ,(compile-sboolean #t))
                        (else ,(compile-sboolean #f)))))
            (else (error (string-append "invalid intrinsic " op))))))
; end prim

; test
(define (compile-test i func mappings rodata-offsets)
    (let ((consequent (cadr i))
          (alternate (caddr i)))
        `(inst
            ,(compile-sboolean #f)
            (i32.ne)
            (if (result i32)
                (then ,@(compile-insts consequent func 
                    mappings rodata-offsets))
                (else ,@(compile-insts alternate func 
                    mappings rodata-offsets))))))
        
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
(define (store->type s) (variable->binding (cadr s)))
(define (store->mapping s) (variable->value (cadr s)))
(define (compile-store s) `(call $$set_slot (get_local ,(store->mapping s))))
; end store

; scope / vars
(define (mapping->slot-ref mapping func) 
    (let ((frees (func->free-mappings func)))
        (if (member mapping frees)
            `(call $$get_free (get_local $$close) 
                              (i32.const ,(index mapping frees)))
            `(get_local ,mapping))))

(define (refer->type s) (variable->binding (cadr s)))
(define (refer->mapping s) (variable->value (cadr s)))
(define (compile-refer s func) 
    (let ((mapping (refer->mapping s))
          (frees (map variable->value (func->frees func)))
          (bounds (func->bound-mappings func)))
        (if (or (not (member mapping bounds)) (member mapping frees))
            `(call $$get_slot ,(mapping->slot-ref (refer->mapping s) func))
            (mapping->slot-ref (refer->mapping s) func))))

(define (compile-makeclosure r func mappings)
    (let* ((idx (index (cadr r) mappings))
           (body (func->body func))
           (vars (caddr r)))
        `(inst 
            (call $$alloc_close 
                  ,(inline-comment 
                        (string-append "func=" (symbol->string (cadr r))))
                  (i32.const ,idx) (i32.const ,(length vars)))
            ,@(apply append (map (lambda (f i) `(
               (i32.const ,i)
               ,(mapping->slot-ref (variable->value f) func)
               (call $$store_free)
            )) vars (range 0 (length vars) 1))))))
        
(define (number->funcsig-name n) 
        (string->symbol (string-append "$$fun$" (number->string n))))
(define (compile-apply a) 
    `(inst
        (tee_local $$tmp)
        (get_local $$tmp)
        (call $$get_close_func_index)
        (call_indirect (type ,(number->funcsig-name (cadr a))))))
; end scope / vars

; inst
(define (compile-inst i func mappings rodata-offsets) 
    (let ((op (car i)))
        (cond
            ((eq? op 'constant) (compile-constant i rodata-offsets))
            ((eq? op 'pair) (compile-pair i))
            ((eq? op 'store) (compile-store i))
            ((eq? op 'refer) (compile-refer i func))
            ((eq? op 'makeclosure) (compile-makeclosure i func mappings))
            ((eq? op 'test) (compile-test i func mappings rodata-offsets))
            ((eq? op 'intrinsic) (compile-intrinsic i))
            ((eq? op 'apply) (compile-apply i))
            (else i))))
(define (compile-insts is func mappings rodata-offsets) 
    (fold-right (lambda (i rest) 
        (cond
            ((eq? (car i) 'inst) (append (cdr i) rest))
            ((eq? (car i) 'end) rest)
            (else (cons i rest))))
        '() 
        (map (lambda (i) (compile-inst i func mappings rodata-offsets)) is)))
; end inst

; func

(define (op-match op) (lambda (v) (and (list? v) (eq? (car v) op))))
(define refer? (op-match 'refer))
(define store? (op-match 'store))

(define (func->type f) (cadr f))
(define (func->mapping f) (caddr f))
(define (func->bounds f) (cadddr f))
(define (func->bound-mappings f) (map variable->value (func->bounds f)))
(define (func->frees f) (cadddr (cdr f)))
(define (func->free-mappings f) (map variable->value (func->frees f)))
(define (func->body f) (cdddr (cdr (cdr f))))
(define (func->name f) (string->symbol (symbol->string (func->mapping f))))
(define (func->locals f)
    (let* ((bounds (func->bounds f))
           (body (func->body f))
           (refers-and-stores (filter (lambda (i) (or (refer? i) (store? i))) body))
           (all-mappings (map cadr refers-and-stores))
           (bounds-mappings (map variable->value (func->bounds f)))
           (non-bounds (filter (lambda (i) (not (member (variable->value i) bounds-mappings))) all-mappings)))
        (unique non-bounds)))

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
           (insts  (compile-insts body func mappings rodata-offsets))
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
                ,@insts)
            `(func ,(func->name func) 
                ,@(func->wparams func) 
                (result i32) 
                ,@(func->wlocals func) 
                (local $$tmp i32)
                ,@prelude
                ,@insts))))

(define (compile-funcs funcs rodata-offsets) 
    (let ((mappings (funcs->mappings funcs)))
        (map (lambda (func) (compile-func func mappings rodata-offsets)) funcs)))

(define (funcs->table funcs) `(table ,(length funcs) anyfunc))
(define (funcs->mappings funcs) (map func->mapping funcs))
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
