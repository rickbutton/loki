; TODO - missing the "free"/"bound" from mark-vars
(define-library 
(p08_funcs2wat)
(import (scheme base))
(import (srfi 151))
(import (util))
(import (shared))
(export p08_funcs2wat)
(begin

; fixnum
(define wfixnum-shift 2)
(define wfixnum-mask  #b11)
(define wfixnum-tag  #b00)

(define (sfixnum? i) (integer? i))
(define (wfixnum? i) (eq? (bitwise-and i wfixnum-mask) wfixnum-tag))

(define (sfixnum->wfixnum i) (arithmetic-shift i wfixnum-shift))
(define (wfixnum->sfixnum i) (arithmetic-shift i (- 0 wfixnum-shift)))

(define (compile-sfixnum x)
    `(i32.const ,(sfixnum->wfixnum x)))
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
    `(i32.const ,(sboolean->wboolean x)))
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
    `(i32.const ,(schar->wchar x)))
; end char

; null
(define null-tag   #b00101111)

(define (snull? n) (null? n))
(define (wnull? n) (eq? n null-tag))

(define (snull->wnull n) null-tag)
(define (wnull->snull n) '())

(define (compile-snull x)
    `(i32.const ,(snull->wnull x)))
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
            (else (error (string-append "invalid intrinsic " op))))))
; end prim

; constant
(define (compile-constant c)
    (let ((x (car (cdr c))))
        (cond
            ((sfixnum?  x) (compile-sfixnum x))
            ((sboolean? x) (compile-sboolean x))
            ((schar? x) (compile-schar x))
            ((snull? x) (compile-snull x)))))
; end constant

; rodata

(define (compile-string s rodata-offsets)
    (let ((str (cadr s)) (idx (caddr s)))
    `(call $$alloc_string 
        (get_global $$rodata-id)
        (i32.const ,(list-ref rodata-offsets idx))
        (i32.const ,(string->utf8-byte-length str)))))

(define (rodata-string? d)
    (and (list? d) (eq? (car d) 'string)))

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

(define (rodata-string->length d) (string->utf8-byte-length (cadr d)))
(define (rodata-string->wasm-data d) (cadr d))
(define (rodata->length d)
    (cond 
        ((rodata-string? d) (rodata-string->length d))
        (else (error "unknown rodata"))))
(define (rodata->wasm-data d)
    (cond 
        ((rodata-string? d) (rodata-string->wasm-data d))
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
(define (compile-store s) `(set_local ,(store->mapping s)))
; end store

; scope / vars
(define (mapping->get-ref mapping func) 
    (let ((bounds (map variable->value (func->bounds func))) (frees (map variable->value (func->frees func))))
        (if (member mapping bounds)
            `(get_local ,mapping)
            (if (member mapping frees)
                `(call $$get_free (get_local $$close) (i32.const ,(index mapping frees)))
                `(call $$unslot (get_local ,mapping))))))

(define (refer->type s) (variable->binding (cadr s)))
(define (refer->mapping s) (variable->value (cadr s)))
(define (compile-refer s func) 
    (mapping->get-ref (refer->mapping s) func))

(define (compile-referfunc r func mappings)
    (let* ((idx (index (cadr r) mappings))
           (body (func->body func))
           (vars (caddr r)))
        `(inst (call $$alloc_close (i32.const ,idx) (i32.const ,(length vars)))
          ,@(apply append (map (lambda (f i) `(
             (i32.const ,i)
             ,(mapping->get-ref (variable->value f) func)
             (call $$store_free)
          )) vars (range 0 (length vars) 1))))))
        
(define (compile-slot s)
    `(call $$alloc_slot))

(define (number->funcsig-name n) (string->symbol (string-append "$$close" (number->string n))))
(define (number->call-close-name n) (string->symbol (string-append "$$call-close-" (number->string n))))
(define (compile-apply a) 
    `(call ,(number->call-close-name (cadr a))))
; end scope / vars

; inst
(define (compile-inst i func mappings rodata-offsets) 
    (let ((op (car i)))
        (cond
            ((eq? op 'constant) (compile-constant i))
            ((eq? op 'string) (compile-string i rodata-offsets))
            ((eq? op 'pair) (compile-pair i))
            ((eq? op 'store) (compile-store i))
            ((eq? op 'refer) (compile-refer i func))
            ((eq? op 'referfunc) (compile-referfunc i func mappings))
            ((eq? op 'slot) (compile-slot i))
            ((eq? op 'intrinsic) (compile-intrinsic i))
            ((eq? op 'apply) (compile-apply i))
            (else i))))
(define (compile-insts is func mappings rodata-offsets) 
    (fold-right (lambda (i rest) (if (eq? (car i) 'inst) 
        (append (cdr i) rest)
        (cons i rest)))
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
(define (func->frees f) (cadddr (cdr f)))
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

(define (compile-func func mappings rodata-offsets) 
    (let* ((body   (func->body func)) 
           (insts  (compile-insts body func mappings rodata-offsets))
           (locals (func->locals func)))
        (if (eq? (func->type func) 'close)
            `(func ,(func->name func) ,@(func->wparams func) (param $$close i32) (result i32) ,@(func->wlocals func) ,@insts)
            `(func ,(func->name func) ,@(func->wparams func) (result i32) ,@(func->wlocals func) ,@insts))))

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
            (type $$close0 (func (param i32) (result i32)))
            (type $$close1 (func (param i32) (param i32) (result i32)))
            (type $$close2 (func (param i32) (param i32) (param i32) (result i32)))
            (type $$close3 (func (param i32) (param i32) (param i32) (param i32) (result i32)))
            (global $$rodata-id (import "env" "$$rodata-id") i32)
            (global $$rodata-offset (import "env" "$$rodata-offset") i32)

            (import "env" "memory" (memory 0))
            (import "env" "$$alloc_slot"  (func $$alloc_slot (param i32) (result i32)))
            (import "env" "$$unslot"      (func $$unslot (param i32) (result i32)))
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

            (func $$call-close-0 (param $$close i32)
                (result i32)
                (local $$idx i32)

                (get_local $$close)

                (get_local $$close)
                (call $$get_close_func_index)

                (call_indirect (type $$close0))
            )

            (func $$call-close-1 (param $$close i32) (param $$0 i32)
                (result i32)
                (local $$idx i32)

                (get_local $$0)
                (get_local $$close)

                (get_local $$close)
                (call $$get_close_func_index)

                (call_indirect (type $$close1))
            )

            (func $$call-close-2 (param $$close i32) (param $$0 i32) (param $$1 i32)
                (result i32)
                (local $$idx i32)

                (get_local $$0)
                (get_local $$1)
                (get_local $$close)

                (get_local $$close)
                (call $$get_close_func_index)

                (call_indirect (type $$close2))
            )

            (func $$call-close-3 (param $$close i32) (param $$0 i32) (param $$1 i32) (param $$2 i32)
                (result i32)
                (local $$idx i32)

                (get_local $$0)
                (get_local $$1)
                (get_local $$2)
                (get_local $$close)

                (get_local $$close)
                (call $$get_close_func_index)

                (call_indirect (type $$close3))
            )

            ,@cfuncs
            (export "main" (func $$main))
        )))

(define (p08_funcs2wat funcs) (compile-program funcs))))
