(define-library 
(p05_funcs2wat)
(import (scheme base))
(import (srfi 151))
(import (util))
(export p05_funcs2wat)
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

(define wobj-mask   #b111)
(define wobj-shift   3)
(define wpair-tag    #b01)
(define wclose-tag   #b11)

; pair
(define (compile-pair x)
    `(call $$alloc-pair))
; end pair

; prim
(define (prim->op x) (car (cdr x)))

(define (compile-primcall p)
    (let ((op (prim->op p)))
        (cond
            ((eq? op 'add) '(i32.add))
            ((eq? op 'sub) '(i32.sub))
            ((eq? op 'car) '(call $$car))
            ((eq? op 'cdr) '(call $$cdr))
            ((eq? op 'cons) '(call $$alloc-pair))
            (else (error (string-append "invalid primcall: " op))))))
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

; store
(define (store->type s) (caddr s))
(define (store->mapping s) (cadddr s))
(define (compile-store s) `(set_local ,(store->mapping s)))
; end store

; scope / vars
(define (mapping->get-ref mapping func) 
    (let ((bounds (map cdr (func->bounds func))) (frees (map cdr (func->frees func))))
        (if (member mapping bounds)
            `(get_local ,mapping)
            (if (member mapping frees)
                `(call $$get-free (get_local $$close) (i32.const ,(index mapping frees)))
                `(call $$unslot (get_local ,mapping))))))

(define (refer->type s) (caddr s))
(define (refer->mapping s) (cadddr s))
(define (compile-refer s func) 
    (mapping->get-ref (refer->mapping s) func))

(define (compile-referfunc r func mappings)
    (let* ((idx (index (cadr r) mappings))
           (body (func->body func))
           (vars (caddr r)))
        `(inst (call $$alloc-close (i32.const ,idx) (i32.const ,(length vars)))
          ,@(apply append (map (lambda (f i) `(
             (i32.const ,i)
             ,(mapping->get-ref (cdr f) func)
             (call $$store-free)
          )) vars (range 0 (length vars) 1))))))
        
(define (compile-slot s)
    `(call $$alloc-slot))

(define (number->funcsig-name n) (string->symbol (string-append "$$close" (number->string n))))
(define (number->call-close-name n) (string->symbol (string-append "$$call-close-" (number->string n))))
(define (compile-apply a) 
    `(call ,(number->call-close-name (cadr a))))
; end scope / vars

; inst
(define (compile-inst i func mappings) 
    (let ((op (car i)))
        (cond
            ((eq? op 'constant) (compile-constant i))
            ((eq? op 'pair) (compile-pair i))
            ((eq? op 'store) (compile-store i))
            ((eq? op 'refer) (compile-refer i func))
            ((eq? op 'referfunc) (compile-referfunc i func mappings))
            ((eq? op 'slot) (compile-slot i))
            ((eq? op 'primcall) (compile-primcall i))
            ((eq? op 'apply) (compile-apply i))
            (else i))))
(define (compile-insts is func mappings) 
    (fold-right (lambda (i rest) (if (eq? (car i) 'inst) 
        (append (cdr i) rest)
        (cons i rest)))
        '() 
        (map (lambda (i) (compile-inst i func mappings)) is)))
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
           (all-mappings (map cadddr refers-and-stores))
           (bounds-mappings (map cdr (func->bounds f)))
           (non-bounds (filter (lambda (i) (not (member i bounds-mappings))) all-mappings)))
        (unique non-bounds)))

(define (func->wparams f)
    (let ((bounds (func->bounds f)))
        (map (lambda (p) `(param ,(cdr p) i32)) bounds)))

(define (func->wlocals f)
    (let ((locals (func->locals f)))
        (map (lambda (l) `(local ,l i32)) locals)))

(define (compile-func func mappings) 
    (let* ((body   (func->body func)) 
           (insts  (compile-insts body func mappings))
           (locals (func->locals func)))
        (if (eq? (func->type func) 'close)
            `(func ,(func->name func) ,@(func->wparams func) (param $$close i32) (result i32) ,@(func->wlocals func) ,@insts)
            `(func ,(func->name func) ,@(func->wparams func) (result i32) ,@(func->wlocals func) ,@insts))))

(define (compile-funcs funcs) 
    (let ((mappings (funcs->mappings funcs)))
        (map (lambda (func) (compile-func func mappings)) funcs)))

(define (funcs->table funcs) `(table ,(length funcs) anyfunc))
(define (funcs->mappings funcs) (map func->mapping funcs))
(define (funcs->elems funcs) `(elem (i32.const 0) ,@(funcs->mappings funcs)))
; end func

(define wasm-i32-size 4)
(define wasm-heap-top-ptr 16380)
(define (compile-program funcs)
    (let ((cfuncs (compile-funcs funcs)))
        `(module
            (type $$close0 (func (param i32) (result i32)))
            (type $$close1 (func (param i32) (param i32) (result i32)))
            (type $$close2 (func (param i32) (param i32) (param i32) (result i32)))
            (type $$close3 (func (param i32) (param i32) (param i32) (param i32) (result i32)))
            (memory $0 16384)
            (export "memory" (memory 0))
            ,(funcs->table funcs)
            ,(funcs->elems funcs)

            (func $$main (result i32) (call $$fentry))

            (func $$alloc (param $size i32) (result i32) (local $ptr i32)
                ; get current top of heap from heap top pointer (16383)
                (i32.const ,wasm-heap-top-ptr)
                (i32.load)
                (set_local $ptr)

                ; inc pointer by param
                (i32.const ,wasm-heap-top-ptr)
                (get_local $ptr)
                (get_local $size)
                (i32.const ,wasm-i32-size)
                (i32.mul)
                (i32.add)
                (i32.store)

                ; return pointer
                (get_local $ptr))

            (func $$alloc-slot (param $val i32) (result i32) (local $ptr i32)
                (i32.const 1) ; size
                (call $$alloc)
                (tee_local $ptr)

                (get_local $val) ; store val
                (i32.store)

                (get_local $ptr))

            (func $$alloc-pair (param $car i32) (param $cdr i32) (result i32) (local $ptr i32)
                (i32.const 2) ; size
                (call $$alloc)
                (tee_local $ptr)

                (get_local $car) ; store car
                (i32.store)

                (get_local $ptr) ; store cdr
                (i32.const ,wasm-i32-size)
                (i32.add)
                (get_local $cdr)
                (i32.store)

                (get_local $ptr) ; get ptr
                (i32.const ,wobj-shift) ; shift left
                (i32.shl)   
                (i32.const ,wpair-tag) ; tag as pair
                (i32.or)
                )

            (func $$alloc-close (param $findex i32) (param $size i32) (result i32) (local $ptr i32)
                (get_local $size)
                (i32.const 1)
                (i32.add)
                (call $$alloc)

                (tee_local $ptr)
                (get_local $findex)
                (i32.store)

                (get_local $ptr)
                (i32.const ,wobj-shift) ; shift left
                (i32.shl)   
                (i32.const ,wclose-tag) ; tag as slot
                (i32.or)
            )

            (func $$store-free 
                (param $ptr i32) 
                (param $index i32)
                (param $val i32)
                (result i32)

                (get_local $ptr)
                (i32.const ,wobj-shift)
                (i32.shr_u) ; convert to real pointer
                (i32.const ,wasm-i32-size)
                (i32.add) ; inc to first slot
                (get_local $index)
                (i32.const ,wasm-i32-size)
                (i32.mul)
                (i32.add) ; inc to requested slot
                (get_local $val)
                (i32.store)
                (get_local $ptr))

            (func $$get-free
                (param $ptr i32) 
                (param $index i32)
                (result i32)

                (get_local $ptr)
                (i32.const ,wobj-shift)
                (i32.shr_u) ; convert to real pointer
                (i32.const ,wasm-i32-size)
                (i32.add) ; inc to first slot
                (get_local $index)
                (i32.const ,wasm-i32-size)
                (i32.mul)
                (i32.add) ; inc to requested slot

                (i32.load))
            
            (func $$get-close-func-index (param $close i32) (result i32)
                (get_local $close)
                (i32.const ,wobj-shift)
                (i32.shr_u)
                (i32.load))

            (func $$call-close-0 (param $$close i32)
                (result i32)
                (local $$idx i32)

                (get_local $$close)

                (get_local $$close)
                (call $$get-close-func-index)

                (call_indirect (type $$close0))
            )

            (func $$call-close-1 (param $$close i32) (param $$0 i32)
                (result i32)
                (local $$idx i32)

                (get_local $$0)
                (get_local $$close)

                (get_local $$close)
                (call $$get-close-func-index)

                (call_indirect (type $$close1))
            )

            (func $$call-close-2 (param $$close i32) (param $$0 i32) (param $$1 i32)
                (result i32)
                (local $$idx i32)

                (get_local $$0)
                (get_local $$1)
                (get_local $$close)

                (get_local $$close)
                (call $$get-close-func-index)

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
                (call $$get-close-func-index)

                (call_indirect (type $$close3))
            )

            (func $$unslot (param $slot i32) (result i32)
                (get_local $slot)
                (i32.load))

            (func $$car (param $pair i32) (result i32)
                (get_local $pair)
                (i32.const ,wobj-shift)
                (i32.shr_u)
                (i32.load))
            (func $$cdr (param $pair i32) (result i32)
                (get_local $pair)
                (i32.const ,wobj-shift)
                (i32.shr_u)
                (i32.const ,wasm-i32-size)
                (i32.add)
                (i32.load))

            ,@cfuncs
            (export "main" (func $$main))
        )))

(define (p05_funcs2wat funcs) (compile-program funcs))))