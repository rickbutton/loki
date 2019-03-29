(define-module (pass99)
    #:export (funcs->wat))

(use-modules (util))

; fixnum
(define wfixnum-shift 2)
(define wfixnum-mask  #b11)
(define wfixnum-tag  #b00)

(define (sfixnum? i) (integer? i))
(define (wfixnum? i) (eq? (bitwise-and i wfixnum-mask) wfixnum-tag))

(define (sfixnum->wfixnum i) (ash i wfixnum-shift))
(define (wfixnum->sfixnum i) (ash i (- 0 wfixnum-shift)))

(define (compile-sfixnum x)
    `(i32.const ,(sfixnum->wfixnum x)))
; end fixnum

; boolean
(define wboolean-mask #b0011111)
(define wboolean-tag #b0011111)
(define false-tag #b00011111)
(define true-tag #b10011111)

(define (sboolean? b) (boolean? b))
(define (wboolean? b) (eq (bitwise-and i wboolean-mask) wboolean-tag))

(define (sboolean->wboolean b) (if b true-tag false-tag))
(define (wboolean->sboolean b) (if (eq b true-tag) #t #f))

(define (compile-sboolean x)
    `(i32.const ,(sboolean->wboolean x)))
; end boolean

; char
(define wchar-mask #b00011111)
(define wchar-tag  #b00001111)
(define wchar-shift 8)

(define (schar? c) (char? c))
(define (wchar? b) (eq (bitwise-and i wchar-mask) wchar-tag))

(define (schar->wchar c) (bitwise-ior (ash (char->integer c) wchar-shift) wchar-tag))
(define (wchar->schar c) (ash (char->integer c) (- 0 wchar-shift)))

(define (compile-schar x)
    `(i32.const ,(schar->wchar x)))
; end char

; null
(define null-tag   #b00101111)

(define (snull? n) (null? n))
(define (wnull? n) (eq n null-tag))

(define (snull->wnull n) null-tag)
(define (wnull->snull n) '())

(define (compile-snull x)
    `(i32.const ,(snull->wnull x)))
; end null

(define wobj-mask   #b111)
(define wobj-shift   3)
; pair
(define wpair-tag   #b01)

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



; var
(define wvar-tag   #b10)
(define (compile-var x)
    `(call $$alloc-slot))
;end var

; scope / vars

(define (mapping->get-bound-or-free mapping func)
    (let* ((frees (func->frees func)) (mappings (if (null? frees) '() (map cdr frees))))
        (if (member mapping mappings)
            `(call $$get-close-free-slot (get_local $$close) (i32.const ,(index mapping mappings)))
            `(get_local ,mapping))))

; refer
(define (refer->type s) (caddr s))
(define (refer->mapping s) (cadddr s))
(define (compile-refer s func) `(call $$unslot ,(mapping->get-bound-or-free (refer->mapping s) func)))
; end refer

(define (compile-referfunc r func mappings)
    (let ((idx (index (cadr r) mappings)) (frees (caddr r)))
        `(block (result i32)
          (call $$alloc-close (i32.const ,idx) (i32.const ,(length frees)))
          ,@(apply append (map (lambda (f i) `(
             (i32.const ,i)
             ,(mapping->get-bound-or-free (cdr f) func)
             (call $$store-close-free)
          )) frees (range 0 (length frees) 1))))))
         ; need to pump located frees (find if var is free in current scope, and emit insts to store in allocated close)
        ; ^ this doesn't work, we need to alloc-close first, and then call $$store-close-free for each free
        ; maybe, generate an $alloc-close for each size of closure, might suck
(define (compile-param p) `(param ,(car (cdr (cdr p))) i32))

(define (number->call-close-name n)
    (string->symbol (string-append "$$call-close-" (number->string n))))
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
            ((eq? op 'primcall) (compile-primcall i))
            ((eq? op 'param) (compile-param i))
            ((eq? op 'var) (compile-var i))
            ((eq? op 'apply) (compile-apply i))
            (else i))))
(define (compile-insts is func mappings) (map (lambda (i) (compile-inst i func mappings)) is))
; end inst

; 

; func
(define wclose-tag #b11)

(define (op-match op) (lambda (v) (and (list? v) (eq? (car v) op))))
(define var? (op-match 'var))
(define param? (op-match 'param))
(define (non-env? x) (not (param? x)))

(define (func->type f) (cadr f))
(define (func->mapping f) (caddr f))
(define (func->frees f) (car (cdr (cadddr f))))
(define (func->body f) (cddddr f))
(define (func->name f) (string->symbol (symbol->string (func->mapping f))))

(define (var->funcheader p) `(local ,(car (cdr (cdr p))) i32))
(define (compile-func func mappings) 
    (let* ((body   (func->body func))
          (insts  (compile-insts (filter non-env? body) func mappings))
          (vars   (compile-insts (map var->funcheader (filter var? body)) func mappings))
          (params (compile-insts (filter param? body) func mappings)))
        (if (eq? (func->type func) 'close)
            `(func ,(func->name func) ,@params (param $$close i32) (result i32) ,@vars ,@insts)
            `(func ,(func->name func) ,@params (result i32) ,@vars ,@insts))))

(define (compile-funcs funcs) 
    (let ((mappings (funcs->mappings funcs)))
        (map (lambda (func) (compile-func func mappings)) funcs)))

(define (funcs->table funcs) `(table ,(length funcs) anyfunc))
(define (funcs->mappings funcs) (map func->mapping funcs))
(define (funcs->elems funcs) `(elem (i32.const 0) ,@(funcs->mappings funcs)))
; end func

; funcsig
(define (apply->arg-count x) (car (cdr x)))
(define (func->max-apply-args func)
    (let* ((applys (filter (lambda (i) (eq? (car i) 'apply)) (func->body func)))
           (counts (map (lambda (a) (apply->arg-count a)) applys)))
        (if (null? counts) 0 (car (sort counts >)))))
(define (funcs->max-apply-args funcs)
    (let ((maxs (map func->max-apply-args funcs)))
        (car (sort maxs >))))
    
(define (number->funcsig-params n)
    (if (eq? 0 n) '() (cons '(param i32) (number->funcsig-params (- n 1)))))

(define (number->funcsig-name n)
    (string->symbol (string-append "$funcsig$" (number->string n))))

(define (number->funcsig n)
    `(type
        ,(number->funcsig-name n)
        (func ,@(number->funcsig-params (+ 1 n)) (result i32))))

(define (funcs->funcsigs funcs)
    (let ((max (funcs->max-apply-args funcs)))
        (map number->funcsig (range 0 (+ max 2) 1))))

(define (number->call-close-param-name n)
    (string->symbol (string-append "$$param-" (number->string n))))
(define (number->call-close-get-param n)
    `(get_local ,(number->call-close-param-name n)))
(define (number->call-close n)
    `(func ,(number->call-close-name n)
        ,@(map (lambda (n) `(param ,(number->call-close-param-name n) i32)) (range 0 n 1))
        (param $close i32)
        (result i32)

        ,@(map (lambda (n) (number->call-close-get-param n)) (range 0 n 1))

        (get_local $close)
        (get_local $close)
        (call $$get-close-func-index)
        (call_indirect (type ,(number->funcsig-name n)))))
(define (funcs->call-close funcs)
    (let ((max (funcs->max-apply-args funcs)))
        (map number->call-close (range 0 (+ max 1) 1))))
; end funcsig

(define wasm-i32-size 4)
(define wasm-heap-top-ptr 16380)
(define (compile-program funcs)
    (let ((cfuncs (compile-funcs funcs)))
        `(module
            ,@(funcs->funcsigs funcs)
            (memory $0 16384)
            (export "memory" (memory 0))
            ,(funcs->table funcs)
            ,(funcs->elems funcs)

            (func $$main (result i32) (call $$fentry))

            ,@(funcs->call-close funcs)

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

            (func $$alloc-slot (param $val i32) (result i32) (local $ptr i32)
                (i32.const 1) ; size
                (call $$alloc)
                (tee_local $ptr)
                (get_local $val) ; store car
                (i32.store)

                (get_local $ptr) ; get ptr
                (i32.const ,wobj-shift) ; shift left
                (i32.shl)   
                (i32.const ,wvar-tag) ; tag as slot
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

            (func $$store-close-free 
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

            (func $$get-close-free-slot
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

                (i32.const ,wobj-shift) ; shift left
                (i32.shl)   
                (i32.const ,wvar-tag) ; tag as slot
                (i32.or))
            
            (func $$get-close-func-index (param $close i32) (result i32)
                (get_local $close)
                (i32.const ,wobj-shift)
                (i32.shr_u)
                (i32.load))

            (func $$unslot (param $slot i32) (result i32)
                (get_local $slot)
                (i32.const ,wobj-shift)
                (i32.shr_u)
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

(define (funcs->wat funcs) (compile-program funcs))