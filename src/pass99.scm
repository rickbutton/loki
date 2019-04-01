(define-module (pass99)
    #:export (funcs->wat))

(use-modules (util))
(use-modules (srfi srfi-1))

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
;end var

; scope / vars

(define (mapping->get-bound mapping func)
    (let* ((bounds (func->bounds func)) (mappings (if (null? bounds) '() (map caddr bounds))))
        (if (eq? (func->type func) 'close)
            `(call $$get-bound-slot (get_local $$bounds) (i32.const ,(index mapping mappings)))
            `(get_local ,mapping))))
(define (mapping->get-free mapping func)
    (let* ((frees (func->frees func)) (mappings (if (null? frees) '() (map caddr frees))))
        `(call $$get-free (get_local $$close) (i32.const ,(index mapping mappings)))))

(define (mapping->get-bound-or-free mapping func)
    (if (func-has-bound? func mapping)
        (mapping->get-bound mapping func)
        (if (func-has-free? func mapping)
            (mapping->get-free mapping func)
            `(unknown ,mapping ,(func->bounds func) ,(func->frees func)))))
            ;(error (string-append "unknown mapping " (symbol->string mapping))))))

; refer
(define (refer->type s) (caddr s))
(define (refer->mapping s) (cadddr s))
(define (compile-refer s func) 
    (if (eq? (func->type func) 'close)
        `(inst 
            ,(mapping->get-bound-or-free (refer->mapping s) func)
            (call $$unslot))
        (mapping->get-bound-or-free (refer->mapping s) func)))
; end refer

(define (compile-referfunc r func mappings)
    (let* ((idx (index (cadr r) mappings))
           (body (func->body func))
           (vars (caddr r)))
        `(inst (call $$alloc-close (i32.const ,idx) (i32.const ,(length vars)))
          ,@(apply append (map (lambda (f i) `(
             (i32.const ,i)
             ,(mapping->get-bound-or-free (cdr f) func)
             (call $$store-free)
          )) vars (range 0 (length vars) 1))))))
         ; need to pump located frees (find if var is free in current scope, and emit insts to store in allocated close)
        ; ^ this doesn't work, we need to alloc-close first, and then call $$store-close-free for each free
        ; maybe, generate an $alloc-close for each size of closure, might suck

(define (compile-apply a) 
    `(inst
      (call $$alloc-bounds (i32.const ,(cadr a)))
      ,@(map (lambda (i) `(call $$store-bound (i32.const ,i))) (range 0 (cadr a) 1))
      (call $$call-close)))
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
(define wclose-tag #b11)

(define (op-match op) (lambda (v) (and (list? v) (eq? (car v) op))))
(define free? (op-match 'free))
(define bound? (op-match 'bound))

(define (func->type f) (cadr f))
(define (func->mapping f) (caddr f))
(define (func->body f) (cdddr f))
(define (func->name f) (string->symbol (symbol->string (func->mapping f))))

(define (func->bounds f) (filter bound? (func->body f)))
(define (func->frees f) (filter free? (func->body f)))
(define (func-has-bound? f mapping) (find (lambda (i) (eq? mapping (caddr i))) (func->bounds f)))
(define (func-has-free? f mapping) (find (lambda (i) (eq? mapping (caddr i))) (func->frees f)))
(define (func->bounds-param f)
    (let ((bounds (func->bounds f)))
        (if (eq? (length bounds) 0) '() (list '(param $$bounds i32)))))

(define (var? i) (or (free? i) (bound? i)))
(define (non-var? i) (not (var? i)))
(define (compile-func func mappings) 
    (let* ((body   (func->body func))
          (bounds  (compile-insts (filter bound? body) func mappings))
          (insts  (compile-insts (filter non-var? body) func mappings)))
        (if (eq? (func->type func) 'close)
            `(func ,(func->name func) (param $$close i32) ,@(func->bounds-param func) (result i32) ,@insts)
            `(func ,(func->name func) (result i32) ,@(map (lambda (b) `(local ,(caddr b) i32)) bounds) ,@insts))))

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
            (memory $0 16384)
            (export "memory" (memory 0))
            (type $$closefunc (func (param i32) (param i32) (result i32)))
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

            (func $$call-close (param $$close i32) (param $$bounds i32)
                (result i32)
                (local $$idx i32)

                (get_local $$close)
                (get_local $$bounds)

                (get_local $$close)
                (call $$get-close-func-index)

                (call_indirect (type $$closefunc))
            )

            (func $$alloc-bounds (param $size i32) (result i32)
                (get_local $size)
                (call $$alloc))

            (func $$store-bound 
                (param $val i32) 
                (param $ptr i32)
                (param $index i32)
                (result i32)

                (get_local $index)
                (i32.const ,wasm-i32-size)
                (i32.mul)

                (get_local $ptr)

                (i32.add) ; inc to requested slot
                (get_local $val)
                (i32.store)
                (get_local $ptr))

            (func $$get-bound-slot
                (param $ptr i32) 
                (param $index i32)
                (result i32)

                (get_local $index)
                (i32.const ,wasm-i32-size)
                (i32.mul)

                (get_local $ptr)

                (i32.add)) ; inc to requested slot


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

(define (funcs->wat funcs) (compile-program funcs))