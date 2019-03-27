(define-module (pass99)
    #:export (funcs->wat))

(use-modules (util))

; fixnum
(define wfixnum-shift 2)
(define wfixnum-tag  #b00)

(define (sfixnum? i) (integer? i))
(define (wfixnum? i) (eq? (bitwise-and i wfixnum-tag) wfixnum-tag))

(define (sfixnum->wfixnum i) (ash i wfixnum-shift))
(define (wfixnum->sfixnum i) (ash i (- 0 wfixnum-shift)))

(define (compile-sfixnum x)
    `(i32.const ,(sfixnum->wfixnum x)))
; end fixnum

; boolean
(define wboolean-tag #b0011111)
(define false-tag #b00011111)
(define true-tag #b10011111)

(define (sboolean? b) (boolean? b))
(define (wboolean? b) (eq (bitwise-and i wboolean-tag) wboolean-tag))

(define (sboolean->wboolean b) (if b true-tag false-tag))
(define (wboolean->sboolean b) (if (eq b true-tag) #t #f))

(define (compile-sboolean x)
    `(i32.const ,(sboolean->wboolean x)))
; end boolean

; char
(define wchar-tag #b00001111)
(define wchar-shift 8)

(define (schar? c) (char? c))
(define (wchar? b) (eq (bitwise-and i wchar-tag) wchar-tag))

(define (schar->wchar c) (bitwise-ior (ash (char->integer c) wchar-shift) wchar-tag))
(define (wchar->schar c) (ash (char->integer c) (- 0 wchar-shift)))

(define (compile-schar x)
    `(i32.const ,(schar->wchar x)))
; end char

; null
(define null-tag #b00101111)

(define (snull? n) (null? n))
(define (wnull? n) (eq n null-tag))

(define (snull->wnull n) null-tag)
(define (wnull->snull n) '())

(define (compile-snull x)
    `(i32.const ,(snull->wnull x)))
; end null

; prim
(define (prim->op x) (car (cdr x)))

(define (compile-primcall p)
    (let ((op (prim->op p)))
        (cond
            ((eq? op 'add) '(i32.add))
            ((eq? op 'sub) '(i32.sub))
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

; refer
(define (refer->type s) (caddr s))
(define (refer->mapping s) (cadddr s))
(define (compile-refer s) `(get_local ,(refer->mapping s)))
; end refer

; scope / vars
(define (compile-referfunc r mappings) `(i32.const ,(index (car (cdr r)) mappings)))
(define (compile-param p) `(param ,(car (cdr (cdr p))) i32))
(define (compile-var p) `(local ,(car (cdr (cdr p))) i32))
(define (compile-apply a) `(call_indirect (type ,(number->funcsig-name (car (cdr a))))))
; end scope / vars

; inst
(define (compile-inst i mappings) 
    (let ((op (car i)))
        (cond
            ((eq? op 'constant) (compile-constant i))
            ((eq? op 'store) (compile-store i))
            ((eq? op 'refer) (compile-refer i))
            ((eq? op 'referfunc) (compile-referfunc i mappings))
            ((eq? op 'primcall) (compile-primcall i))
            ((eq? op 'param) (compile-param i))
            ((eq? op 'var) (compile-var i))
            ((eq? op 'apply) (compile-apply i))
            (else i))))
(define (compile-insts is mappings) (map (lambda (i) (compile-inst i mappings)) is))
; end inst

; func
(define (op-match op) (lambda (v) (and (list? v) (eq? (car v) op))))
(define var? (op-match 'var))
(define param? (op-match 'param))
(define (non-env? x) (and (not (var? x)) (not (param? x))))

(define (func->mapping f) (car (cdr f)))
(define (func->body f) (cdr (cdr f)))
(define (func->name f) (string->symbol (symbol->string (func->mapping f))))

(define (compile-func func mappings) 
    (let* ((body   (func->body func))
          (insts  (compile-insts (filter non-env? body) mappings))
          (vars   (compile-insts (filter var? body) mappings))
          (params (compile-insts (filter param? body) mappings)))
        `(func ,(func->name func) ,@params (result i32) ,@vars ,@insts)))

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
        (func ,@(number->funcsig-params n) (result i32))))

(define (funcs->funcsigs funcs)
    (let ((max (funcs->max-apply-args funcs)))
        (map number->funcsig (range 0 (+ max 1) 1))))
; end funcsig

(define (compile-program funcs)
    (let ((cfuncs (compile-funcs funcs)))
        `(module
            ,@(funcs->funcsigs funcs)
            (memory $0 1024)
            ,(funcs->table funcs)
            ,(funcs->elems funcs)

            ,@cfuncs
            (func $$main (result i32) (call $$fentry))
            (export "main" (func $$main))
        )))

(define (funcs->wat funcs) (compile-program funcs))