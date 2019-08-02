# schwasm

### A Scheme to WebAssembly Compiler

This supports almost zero percent of any scheme standard or spec. I'm hoping to eventually make this R7RS small compliant at least. No promises.

Take caution, because this probably doesn't work on your machine without tweaks.

### How to run

You will need [chibi scheme](https://github.com/ashinn/chibi-scheme/), [node](https://nodejs.org), and [wabt](https://github.com/WebAssembly/wabt) on your path.

```
make example

```

### Things that work?

- values
    - fixnums (unsigned, 0 -> 2^30, no overflow protection)
    - chars
    - booleans
    - null
    - pairs
    - utf8 strings
- operations
    - simple math operations
    - pair operations (cons/car/cdr)
    - few conditional operators
        - le_s
- syntax
    - (begin exprs ...)
    - (define var expr) **(no (define (var formals) body))**
    - (set! var expr)
    - (lambda (formals) body)

A CPS transformation is done to the source, which enables full tail call optimization, and eventually first-order continuations, although `call/cc` is not yet implemented (it will be trivial to add, though).

### TODO

- any level of optimization (CPS-induced code explosion is real!)
- more heap allocated values (runtime symbols, etc)
- self hosting
- modules (via define-library)
- test suite for back end
- garbage collection
- easy FFI with JavaScript
- WASI?

### Example

This example may be out of date. No Promises!

input:

```scheme
(define (fib n)
    (if (%%prim%le_s n 2)
        1
        (%%prim%add 
            (fib (%%prim%sub n 1)) 
            (fib (%%prim%sub n 2)))))

(fib 25)
```

output:

```wat
(module (type $$fun$0 (func (param i32) (result i32)))
        (type $$fun$1 (func (param i32) (param i32) (result i32)))
        (type $$fun$2 (func (param i32) (param i32) (param i32) (result i32)))
        (type $$fun$3
              (func (param i32)
                    (param i32)
                    (param i32)
                    (param i32)
                    (result i32)))
        (global $$rodata-id (import "env" "$$rodata-id") i32)
        (global $$rodata-offset (import "env" "$$rodata-offset") i32)
        (import "env" "memory" (memory 0))
        (import "env" "$$alloc_slot" (func $$alloc_slot (result i32)))
        (import "env" "$$set_slot" (func $$set_slot (param i32 i32)))
        (import "env" "$$get_slot" (func $$get_slot (param i32) (result i32)))
        (import "env"
                "$$alloc_pair"
                (func $$alloc_pair (param i32 i32) (result i32)))
        (import "env" "$$car" (func $$car (param i32) (result i32)))
        (import "env" "$$cdr" (func $$cdr (param i32) (result i32)))
        (import "env"
                "$$alloc_close"
                (func $$alloc_close (param i32 i32) (result i32)))
        (import "env"
                "$$store_free"
                (func $$store_free (param i32 i32 i32) (result i32)))
        (import "env"
                "$$get_free"
                (func $$get_free (param i32 i32) (result i32)))
        (import "env"
                "$$get_close_func_index"
                (func $$get_close_func_index (param i32) (result i32)))
        (import "env"
                "$$alloc_string"
                (func $$alloc_string (param i32 i32 i32) (result i32)))
        (data (get_global $$rodata-offset) "")
        (table 9 anyfunc)
        (elem (i32.const 0)
              $$fentry
              $$f7
              $$f6
              $$f5
              $$f4
              $$f3
              $$f2
              $$f1
              $$fexit)
        (func $$main (result i32) (call $$fentry))
        (func $$fentry
              (result i32)
              (local $v1_fib i32)
              (local $$tmp i32)
              (set_local $v1_fib (call $$alloc_slot))
              (call $$alloc_close (;func=$$f7;) (i32.const 1) (i32.const 1))
              (i32.const 0)
              (get_local $v1_fib)
              (call $$store_free)
              (call $$set_slot (get_local $v1_fib))
              (i32.const (;val=25;) 100)
              (call $$alloc_close
                    (;func=$$fexit;)
                    (i32.const 8)
                    (i32.const 0))
              (call $$get_slot (get_local $v1_fib))
              (tee_local $$tmp)
              (get_local $$tmp)
              (call $$get_close_func_index)
              (return_call_indirect (type $$fun$2)))
        (func $$f7
              (param $v2_n i32)
              (param $k_1 i32)
              (param $$close i32)
              (result i32)
              (local $$tmp i32)
              (get_local $k_1)
              (call $$alloc_close (;func=$$f6;) (i32.const 2) (i32.const 2))
              (i32.const 0)
              (get_local $v2_n)
              (call $$store_free)
              (i32.const 1)
              (call $$get_free (get_local $$close) (i32.const 0))
              (call $$store_free)
              (tee_local $$tmp)
              (get_local $$tmp)
              (call $$get_close_func_index)
              (return_call_indirect (type $$fun$1)))
        (func $$f6
              (param $k_2 i32)
              (param $$close i32)
              (result i32)
              (local $$tmp i32)
              (call $$get_slot
                    (call $$get_free (get_local $$close) (i32.const 0)))
              (i32.const (;val=2;) 8)
              (i32.le_s)
              (if (result i32)
                  (then (i32.const (;val=#t;) 159))
                  (else (i32.const (;val=#f;) 31)))
              (call $$alloc_close (;func=$$f5;) (i32.const 3) (i32.const 3))
              (i32.const 0)
              (call $$get_free (get_local $$close) (i32.const 0))
              (call $$store_free)
              (i32.const 1)
              (call $$get_free (get_local $$close) (i32.const 1))
              (call $$store_free)
              (i32.const 2)
              (get_local $k_2)
              (call $$store_free)
              (tee_local $$tmp)
              (get_local $$tmp)
              (call $$get_close_func_index)
              (return_call_indirect (type $$fun$1)))
        (func $$f5
              (param $rv_1 i32)
              (param $$close i32)
              (result i32)
              (local $$tmp i32)
              (get_local $rv_1)
              (i32.const (;val=#f;) 31)
              (i32.ne)
              (if (result i32)
                  (then (i32.const (;val=1;) 4)
                        (call $$get_slot
                              (call $$get_free
                                    (get_local $$close)
                                    (i32.const 2)))
                        (tee_local $$tmp)
                        (get_local $$tmp)
                        (call $$get_close_func_index)
                        (return_call_indirect (type $$fun$1)))
                  (else
                   (call $$get_slot
                         (call $$get_free (get_local $$close) (i32.const 0)))
                   (i32.const (;val=1;) 4)
                   (i32.sub)
                   (call $$alloc_close
                         (;func=$$f4;)
                         (i32.const 4)
                         (i32.const 3))
                   (i32.const 0)
                   (call $$get_free (get_local $$close) (i32.const 0))
                   (call $$store_free)
                   (i32.const 1)
                   (call $$get_free (get_local $$close) (i32.const 1))
                   (call $$store_free)
                   (i32.const 2)
                   (call $$get_free (get_local $$close) (i32.const 2))
                   (call $$store_free)
                   (tee_local $$tmp)
                   (get_local $$tmp)
                   (call $$get_close_func_index)
                   (return_call_indirect (type $$fun$1)))))
        (func $$f4
              (param $rv_5 i32)
              (param $$close i32)
              (result i32)
              (local $$tmp i32)
              (get_local $rv_5)
              (call $$alloc_close (;func=$$f3;) (i32.const 5) (i32.const 3))
              (i32.const 0)
              (call $$get_free (get_local $$close) (i32.const 0))
              (call $$store_free)
              (i32.const 1)
              (call $$get_free (get_local $$close) (i32.const 1))
              (call $$store_free)
              (i32.const 2)
              (call $$get_free (get_local $$close) (i32.const 2))
              (call $$store_free)
              (call $$get_slot
                    (call $$get_free (get_local $$close) (i32.const 1)))
              (tee_local $$tmp)
              (get_local $$tmp)
              (call $$get_close_func_index)
              (return_call_indirect (type $$fun$2)))
        (func $$f3
              (param $rv_2 i32)
              (param $$close i32)
              (result i32)
              (local $$tmp i32)
              (call $$get_slot
                    (call $$get_free (get_local $$close) (i32.const 0)))
              (i32.const (;val=2;) 8)
              (i32.sub)
              (call $$alloc_close (;func=$$f2;) (i32.const 6) (i32.const 3))
              (i32.const 0)
              (call $$get_free (get_local $$close) (i32.const 1))
              (call $$store_free)
              (i32.const 1)
              (get_local $rv_2)
              (call $$store_free)
              (i32.const 2)
              (call $$get_free (get_local $$close) (i32.const 2))
              (call $$store_free)
              (tee_local $$tmp)
              (get_local $$tmp)
              (call $$get_close_func_index)
              (return_call_indirect (type $$fun$1)))
        (func $$f2
              (param $rv_4 i32)
              (param $$close i32)
              (result i32)
              (local $$tmp i32)
              (get_local $rv_4)
              (call $$alloc_close (;func=$$f1;) (i32.const 7) (i32.const 2))
              (i32.const 0)
              (call $$get_free (get_local $$close) (i32.const 1))
              (call $$store_free)
              (i32.const 1)
              (call $$get_free (get_local $$close) (i32.const 2))
              (call $$store_free)
              (call $$get_slot
                    (call $$get_free (get_local $$close) (i32.const 0)))
              (tee_local $$tmp)
              (get_local $$tmp)
              (call $$get_close_func_index)
              (return_call_indirect (type $$fun$2)))
        (func $$f1
              (param $rv_3 i32)
              (param $$close i32)
              (result i32)
              (local $$tmp i32)
              (call $$get_slot
                    (call $$get_free (get_local $$close) (i32.const 0)))
              (get_local $rv_3)
              (i32.add)
              (call $$get_slot
                    (call $$get_free (get_local $$close) (i32.const 1)))
              (tee_local $$tmp)
              (get_local $$tmp)
              (call $$get_close_func_index)
              (return_call_indirect (type $$fun$1)))
        (func $$fexit
              (param $v i32)
              (param $$close i32)
              (result i32)
              (local $$tmp i32)
              (get_local $v))
        (export "main" (func $$main)))
```
