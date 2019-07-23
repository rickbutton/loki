# schwasm

### A Scheme to WebAssembly Compiler

This supports almost zero percent of any scheme standard or spec. I'm hoping to eventually make this R7RS small compliant at least. No promises.

Take caution, because this probably doesn't work on your machine without tweaks.

### How to run

You will need [chibi scheme](https://github.com/ashinn/chibi-scheme/), [node](https://nodejs.org), and [wabt](https://github.com/WebAssembly/wabt) on your path.

```
make example
```

### TODO

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

```scheme
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
        (table 2 anyfunc)
        (elem (i32.const 0) $$fentry $$f1)
        (func $$main (result i32) (call $$fentry))
        (func $$fentry
              (result i32)
              (local $v1_fib i32)
              (local $$tmp i32)
              (set_local $v1_fib (call $$alloc_slot))
              (call $$alloc_close (;func=$$f1;) (i32.const 1) (i32.const 1))
              (i32.const 0)
              (get_local $v1_fib)
              (call $$store_free)
              (call $$set_slot (get_local $v1_fib))
              (i32.const (;val=25;) 100)
              (call $$get_slot (get_local $v1_fib))
              (tee_local $$tmp)
              (get_local $$tmp)
              (call $$get_close_func_index)
              (call_indirect (type $$fun$1))
              (return))
        (func $$f1
              (param $v2_n i32)
              (param $$close i32)
              (result i32)
              (local $$tmp i32)
              (get_local $v2_n)
              (i32.const (;val=2;) 8)
              (i32.le_s)
              (if (result i32)
                  (then (i32.const (;val=#t;) 159))
                  (else (i32.const (;val=#f;) 31)))
              (i32.const (;val=#f;) 31)
              (i32.ne)
              (if (result i32)
                  (then (i32.const (;val=1;) 4))
                  (else (get_local $v2_n)
                        (i32.const (;val=1;) 4)
                        (i32.sub)
                        (call $$get_slot
                              (call $$get_free
                                    (get_local $$close)
                                    (i32.const 0)))
                        (tee_local $$tmp)
                        (get_local $$tmp)
                        (call $$get_close_func_index)
                        (call_indirect (type $$fun$1))
                        (get_local $v2_n)
                        (i32.const (;val=2;) 8)
                        (i32.sub)
                        (call $$get_slot
                              (call $$get_free
                                    (get_local $$close)
                                    (i32.const 0)))
                        (tee_local $$tmp)
                        (get_local $$tmp)
                        (call $$get_close_func_index)
                        (call_indirect (type $$fun$1))
                        (i32.add)))
              (return))
        (export "main" (func $$main)))```
