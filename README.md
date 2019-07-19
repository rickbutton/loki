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
(define (+ a b c) (%%prim%add (%%prim%add a b) c))

(define (test x)
    (define y (%%prim%add x 2))
    (define z (%%prim%add x 3))
    (+ x y z))

(%%prim%cons (test 10) "😀 schwasm!")
```

output:

```scheme
(module (type $$close0 (func (param i32) (result i32)))
        (type $$close1 (func (param i32) (param i32) (result i32)))
        (type $$close2
              (func (param i32) (param i32) (param i32) (result i32)))
        (type $$close3
              (func (param i32)
                    (param i32)
                    (param i32)
                    (param i32)
                    (result i32)))
        (global $$rodata-id (import "env" "$$rodata-id") i32)
        (global $$rodata-offset (import "env" "$$rodata-offset") i32)
        (import "env" "memory" (memory 0))
        (import "env"
                "$$alloc_slot"
                (func $$alloc_slot (param i32) (result i32)))
        (import "env" "$$unslot" (func $$unslot (param i32) (result i32)))
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
        (data (get_global $$rodata-offset) "😀 schwasm!")
        (table 3 anyfunc)
        (elem (i32.const 0) $$fentry $$f1 $$f2)
        (func $$main (result i32) (call $$fentry))
        (func $$call-close-0
              (param $$close i32)
              (result i32)
              (local $$idx i32)
              (get_local $$close)
              (get_local $$close)
              (call $$get_close_func_index)
              (call_indirect (type $$close0)))
        (func $$call-close-1
              (param $$close i32)
              (param $$0 i32)
              (result i32)
              (local $$idx i32)
              (get_local $$0)
              (get_local $$close)
              (get_local $$close)
              (call $$get_close_func_index)
              (call_indirect (type $$close1)))
        (func $$call-close-2
              (param $$close i32)
              (param $$0 i32)
              (param $$1 i32)
              (result i32)
              (local $$idx i32)
              (get_local $$0)
              (get_local $$1)
              (get_local $$close)
              (get_local $$close)
              (call $$get_close_func_index)
              (call_indirect (type $$close2)))
        (func $$call-close-3
              (param $$close i32)
              (param $$0 i32)
              (param $$1 i32)
              (param $$2 i32)
              (result i32)
              (local $$idx i32)
              (get_local $$0)
              (get_local $$1)
              (get_local $$2)
              (get_local $$close)
              (get_local $$close)
              (call $$get_close_func_index)
              (call_indirect (type $$close3)))
        (func $$fentry
              (result i32)
              (local $v1_+ i32)
              (local $v5_test i32)
              (call $$alloc_close (i32.const 1) (i32.const 0))
              (call $$alloc_slot)
              (set_local $v1_+)
              (call $$alloc_close (i32.const 2) (i32.const 1))
              (i32.const 0)
              (call $$unslot (get_local $v1_+))
              (call $$store_free)
              (call $$alloc_slot)
              (set_local $v5_test)
              (call $$unslot (get_local $v5_test))
              (i32.const 40)
              (call $$call-close-1)
              (call $$alloc_string
                    (get_global $$rodata-id)
                    (i32.const 0)
                    (i32.const 13))
              (call $$alloc_pair)
              (return))
        (func $$f1
              (param $v2_a i32)
              (param $v3_b i32)
              (param $v4_c i32)
              (param $$close i32)
              (result i32)
              (get_local $v2_a)
              (get_local $v3_b)
              (i32.add)
              (get_local $v4_c)
              (i32.add)
              (return))
        (func $$f2
              (param $v6_x i32)
              (param $$close i32)
              (result i32)
              (local $v1_+ i32)
              (local $v7_y i32)
              (local $v8_z i32)
              (get_local $v6_x)
              (i32.const 8)
              (i32.add)
              (call $$alloc_slot)
              (set_local $v7_y)
              (get_local $v6_x)
              (i32.const 12)
              (i32.add)
              (call $$alloc_slot)
              (set_local $v8_z)
              (call $$get_free (get_local $$close) (i32.const 0))
              (get_local $v6_x)
              (call $$unslot (get_local $v7_y))
              (call $$unslot (get_local $v8_z))
              (call $$call-close-3)
              (return))
        (export "main" (func $$main)))
```
