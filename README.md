# schwasm

### A Scheme to WebAssembly Compiler

This supports almost zero percent of any scheme standard or spec. I implemented this as a way to practice compiler writing (specifically closure conversion), and to learn WebAssembly. I'm hoping to eventually make this R7RS small compliant at least. No promises.

Take caution, because this probably doesn't work on your machine without tweaks.


### How to run

You will need [chibi scheme](https://github.com/ashinn/chibi-scheme/), [node](https://nodejs.org), and [wabt](https://github.com/WebAssembly/wabt) on your path.

```
make run
```

### TODO

- more heap allocated values (strings, runtime symbols, etc)
- enough standard library to run the compiler 
- modules (via define-libray)
- test suite
- a real parser (currently using the host's parser, since I don't have strings)
- FFI with JavaScript

## Plans?

- native layer (garbage collector, etc) in Rust

### Example

This example may be out of date. No Promises!

input:

```scheme
(define make-add (lambda (x) (lambda (y) (add x y))))
(define add-to-12 (make-add 12))

(add-to-12 45)
```

output:

```scheme
(module (type $$close0 (func (param i32) (result i32)))
        (type $$close1 (func (param i32) (param i32) (result i32)))
        (type $$close2
              (func (param i32) (param i32) (param i32) (result i32)))
        (memory $0 16384)
        (export "memory" (memory 0))
        (table 3 anyfunc)
        (elem (i32.const 0) $$fentry $$f1 $$f2)
        (func $$main (result i32) (call $$fentry))
        (func $$alloc
              (param $size i32)
              (result i32)
              (local $ptr i32)
              (i32.const 16380)
              (i32.load)
              (set_local $ptr)
              (i32.const 16380)
              (get_local $ptr)
              (get_local $size)
              (i32.const 4)
              (i32.mul)
              (i32.add)
              (i32.store)
              (get_local $ptr))
        (func $$alloc-slot
              (param $val i32)
              (result i32)
              (local $ptr i32)
              (i32.const 1)
              (call $$alloc)
              (tee_local $ptr)
              (get_local $val)
              (i32.store)
              (get_local $ptr))
        (func $$alloc-pair
              (param $car i32)
              (param $cdr i32)
              (result i32)
              (local $ptr i32)
              (i32.const 2)
              (call $$alloc)
              (tee_local $ptr)
              (get_local $car)
              (i32.store)
              (get_local $ptr)
              (i32.const 4)
              (i32.add)
              (get_local $cdr)
              (i32.store)
              (get_local $ptr)
              (i32.const 3)
              (i32.shl)
              (i32.const 1)
              (i32.or))
        (func $$alloc-close
              (param $findex i32)
              (param $size i32)
              (result i32)
              (local $ptr i32)
              (get_local $size)
              (i32.const 1)
              (i32.add)
              (call $$alloc)
              (tee_local $ptr)
              (get_local $findex)
              (i32.store)
              (get_local $ptr)
              (i32.const 3)
              (i32.shl)
              (i32.const 3)
              (i32.or))
        (func $$store-free
              (param $ptr i32)
              (param $index i32)
              (param $val i32)
              (result i32)
              (get_local $ptr)
              (i32.const 3)
              (i32.shr_u)
              (i32.const 4)
              (i32.add)
              (get_local $index)
              (i32.const 4)
              (i32.mul)
              (i32.add)
              (get_local $val)
              (i32.store)
              (get_local $ptr))
        (func $$get-free
              (param $ptr i32)
              (param $index i32)
              (result i32)
              (get_local $ptr)
              (i32.const 3)
              (i32.shr_u)
              (i32.const 4)
              (i32.add)
              (get_local $index)
              (i32.const 4)
              (i32.mul)
              (i32.add)
              (i32.load))
        (func $$get-close-func-index
              (param $close i32)
              (result i32)
              (get_local $close)
              (i32.const 3)
              (i32.shr_u)
              (i32.load))
        (func $$call-close-0
              (param $$close i32)
              (result i32)
              (local $$idx i32)
              (get_local $$close)
              (get_local $$close)
              (call $$get-close-func-index)
              (call_indirect (type $$close0)))
        (func $$call-close-1
              (param $$close i32)
              (param $$0 i32)
              (result i32)
              (local $$idx i32)
              (get_local $$0)
              (get_local $$close)
              (get_local $$close)
              (call $$get-close-func-index)
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
              (call $$get-close-func-index)
              (call_indirect (type $$close2)))
        (func $$unslot
              (param $slot i32)
              (result i32)
              (get_local $slot)
              (i32.load))
        (func $$car
              (param $pair i32)
              (result i32)
              (get_local $pair)
              (i32.const 3)
              (i32.shr_u)
              (i32.load))
        (func $$cdr
              (param $pair i32)
              (result i32)
              (get_local $pair)
              (i32.const 3)
              (i32.shr_u)
              (i32.const 4)
              (i32.add)
              (i32.load))
        (func $$fentry
              (result i32)
              (local $$v3 i32)
              (local $$v4 i32)
              (call $$alloc-close (i32.const 1) (i32.const 0))
              (call $$alloc-slot)
              (set_local $$v3)
              (call $$unslot (get_local $$v3))
              (i32.const 48)
              (call $$call-close-1)
              (call $$alloc-slot)
              (set_local $$v4)
              (call $$unslot (get_local $$v4))
              (i32.const 180)
              (call $$call-close-1)
              (return))
        (func $$f1
              (param $$v1 i32)
              (param $$close i32)
              (result i32)
              (call $$alloc-close (i32.const 2) (i32.const 1))
              (i32.const 0)
              (get_local $$v1)
              (call $$store-free)
              (return))
        (func $$f2
              (param $$v2 i32)
              (param $$close i32)
              (result i32)
              (call $$get-free (get_local $$close) (i32.const 0))
              (get_local $$v2)
              (i32.add)
              (return))
        (export "main" (func $$main)))
```