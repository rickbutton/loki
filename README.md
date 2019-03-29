# schwasm

### A Scheme to WebAssembly Compiler

This supports almost zero percent of any scheme standard or spec. I implemented this as a way to practice compiler writing (specifically closure conversion), and to learn WebAssembly. I'm hoping to eventually make this R5RS-compliant at least. No promises.

Take caution, because this probably doesn't work on your machine without tweaks.


### How to run

You will need [Guile Scheme](https://www.gnu.org/software/guile/), [node](https://nodejs.org), and [wabt](https://github.com/WebAssembly/wabt) on your path.

```
make run
```

### TODO

- free variable capture (lambdas are converted to closures and properly lifted, but frees are not handled)
- a heap + heap allocated values (pairs, strings, runtime symbols, etc)
- a real parser (currently using the host's parser, since I don't have strings)
- FFI with JavaScript

### Example

This example may be out of date. No Promises!

input:

```scheme
(define make (lambda () (lambda (x y) (sub x y))))
(define - (make))

(- 2 10)
```

output:

```scheme
(module
  (type $funcsig$0 (func (result i32)))
  (type $funcsig$1 (func (param i32) (result i32)))
  (type $funcsig$2
        (func (param i32) (param i32) (result i32)))
  (memory $0 1024)
  (table 3 anyfunc)
  (elem (i32.const 0) $$fentry $$f27 $$f28)
  (func $$fentry
        (result i32)
        (local $$v25 i32)
        (local $$v26 i32)
        (i32.const 1)
        (set_local $$v25)
        (get_local $$v25)
        (call_indirect (type $funcsig$0))
        (set_local $$v26)
        (i32.const 8)
        (i32.const 40)
        (get_local $$v26)
        (call_indirect (type $funcsig$2))
        (return))
  (func $$f27 (result i32) (i32.const 2) (return))
  (func $$f28
        (param $$v23 i32)
        (param $$v24 i32)
        (result i32)
        (get_local $$v23)
        (get_local $$v24)
        (i32.sub)
        (return))
  (func $$main (result i32) (call $$fentry))
  (export "main" (func $$main)))

```