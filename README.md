# loki

### A Scheme to WebAssembly Compiler

This supports almost zero percent of any scheme standard or spec. I'm hoping to eventually make this R7RS small compliant at least. No promises.

Take caution, because this probably doesn't work on your machine without tweaks.

#### Why is it called loki?

In Norse mythology, Loki is a mischievous trickster. Attemting to run scheme in the browser is the epitome of mischief.

### How to run

You will need [chibi scheme](https://github.com/ashinn/chibi-scheme/), [node](https://nodejs.org), and [wabt](https://github.com/WebAssembly/wabt) on your path.

```
make example
```

### Things that work?

- values
    - numbers
    - chars
    - booleans
    - null
    - pairs
    - strings
    - symbols
- operations
    - simple math operations
    - pair operations (cons/car/cdr)
    - few conditional operators
        - le_s
- syntax
    - (begin exprs ...)
    - (quote expr) **no quasi/unquote**
    - (if test t f) **(no single arm if)**
    - (define var expr) **(no `(define (var formals ...) body)`)**
    - (set! var expr)
    - (lambda (formals ...) body)
    - (call/cc (lambda (k) (k ...)))

A CPS transformation is done to the source, which enables full tail call optimization and first class continuations. (preliminary call/cc support has landed!)

### TODO

- any level of optimization (CPS-induced code explosion is real!)
- self hosting
- modules (via define-library)
- test suite for back end
- easy FFI with JavaScript
- WASI?

### Example

This example may be out of date. No Promises!

input:

```scheme
(define str ($$prim$concat-string "this is a test " "of string concat! 😀"))

(define fib (lambda (n)
      (if ($$prim$le_s n 2)
        1
        ($$prim$add 
            (fib ($$prim$sub n 1)) 
            (fib ($$prim$sub n 2))))))

(call/cc (lambda (k) (k ($$prim$cons 
                        'symbol 
                        ($$prim$cons 
                              (fib 25)
                              ($$prim$cons
                                    str
                                    '()))))))
```

output:

```wat
(module (type $$fun$0 (func (param anyref) (result anyref)))
        (type $$fun$1 (func (param anyref) (param anyref) (result anyref)))
        (type $$fun$2
              (func (param anyref)
                    (param anyref)
                    (param anyref)
                    (result anyref)))
        (type $$fun$3
              (func (param anyref)
                    (param anyref)
                    (param anyref)
                    (param anyref)
                    (result anyref)))
        (global $$iv$true (import "env" "$$iv$true") anyref)
        (global $$iv$false (import "env" "$$iv$false") anyref)
        (global $$iv$null (import "env" "$$iv$null") anyref)
        (global $$iv$void (import "env" "$$iv$void") anyref)
        (import "env" "memory" (memory 0))
        (import "env"
                "$$prim$make-number"
                (func $$prim$make-number (param i32) (result anyref)))
        (import "env"
                "$$prim$add"
                (func $$prim$add (param anyref anyref) (result anyref)))
        (import "env"
                "$$prim$sub"
                (func $$prim$sub (param anyref anyref) (result anyref)))
        (import "env"
                "$$prim$le_s"
                (func $$prim$le_s (param anyref anyref) (result anyref)))
        (import "env"
                "$$prim$test"
                (func $$prim$test (param anyref) (result i32)))
        (import "env"
                "$$prim$make-char"
                (func $$prim$make-char (param i32) (result anyref)))
        (import "env"
                "$$prim$make-slot"
                (func $$prim$make-slot (result anyref)))
        (import "env"
                "$$prim$set-slot"
                (func $$prim$set-slot (param anyref anyref)))
        (import "env"
                "$$prim$get-slot"
                (func $$prim$get-slot (param anyref) (result anyref)))
        (import "env"
                "$$prim$cons"
                (func $$prim$cons (param anyref anyref) (result anyref)))
        (import "env"
                "$$prim$car"
                (func $$prim$car (param anyref) (result anyref)))
        (import "env"
                "$$prim$cdr"
                (func $$prim$cdr (param anyref) (result anyref)))
        (import "env"
                "$$prim$concat-string"
                (func $$prim$concat-string
                      (param anyref anyref)
                      (result anyref)))
        (import "env"
                "$$prim$make-closure"
                (func $$prim$make-closure (param i32 i32 i32) (result anyref)))
        (import "env"
                "$$prim$set-free"
                (func $$prim$set-free
                      (param anyref i32 anyref)
                      (result anyref)))
        (import "env"
                "$$prim$get-free"
                (func $$prim$get-free (param anyref i32) (result anyref)))
        (import "env"
                "$$prim$get-closure-findex"
                (func $$prim$get-closure-findex
                      (param anyref i32)
                      (result i32)))
        (import "env"
                "$$prim$make-string"
                (func $$prim$make-string (param i32 i32) (result anyref)))
        (import "env"
                "$$prim$make-symbol"
                (func $$prim$make-symbol (param i32 i32) (result anyref)))
        (data (i32.const 0) "this is a test of string concat! 😀symbol")
        (table $$functable 18 anyfunc)
        (elem (i32.const 0)
              $$finit
              $$fentry
              $$f15
              $$f14
              $$f13
              $$f12
              $$f11
              $$f10
              $$f9
              $$f8
              $$f7
              $$f6
              $$f5
              $$f4
              $$f3
              $$f2
              $$f1
              $$fexit)
        (table $$rodatatable 3 anyref)
        (func $$main (result anyref) (call $$fentry))
        (func $$finit
              (result anyref)
              (local $$tmp anyref)
              (table.set $$rodatatable
                         (i32.const 0)
                         (call $$prim$make-string
                               (;val=this is a test ;)
                               (i32.const 0)
                               (i32.const 15)))
              (table.set $$rodatatable
                         (i32.const 1)
                         (call $$prim$make-string
                               (;val=of string concat! 😀;)
                               (i32.const 15)
                               (i32.const 22)))
              (table.set $$rodatatable
                         (i32.const 2)
                         (call $$prim$make-symbol
                               (;val=symbol;)
                               (i32.const 37)
                               (i32.const 6)))
              (call $$prim$make-number (i32.const 0)))
        (func $$fentry
              (result anyref)
              (local $v2_fib anyref)
              (local $v1_str anyref)
              (local $$tmp anyref)
              (set_local $v2_fib (call $$prim$make-slot))
              (set_local $v1_str (call $$prim$make-slot))
              (table.get $$rodatatable (i32.const 0))
              (table.get $$rodatatable (i32.const 1))
              (call $$prim$concat-string)
              (call $$prim$make-closure
                    (;func=$$f15;)
                    (i32.const 2)
                    (i32.const 1)
                    (i32.const 2))
              (i32.const 0)
              (get_local $v2_fib)
              (call $$prim$set-free)
              (i32.const 1)
              (get_local $v1_str)
              (call $$prim$set-free)
              (tee_local $$tmp)
              (get_local $$tmp)
              (call $$prim$get-closure-findex (i32.const 1))
              (return_call_indirect (type $$fun$1) $$functable))
        (func $$f15
              (param $rv_1 anyref)
              (param $$close anyref)
              (result anyref)
              (local $$tmp anyref)
              (get_local $rv_1)
              (call $$prim$set-slot
                    (call $$prim$get-free (get_local $$close) (i32.const 1)))
              (call $$prim$make-closure
                    (;func=$$f14;)
                    (i32.const 3)
                    (i32.const 2)
                    (i32.const 1))
              (i32.const 0)
              (call $$prim$get-free (get_local $$close) (i32.const 0))
              (call $$prim$set-free)
              (call $$prim$set-slot
                    (call $$prim$get-free (get_local $$close) (i32.const 0)))
              (call $$prim$make-closure
                    (;func=$$f5;)
                    (i32.const 12)
                    (i32.const 2)
                    (i32.const 2))
              (i32.const 0)
              (call $$prim$get-free (get_local $$close) (i32.const 0))
              (call $$prim$set-free)
              (i32.const 1)
              (call $$prim$get-free (get_local $$close) (i32.const 1))
              (call $$prim$set-free)
              (call $$prim$make-closure
                    (;func=$$fexit;)
                    (i32.const 17)
                    (i32.const 1)
                    (i32.const 0))
              (call $$prim$make-closure
                    (;func=$$f7;)
                    (i32.const 10)
                    (i32.const 2)
                    (i32.const 0))
              (tee_local $$tmp)
              (get_local $$tmp)
              (call $$prim$get-closure-findex (i32.const 2))
              (return_call_indirect (type $$fun$2) $$functable))
        (func $$f14
              (param $v3_n anyref)
              (param $k_1 anyref)
              (param $$close anyref)
              (result anyref)
              (local $$tmp anyref)
              (get_local $k_1)
              (call $$prim$make-closure
                    (;func=$$f13;)
                    (i32.const 4)
                    (i32.const 1)
                    (i32.const 2))
              (i32.const 0)
              (get_local $v3_n)
              (call $$prim$set-free)
              (i32.const 1)
              (call $$prim$get-free (get_local $$close) (i32.const 0))
              (call $$prim$set-free)
              (tee_local $$tmp)
              (get_local $$tmp)
              (call $$prim$get-closure-findex (i32.const 1))
              (return_call_indirect (type $$fun$1) $$functable))
        (func $$f13
              (param $k_2 anyref)
              (param $$close anyref)
              (result anyref)
              (local $$tmp anyref)
              (call $$prim$get-slot
                    (call $$prim$get-free (get_local $$close) (i32.const 0)))
              (call $$prim$make-number (i32.const 2))
              (call $$prim$le_s)
              (call $$prim$make-closure
                    (;func=$$f12;)
                    (i32.const 5)
                    (i32.const 1)
                    (i32.const 3))
              (i32.const 0)
              (call $$prim$get-free (get_local $$close) (i32.const 0))
              (call $$prim$set-free)
              (i32.const 1)
              (call $$prim$get-free (get_local $$close) (i32.const 1))
              (call $$prim$set-free)
              (i32.const 2)
              (get_local $k_2)
              (call $$prim$set-free)
              (tee_local $$tmp)
              (get_local $$tmp)
              (call $$prim$get-closure-findex (i32.const 1))
              (return_call_indirect (type $$fun$1) $$functable))
        (func $$f12
              (param $rv_2 anyref)
              (param $$close anyref)
              (result anyref)
              (local $$tmp anyref)
              (get_local $rv_2)
              (call $$prim$test)
              (if (result anyref)
                  (then (call $$prim$make-number (i32.const 1))
                        (call $$prim$get-slot
                              (call $$prim$get-free
                                    (get_local $$close)
                                    (i32.const 2)))
                        (tee_local $$tmp)
                        (get_local $$tmp)
                        (call $$prim$get-closure-findex (i32.const 1))
                        (return_call_indirect (type $$fun$1) $$functable))
                  (else
                   (call $$prim$get-slot
                         (call $$prim$get-free
                               (get_local $$close)
                               (i32.const 0)))
                   (call $$prim$make-number (i32.const 1))
                   (call $$prim$sub)
                   (call $$prim$make-closure
                         (;func=$$f11;)
                         (i32.const 6)
                         (i32.const 1)
                         (i32.const 3))
                   (i32.const 0)
                   (call $$prim$get-free (get_local $$close) (i32.const 0))
                   (call $$prim$set-free)
                   (i32.const 1)
                   (call $$prim$get-free (get_local $$close) (i32.const 1))
                   (call $$prim$set-free)
                   (i32.const 2)
                   (call $$prim$get-free (get_local $$close) (i32.const 2))
                   (call $$prim$set-free)
                   (tee_local $$tmp)
                   (get_local $$tmp)
                   (call $$prim$get-closure-findex (i32.const 1))
                   (return_call_indirect (type $$fun$1) $$functable))))
        (func $$f11
              (param $rv_6 anyref)
              (param $$close anyref)
              (result anyref)
              (local $$tmp anyref)
              (get_local $rv_6)
              (call $$prim$make-closure
                    (;func=$$f10;)
                    (i32.const 7)
                    (i32.const 1)
                    (i32.const 3))
              (i32.const 0)
              (call $$prim$get-free (get_local $$close) (i32.const 0))
              (call $$prim$set-free)
              (i32.const 1)
              (call $$prim$get-free (get_local $$close) (i32.const 1))
              (call $$prim$set-free)
              (i32.const 2)
              (call $$prim$get-free (get_local $$close) (i32.const 2))
              (call $$prim$set-free)
              (call $$prim$get-slot
                    (call $$prim$get-free (get_local $$close) (i32.const 1)))
              (tee_local $$tmp)
              (get_local $$tmp)
              (call $$prim$get-closure-findex (i32.const 2))
              (return_call_indirect (type $$fun$2) $$functable))
        (func $$f10
              (param $rv_3 anyref)
              (param $$close anyref)
              (result anyref)
              (local $$tmp anyref)
              (call $$prim$get-slot
                    (call $$prim$get-free (get_local $$close) (i32.const 0)))
              (call $$prim$make-number (i32.const 2))
              (call $$prim$sub)
              (call $$prim$make-closure
                    (;func=$$f9;)
                    (i32.const 8)
                    (i32.const 1)
                    (i32.const 3))
              (i32.const 0)
              (call $$prim$get-free (get_local $$close) (i32.const 1))
              (call $$prim$set-free)
              (i32.const 1)
              (get_local $rv_3)
              (call $$prim$set-free)
              (i32.const 2)
              (call $$prim$get-free (get_local $$close) (i32.const 2))
              (call $$prim$set-free)
              (tee_local $$tmp)
              (get_local $$tmp)
              (call $$prim$get-closure-findex (i32.const 1))
              (return_call_indirect (type $$fun$1) $$functable))
        (func $$f9
              (param $rv_5 anyref)
              (param $$close anyref)
              (result anyref)
              (local $$tmp anyref)
              (get_local $rv_5)
              (call $$prim$make-closure
                    (;func=$$f8;)
                    (i32.const 9)
                    (i32.const 1)
                    (i32.const 2))
              (i32.const 0)
              (call $$prim$get-free (get_local $$close) (i32.const 1))
              (call $$prim$set-free)
              (i32.const 1)
              (call $$prim$get-free (get_local $$close) (i32.const 2))
              (call $$prim$set-free)
              (call $$prim$get-slot
                    (call $$prim$get-free (get_local $$close) (i32.const 0)))
              (tee_local $$tmp)
              (get_local $$tmp)
              (call $$prim$get-closure-findex (i32.const 2))
              (return_call_indirect (type $$fun$2) $$functable))
        (func $$f8
              (param $rv_4 anyref)
              (param $$close anyref)
              (result anyref)
              (local $$tmp anyref)
              (call $$prim$get-slot
                    (call $$prim$get-free (get_local $$close) (i32.const 0)))
              (get_local $rv_4)
              (call $$prim$add)
              (call $$prim$get-slot
                    (call $$prim$get-free (get_local $$close) (i32.const 1)))
              (tee_local $$tmp)
              (get_local $$tmp)
              (call $$prim$get-closure-findex (i32.const 1))
              (return_call_indirect (type $$fun$1) $$functable))
        (func $$f7
              (param $f_1 anyref)
              (param $cc_1 anyref)
              (param $$close anyref)
              (result anyref)
              (local $$tmp anyref)
              (call $$prim$make-closure
                    (;func=$$f6;)
                    (i32.const 11)
                    (i32.const 2)
                    (i32.const 1))
              (i32.const 0)
              (get_local $cc_1)
              (call $$prim$set-free)
              (get_local $cc_1)
              (get_local $f_1)
              (tee_local $$tmp)
              (get_local $$tmp)
              (call $$prim$get-closure-findex (i32.const 2))
              (return_call_indirect (type $$fun$2) $$functable))
        (func $$f6
              (param $x_1 anyref)
              (param $__1 anyref)
              (param $$close anyref)
              (result anyref)
              (local $$tmp anyref)
              (get_local $x_1)
              (call $$prim$get-slot
                    (call $$prim$get-free (get_local $$close) (i32.const 0)))
              (tee_local $$tmp)
              (get_local $$tmp)
              (call $$prim$get-closure-findex (i32.const 1))
              (return_call_indirect (type $$fun$1) $$functable))
        (func $$f5
              (param $v4_k anyref)
              (param $k_3 anyref)
              (param $$close anyref)
              (result anyref)
              (local $$tmp anyref)
              (call $$prim$make-number (i32.const 25))
              (call $$prim$make-closure
                    (;func=$$f4;)
                    (i32.const 13)
                    (i32.const 1)
                    (i32.const 3))
              (i32.const 0)
              (call $$prim$get-free (get_local $$close) (i32.const 1))
              (call $$prim$set-free)
              (i32.const 1)
              (get_local $v4_k)
              (call $$prim$set-free)
              (i32.const 2)
              (get_local $k_3)
              (call $$prim$set-free)
              (call $$prim$get-slot
                    (call $$prim$get-free (get_local $$close) (i32.const 0)))
              (tee_local $$tmp)
              (get_local $$tmp)
              (call $$prim$get-closure-findex (i32.const 2))
              (return_call_indirect (type $$fun$2) $$functable))
        (func $$f4
              (param $rv_9 anyref)
              (param $$close anyref)
              (result anyref)
              (local $$tmp anyref)
              (call $$prim$get-slot
                    (call $$prim$get-free (get_local $$close) (i32.const 0)))
              (get_global $$iv$null)
              (call $$prim$cons)
              (call $$prim$make-closure
                    (;func=$$f3;)
                    (i32.const 14)
                    (i32.const 1)
                    (i32.const 3))
              (i32.const 0)
              (get_local $rv_9)
              (call $$prim$set-free)
              (i32.const 1)
              (call $$prim$get-free (get_local $$close) (i32.const 1))
              (call $$prim$set-free)
              (i32.const 2)
              (call $$prim$get-free (get_local $$close) (i32.const 2))
              (call $$prim$set-free)
              (tee_local $$tmp)
              (get_local $$tmp)
              (call $$prim$get-closure-findex (i32.const 1))
              (return_call_indirect (type $$fun$1) $$functable))
        (func $$f3
              (param $rv_10 anyref)
              (param $$close anyref)
              (result anyref)
              (local $$tmp anyref)
              (call $$prim$get-slot
                    (call $$prim$get-free (get_local $$close) (i32.const 0)))
              (get_local $rv_10)
              (call $$prim$cons)
              (call $$prim$make-closure
                    (;func=$$f2;)
                    (i32.const 15)
                    (i32.const 1)
                    (i32.const 2))
              (i32.const 0)
              (call $$prim$get-free (get_local $$close) (i32.const 1))
              (call $$prim$set-free)
              (i32.const 1)
              (call $$prim$get-free (get_local $$close) (i32.const 2))
              (call $$prim$set-free)
              (tee_local $$tmp)
              (get_local $$tmp)
              (call $$prim$get-closure-findex (i32.const 1))
              (return_call_indirect (type $$fun$1) $$functable))
        (func $$f2
              (param $rv_8 anyref)
              (param $$close anyref)
              (result anyref)
              (local $$tmp anyref)
              (table.get $$rodatatable (i32.const 2))
              (get_local $rv_8)
              (call $$prim$cons)
              (call $$prim$make-closure
                    (;func=$$f1;)
                    (i32.const 16)
                    (i32.const 1)
                    (i32.const 2))
              (i32.const 0)
              (call $$prim$get-free (get_local $$close) (i32.const 0))
              (call $$prim$set-free)
              (i32.const 1)
              (call $$prim$get-free (get_local $$close) (i32.const 1))
              (call $$prim$set-free)
              (tee_local $$tmp)
              (get_local $$tmp)
              (call $$prim$get-closure-findex (i32.const 1))
              (return_call_indirect (type $$fun$1) $$functable))
        (func $$f1
              (param $rv_7 anyref)
              (param $$close anyref)
              (result anyref)
              (local $$tmp anyref)
              (get_local $rv_7)
              (call $$prim$get-slot
                    (call $$prim$get-free (get_local $$close) (i32.const 1)))
              (call $$prim$get-slot
                    (call $$prim$get-free (get_local $$close) (i32.const 0)))
              (tee_local $$tmp)
              (get_local $$tmp)
              (call $$prim$get-closure-findex (i32.const 2))
              (return_call_indirect (type $$fun$2) $$functable))
        (func $$fexit
              (param $v anyref)
              (param $$close anyref)
              (result anyref)
              (local $$tmp anyref)
              (get_local $v))
        (export "main" (func $$main))
        (export "init" (func $$finit)))```
