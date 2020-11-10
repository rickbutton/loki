(define-library (lang stack)
(import (scheme base))
(import (loki match))
(import (loki util))
(begin

; a stack::function is a record containing
; - a "chunk" (a vector of stack bytecodes)
; - "constants" (a vector of constants that are referred to by the bytecode)
; - a "localsize" (a number representing the number of locals in the function)

; bytecode instructions
; constant(idx)
; get-local(idx)
; set-local(idx)
; get-upvalue(idx)
; set-upvalue(idx)
; close-upvalue(idx)
; get-global(idx)
; set-global(idx)
; jump(idx)
; jumpf(idx)
; call(argc)
; closure(func)

(define-record-type <stack::function>
  (stack::function chunk constants)
  stack::function?
  (chunk stack::function-chunk)
  (constants stack::function-constants))

(define-record-type <stack::constant>
  (stack::constant value)
  stack::constant?
  (value stack::constant-value))

(define-record-type <stack::get-local>
  (stack::get-local idx)
  stack::get-local?
  (idx stack::get-local-idx))
(define-record-type <stack::set-local>
  (stack::get-local idx)
  stack::get-local?
  (idx stack::get-local-idx))
(define-record-type <stack::get-upvalue>
  (stack::get-upvalue idx)
  stack::get-upvalue?
  (idx stack::get-upvalue-idx))
(define-record-type <stack::set-upvalue>
  (stack::get-upvalue idx)
  stack::get-upvalue?
  (idx stack::get-upvalue-idx))
(define-record-type <stack::close-upvalue>
  (stack::get-upvalue idx)
  stack::get-upvalue?
  (idx stack::get-upvalue-idx))
(define-record-type <stack::get-global>
  (stack::get-global idx)
  stack::get-global?
  (idx stack::get-global-idx))
(define-record-type <stack::set-global>
  (stack::get-global idx)
  stack::get-global?
  (idx stack::get-global-idx))


(define-record-type <stack::pop>
  (stack::pop)
  stack::pop?)

(define-record-type <stack::jump>
  (stack::jump idx)
  stack::jump?
  (idx stack::jump-idx))
(define-record-type <stack::jumpf>
  (stack::jumpf idx)
  stack::jumpf?
  (idx stack::jumpf-idx))

(define-record-type <stack::call>
  (stack::call argc)
  stack::call?
  (argc stack::call-argc))
(define-record-type <stack::call-tail>
  (stack::call argc)
  stack::call?
  (argc stack::call-argc))

))
