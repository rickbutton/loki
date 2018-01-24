(define input-file "test.scm")
(define output-file "bin/a.wat")

(define (list-count-internal l count)
 (if (null? l) count
     (list-count-internal (cdr l) (+ count 1))))
(define (list-count l) (list-count-internal l 0))

(define (last l)
    (cond ((null? l) '())
          ((null? (cdr l)) (car l))
          (else (last (cdr l)))))

(define (index a b)
  (let ((tail (member a (reverse b))))
    (and tail (length (cdr tail)))))

(define (contains? l i)
  (if (null? l) #f
      (or (eq? (car l) i) (contains? (cdr l) i))))

(define (reduce fn base-value lis)
   (if (null? lis)
       base-value
       (fn (car lis)
           (reduce fn base-value (cdr lis)))))

(define (read-input-file)
    (with-input-from-file input-file read-all))

(define (write-output-file output)
    (let ((p (open-output-file output-file)))
        (pretty-print output p)
        (close-output-port p)))

(define type-mask #b11)
(define type-shift 2)

(define fixnum-tag #b00)
(define boolean-tag #b01)
(define null-tag #b10)

(define (genw? tag)
    (lambda (i) (eq (bitwise-and i tag) tag)))

(define (wfixnum? i) (genw? fixnum-tag))
(define (wboolean? i) (genw? boolean-tag))
(define (wnull? i) (genw? null-tag))

(define (fixnum->wfixnum i) (bitwise-ior (arithmetic-shift i type-shift) fixnum-tag))
(define (wfixnum->fixnum i) (arithmetic-shift i (- 0 type-shift)))
(define (boolean->wboolean b) (bitwise-ior (arithmetic-shift (if b 1 0) type-shift) boolean-tag))
(define (wboolean->boolean i) (if (eq (arithmetic-shift i (- 0 type-shift)) 1) #t #f))

(define prim-list '(add sub))
(define (prim-op x) (car x))
(define (prim-param x n) (list-ref x (+ 1 n)))
(define (prim? x)
    (and (list? x) (contains? prim-list (prim-op x))))

(define (let? x)
    (and (list? x) (eq? (prim-op x) 'let)))

(define-type env-entry var val)
(define (empty-env) '())
(define (add-to-env env var val) (cons (make-env-entry var val) env))

(define (let-bindings x) (list-ref x 1))
(define (let-body x) (cdr (cdr x)))
(define (let-binding->var b) (car b))
(define (let-binding->val b) (car (cdr b)))

(define (lambda? x)
    (and (list? x) (eq? (prim-op x) 'lambda)))
(define (lambda-formals x) (list-ref x 1))
(define (lambda-body x) (cdr (cdr x)))

(define (apply-func x) (list-ref x 0))
(define (apply-args x) (cdr x))

(define (define? x)
    (and (list? x) (eq? (prim-op x) 'define)))
(define (define-var) (list-ref x 1))
(define (define-body) (cdr (cdr x)))

(define-type func-def idx env bindings body)
(define (func-def->func-name f)
            (string->symbol (string-append "$" (number->string (func-def-idx f)))))
(define (symbol->func-var s) (string->symbol (string-append "$" (symbol->string s))))

(define stack-top-addr 0)
(define stack-addr 1024)
(define stack-obj-size 4)

(define (inline-comment c)
    (string->symbol (string-append "^^-" c "-^^")))

(define (compile-fixnum x env def-func)
    `(i32.const ,(inline-comment (string-append "fixnum:" (number->string x))) ,(fixnum->wfixnum x)))

(define (compile-boolean x env def-func)
    `(i32.const ,(inline-comment (if x "bool:#t" "bool:#f")) ,(boolean->wboolean x)))

(define (compile-null x env def-func)
    `(i32.const (inline-comment "null") ,null-tag))

(define (compile-prim x env def-func)
    (case (prim-op x)
        ((add) `(i32.add ,(compile-expr (prim-param x 0) env def-func) ,(compile-expr (prim-param x 1) env def-func)))
        ((sub) `(i32.sub ,(compile-expr (prim-param x 0) env def-func) ,(compile-expr (prim-param x 1) env def-func)))
        (else "error, unknown primitive")))


(define (compile-exprs x env def-func) (map (lambda (expr) (compile-expr expr env def-func)) x))

(define (compile-binding-to-push b env def-func)
    `(call $push-to-stack ,(compile-expr (let-binding->val b) env def-func)))

(define (generate-set-ret x env def-func)
    (if (null? (cdr x)) `((set_local $ret ,(compile-expr (car x) env def-func)))
                        (cons (compile-expr (car x) env def-func) (generate-set-ret (cdr x) env def-func))))

(define (compile-let x env def-func)
    (let ((vars (map (lambda (b) (let-binding->var b)) (let-bindings x)))
         (new-env (reduce (lambda (b e) (add-to-env e (let-binding->var b) (compile-expr (let-binding->val b) env def-func))) env (let-bindings x))))
        (let ((func (def-func new-env vars (generate-set-ret (let-body x) new-env def-func))))
            `(block (result i32)
                ,@(map (lambda (b) (compile-binding-to-push b new-env def-func)) (let-bindings x))
                (call ,(func-def->func-name func))))))

(define (compile-lambda x env def-func)
    (let ((formals (lambda-formals x))
          (body (lambda-body x))
          (new-env (reduce (lambda (f e) (add-to-env e f 0)) env (lambda-formals x))))
          (let ((func (def-func new-env formals (generate-set-ret body new-env def-func))))
            `(i32.const ,(inline-comment (string-append "func:" (number->string (func-def-idx func)))) ,(func-def-idx func)))))
        
(define (compile-apply-arg-to-push a env def-func) 
    `(call $push-to-stack ,(compile-expr a env def-func)))

(define (compile-apply x env def-func)
    `(block (result i32)
        ,@(map (lambda (a) (compile-apply-arg-to-push a env def-func)) (apply-args x))
        (call_indirect (type $_func-sig) ,(compile-expr (apply-func x) env def-func))))

(define (compile-var-ref x env def-func)
    `(call $get-stack-obj (i32.const ,(inline-comment (string-append "var-ref:" (symbol->string x))) ,(index x (map (lambda (e) (env-entry-var e)) (reverse env))))))

(define (compile-expr x env def-func) 
    (cond
        ((integer? x) (compile-fixnum x env def-func))
        ((boolean? x) (compile-boolean x env def-func))
        ((null? x) (compile-null x env def-func))
        ((prim? x) (compile-prim x env def-func))
        ((symbol? x) (compile-var-ref x env def-func))
        ((let? x) (compile-let x env def-func))
        ((lambda? x) (compile-lambda x env def-func))
        ((list? x) (compile-apply x env def-func))
        (else '())))

(define (generate-function-defs funcs def-func) 
    (map (lambda (f) 
        `(func 
            ,(func-def->func-name f) (result i32)
            (local $ret i32)
            ,@(func-def-body f)
            (call $pop-all-params (i32.const ,(list-count (func-def-bindings f))))
            (return (get_local $ret)))) funcs))

(define (generate-table-defs funcs)
    `(elem (i32.const 0) ,@(map (lambda (f) (func-def->func-name f)) (reverse funcs))))

(define (compile-program x) 
    (let ((funcs '()))
        (define (def-func env bindings body) 
            (let ((func-idx (list-count funcs)))
                (let ((new-func (make-func-def func-idx env bindings body)))
                    (set! funcs (cons new-func funcs))
                    new-func)))
        (let ((prog (compile-expr x (empty-env) def-func)))
            `(module
                (memory $0 1024)
                (table $0 ,(list-count funcs) anyfunc)
                (type $_func-sig (func (result i32)))
                (func $push-to-stack (param $val i32)
                    (local $stack-top i32)
                    (local $addr i32)

                    (set_local $stack-top
                        (i32.load (i32.const ,stack-top-addr)))
                    
                    (set_local $stack-top
                        (i32.add (get_local $stack-top) (i32.const ,stack-obj-size)))
                    
                    (set_local $addr (i32.add (get_local $stack-top) (i32.const ,stack-addr)))
                    
                    (i32.store (get_local $addr) (get_local $val))

                    (i32.store (i32.const ,stack-top-addr) (get_local $stack-top)))
                    
                
                ; needs to accept an offset of obj, not addr
                (func $get-stack-obj (param $offset i32) (result i32)
                    (local $addr i32)
                    (set_local $addr (i32.add (i32.const ,stack-addr) 
                                              ;(i32.mul (i32.const ,stack-obj-size) (get_local $offset))))
                                              (i32.mul (i32.const ,stack-obj-size) (i32.add (get_local $offset) (i32.const 1)))))
                    (return (i32.load (get_local $addr))))
                
                (func $pop-all-params (param $n i32)
                    (local $stack-top i32)
                    
                    (set_local $stack-top (i32.load (i32.const ,stack-top-addr)))
                    (set_local $stack-top (i32.sub (get_local $stack-top) (i32.mul (get_local $n) (i32.const ,stack-obj-size))))
                    (i32.store (i32.const ,stack-top-addr) (get_local $stack-top)))

                (func $entry (result i32) ,prog)
                (func $main (result i32) (call $entry))
                ,@(generate-function-defs funcs def-func)
                ,(generate-table-defs funcs)
                (export "main" (func $main)))
            )))

(let ((prog (compile-program (car (read-input-file)))))
    (pretty-print prog)
    (write-output-file prog))