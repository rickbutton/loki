(define str ($$prim$concat-string "this is a test " "of string concat! 😀"))

(define fib (lambda (n)
      (if ($$prim$le_s n 2)
        1
        ($$prim$add 
            (fib ($$prim$sub n 1)) 
            (fib ($$prim$sub n 2))))))

(call/cc (lambda (k) (k ($$prim$cons 
                          #(1 2 "three" ($$prim$add 2 2) #t #f)
                          ($$prim$cons
                              'symbol 
                              ($$prim$cons 
                                    (fib 25)
                                    ($$prim$cons
                                          str
                                          '())))))))
