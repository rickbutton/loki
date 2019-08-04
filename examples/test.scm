(define fib (lambda (n)
    (if ($$prim$le_s n 2)
        1
        ($$prim$add 
            (fib ($$prim$sub n 1)) 
            (fib ($$prim$sub n 2))))))

(fib 25)
