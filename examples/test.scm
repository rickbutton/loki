(define (+ a b c) (%%prim%add (%%prim%add a b) c))

(define (test x)
    (define y (%%prim%add x 2))
    (define z (%%prim%add x 3))
    (+ x y z))

(%%prim%cons (test 10) "😀 schwasm!")
