(let ((add1 (lambda (x) (add x 1)))
      (sub2 (lambda (x) (sub x 5))))
    (sub2 (add1 2)))