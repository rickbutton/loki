(define-library (loki core list)
  (import (loki core primitives))
  (import (loki core apply))
  (import (loki core let))
  (import (loki core bool))
  (import (loki core exception))
  (import (loki core intrinsics))
  (import (rename (loki core intrinsics) (%cons  cons)
                  (%pair? pair?)
                  (%null? null?)
                  (%list? list?)
                  (%length length)
                  (%car   car)
                  (%cdr   cdr)
                  (%set-car! set-car!)
                  (%set-cdr! set-cdr!)))
  (export
   cons pair? null? list? car cdr
   caar cadr cdar cddr
   set-car! set-cdr!
   make-list list-copy
   member memv memq
   assoc assv assq
   for-all for-each
   length list list-tail list-ref list-set!
   reverse append map
   
   boolean=? symbol=?)
  (begin
   
   (define (caar obj) (car (car obj)))
   (define (cadr obj) (car (cdr obj)))
   (define (cdar obj) (cdr (car obj)))
   (define (cddr obj) (cdr (cdr obj)))
   
   (define (make-list n . o)
     (let ((default (if (pair? o) (car o))))
       (let lp ((n n) (res '()))
         (if (%lte n 0) res (lp (%sub n 1) (cons default res))))))
   
   (define (list-copy ls)
     (let lp ((ls ls) (res '()))
       (if (pair? ls)
           (lp (cdr ls) (cons (car ls) res))
         (append (reverse res) ls))))
   
   (define (member obj ls . o)
     (let ((eq (if (pair? o) (car o) equal?)))
       (let lp ((ls ls))
         (and (pair? ls) (if (eq obj (car ls)) ls (lp (cdr ls)))))))
   (define (memv obj ls) (member obj ls eqv?))
   (define (memq obj ls) (member obj ls eq?))
   
   (define (assoc obj ls . o)
     (let ((eq (if (pair? o) (car o) equal?)))
       (let assoc ((ls ls))
         (if (null? ls)
             #f
           (if (eq obj (caar ls))
               (car ls)
             (assoc (cdr ls)))))))
   (define (assv obj ls) (assoc obj ls eqv?))
   (define (assq obj ls) (assoc obj ls eq?))
   
   (define (for-all proc l . ls)
     (or (null? l)
         (and (apply proc (car l) (map car ls))
              (apply for-all proc (cdr l) (map cdr ls)))))
   
   (define (for-each f ls . lol)
     (define (for1 f ls) (if (pair? ls) (begin (f (car ls)) (for1 f (cdr ls)))))
     (if (null? lol) (for1 f ls) (begin (apply map f ls lol) (if #f #f))))
   
   (define (list . args) args)
   
   (define (list-tail ls k)
     (if (eq? k 0)
         ls
       (list-tail (cdr ls) (%sub k 1))))
   
   (define (list-ref ls k) (car (list-tail ls k)))
   
   (define (list-set! ls k x)
     (if (null? ls)
         (error "invalid list index")
       (if (%number-eq k 0)
           (set-car! ls x)
         (list-set! (cdr ls) (%sub k 1) x))))
   
   (define (reverse ls)
     (let lp ((ls ls) (res '()))
       (if (pair? ls)
           (lp (cdr ls) (cons (car ls) res))
         res)))
   (define (append . args)
     (let loop ((ls '()) (args args))
       (if (null? args)
           ls
         (let g ((ls ls))
           (if (null? ls)
               (loop (car args) (cdr args))
             (cons (car ls) (g (cdr ls))))))))
   (define (map proc ls . lol)
     (define (map1 proc ls res)
       (if (pair? ls)
           (map1 proc (cdr ls) (cons (proc (car ls)) res))
         (reverse res)))
     (define (mapn proc lol res)
       (if (every pair? lol)
           (mapn proc
                 (map1 cdr lol '())
                 (cons (apply proc (map1 car lol '())) res))
         (reverse res)))
     (if (null? lol)
         (map1 proc ls '())
       (mapn proc (cons ls lol) '())))
   
   (define (any pred ls . lol)
     (define (any1 pred ls)
       (if (pair? (cdr ls))
           ((lambda (x) (if x x (any1 pred (cdr ls)))) (pred (car ls)))
         (pred (car ls))))
     (define (anyn pred lol)
       (if (every pair? lol)
           ((lambda (x) (if x x (anyn pred (map cdr lol))))
            (apply pred (map car lol)))
         #f))
     (if (null? lol) (if (pair? ls) (any1 pred ls) #f) (anyn pred (cons ls lol))))
   
   (define (every pred ls . lol)
     (define (every1 pred ls)
       (if (null? (cdr ls))
           (pred (car ls))
         (if (pred (car ls)) (every1 pred (cdr ls)) #f)))
     (if (null? lol)
         (if (pair? ls) (every1 pred ls) #t)
       (not (apply any (lambda xs (not (apply pred xs))) ls lol))))
   
   (define (boolean=? x y . o)
     (if (not (boolean? x))
         #f
       (and (eq? x y) (if (pair? o) (apply boolean=? y o) #t))))
   
   (define (symbol=? x y . o)
     (if (not (symbol? x))
         #f
       (and (eq? x y) (if (pair? o) (apply symbol=? y o) #t))))
   ))
