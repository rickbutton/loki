(define-library
  (loki util)
  (import (scheme base))
  (import (scheme eval))
  (import (scheme write))
  (cond-expand
   (gauche (import (gauche base))))
  (export
   pretty
   map-vector
   fold-left
   all-but-last
   index
   contains?
   range
   debug
   make-anon-id
   make-named-id
   fluid-let
   memp
   for-all
   write-to-string)
  (begin
   
   (define (map-vector fn vec)
     (list->vector (map fn (vector->list vec))))
   
   (define (fold-left f init seq)
     (if (null? seq)
         init
         (fold-left f
                    (f (car seq) init)
                    (cdr seq))))
   
   (define (all-but-last l) (reverse (cdr (reverse l))))
   
   (define (index a b)
     (let ((tail (member a (reverse b))))
          (and tail (length (cdr tail)))))
   
   (define (contains? l i)
     (if (null? l) #f
         (or (eq? (car l) i) (contains? (cdr l) i))))
   
   (define (range start end step)
     (reverse (range-reversed '() start end step)))
   (define (range-reversed fold-var pos end step)
     (if (if (>= step 0)
             (< pos end)
             (> pos end))
         (range-reversed
          (cons pos fold-var)
          (+ pos step)
          end
          step)
         fold-var))
   
   (define (pretty v . args)
     (let ((out (if (pair? args) (car args) (current-output-port))))
          (cond-expand
           (gauche (pprint v :port out :newline #f))
           (else (display v out)))
          (newline out)))
   
   (define (debug . args)
     (for-all (lambda (a)
                      (display a)
                      (display " ")) args)
     (display "\n\n"))
   
   (define (make-anon-id prefix)
     (let ((count 0))
          (lambda ()
                  (set! count (+ 1 count))
                  (string->symbol (string-append prefix (number->string count))))))
   
   (define (make-named-id prefix)
     (let ((count 0))
          (lambda (name)
                  (set! count (+ 1 count))
                  (string->symbol (string-append prefix (number->string count) "_" (symbol->string name))))))
   
   (define-syntax fluid-let
     (syntax-rules ()
                   ((fluid-let () be ...)
                    (begin be ...))
                   ((fluid-let ((p0 e0) (p e) ...) be ...)
                    (let ((saved p0))
                         (set! p0 e0)
                         (call-with-values (lambda ()
                                                   (fluid-let ((p e) ...) be ...))
                                           (lambda results
                                                   (set! p0 saved)
                                                   (apply values results)))))))
   
   (define (memp proc ls)
     (cond ((null? ls) #f)
           ((pair? ls) (if (proc (car ls))
                           ls
                           (memp proc (cdr ls))))
           (else (error "memp: Invalid argument" ls))))
   
   (define (for-all proc l . ls)
     (or (null? l)
         (and (apply proc (car l) (map car ls))
              (apply for-all proc (cdr l) (map cdr ls)))))
   
   (define (write-to-string obj)
     (let ((port (open-output-string)))
          (write obj port)
          (get-output-string port)))
   
   ))
