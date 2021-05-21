(define-library (loki compiler match)
  (import (scheme base))
  (import (loki util))
  (import (loki core syntax))
  (export match)
  (begin
   
   ;; A trivial but extremely useful s-expression matcher.
   ;; Implements a subset of Wright's matcher's patterns.
   ;; Includes additional (syntax id) pattern that matches
   ;; if input is identifier? and free=? to 'id.
   (define-syntax match
     (syntax-rules ()
                   ((match (op arg ...) clause ...)
                    (let ((x (op arg ...)))
                      (match x clause ...)))
                   ((match x)
                    (invalid-form x))
                   ((match x (pat e ...) clause ...)
                    (matcher "base" pat "done" x (e ...) (lambda () (match x clause ...))))))
   
   (define-syntax matcher
     (syntax-rules (- ___ ? %)
                   ((matcher "base" () k arg ...)
                    (matcher k (lambda (x sk fk) (if (null? x) (sk) (fk))) () arg ...))
                   ((matcher "base" - k arg ...)
                    (matcher k (lambda (x sk fk) (sk)) () arg ...))
                   ((matcher "base" (% pred? id) k arg ...)
                    (matcher k
                             (lambda (x sk fk)
                               (if (pred? x 'id) (sk) (fk)))
                             ()
                             arg ...))
                   ((matcher "base" (? pred? p) k arg ...)
                    (matcher "base" p "predicate" pred? k arg ...))
                   ((matcher "predicate" code vars pred? k arg ...)
                    (matcher k
                             (lambda (x sk fk)
                               (if (pred? x)
                                   (code x sk fk)
                                 (fk)))
                             vars
                             arg ...))
                   ((matcher "base" (p1 ___ tailp ...) k arg ...)
                    (matcher "base" p1 "ellipses" (tailp ...) k arg ...))
                   ((matcher "ellipses" code vars (tailp ...) k arg ...)
                    (matcher k
                             (lambda (x sk fk)
                               (let loop ((x x)
                                          (result '()))
                                 (define (match-tail)
                                   (match x
                                     ((tailp ...)
                                      (apply sk (if (null? result)
                                                    (map (lambda (ignore) '()) 'vars)
                                                  (apply map list (reverse result)))))
                                     (- (fk))))
                                 (cond ((null? x) (match-tail))
                                       ((pair? x)
                                        (code (car x)
                                              (lambda car-vars
                                                (loop (cdr x) (cons car-vars result)))
                                              match-tail))
                                       (else (fk)))))
                             vars
                             arg ...))
                   ((matcher "base" (p1 . p2) k arg ...)
                    (matcher "base" p1 "pair" p2 k arg ...))
                   ((matcher "pair" car-code car-vars p2 k arg ...)
                    (matcher "base" p2 "pair-done" car-code car-vars k arg ...))
                   ((matcher "pair-done" cdr-code (cdr-var ...) car-code (car-var ...) k arg ...)
                    (matcher k
                             (lambda (x sk fk)
                               (if (pair? x)
                                   (car-code (car x)
                                             (lambda (car-var ...)
                                               (cdr-code (cdr x)
                                                         (lambda (cdr-var ...)
                                                           (sk car-var ... cdr-var ...))
                                                         fk))
                                             fk)
                                 (fk)))
                             (car-var ... cdr-var ...)
                             arg ...))
                   ((matcher "base" #(p ___) k arg ...)
                    (matcher "base" (p ___) "vector" k arg ...))
                   ((matcher "vector" list-code vars k arg ...)
                    (matcher k
                             (lambda (x sk fk)
                               (if (vector? x)
                                   (list-code (vector->list x)
                                              sk
                                              fk)
                                 (fk)))
                             vars
                             arg ...))
                   ((matcher "base" id k arg ...)
                    (matcher k (lambda (x sk fk) (sk x)) (id) arg ...))
                   ((matcher "done" code vars x (e ...) fk)
                    (code x (lambda vars e ...) fk))))
   
   ))
