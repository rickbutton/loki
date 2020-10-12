(define-library (loki writer)
(import (loki core exception))
(import (scheme base))
(import (scheme char))
(import (scheme case-lambda))
(import (loki printer))
(import (srfi 69))
(export display write
        write-shared write-simple
        char-general-category is-identifier-char?)
(begin

; don't currently support unicode
; return an invalid category
(define (char-general-category c) 'ZZ)

(define (is-identifier-char? c)
  (or (char-ci<=? #\a c #\Z)
      (char<=? #\0 c #\9)
      (memv c '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~
                            #\+ #\- #\. #\@ #\x200C #\x200D))
      (and (> (char->integer c) 127)
           (memq (char-general-category c) ;XXX: could be done faster
             '(Lu Ll Lt Lm Lo Mn Nl No Pd Pc Po Sc Sm Sk So Co Nd Mc Me)))))

(define escapes '(
  (#\"          . #\")
  (#\\          . #\\)
  (#\alarm      . #\a)
  (#\backspace  . #\b)
  (#\tab        . #\t)
  (#\newline    . #\n)
  (#\return     . #\r)))
(define (write-escaped-string str port)
  (string-for-each (lambda (char)
    (let ((match (assv char escapes)))
      (if match
        (let ((mapped (cdr match)))
          (write-char #\\ port)
          (write-char mapped port))
        (write-char char port)))) str))

(define (write-symbol obj port)
  (let* ((string (symbol->string obj))
         (any-special-char? (memq #f
                              (map is-identifier-char? (string->list string)))))
    (if any-special-char?
      (write-escaped-string string port)
      (write-string string port))))

(define (write-pair obj in-maybe-list? port)
  (if in-maybe-list? (write-char #\( port))
  (cond
    ((null? (cdr obj))
      (write-simple (car obj) port)
      (write-char #\) port))
    ((pair? (cdr obj))
      (write-simple (car obj) port)
      (write-char #\space port)
      (write-pair (cdr obj) #f port))
    (else
      (write-simple (car obj) port)
      (write-char #\space port)
      (write-char #\. port)
      (write-char #\space port)
      (write-simple (cdr obj) port)
      (write-char #\) port))))


(define write-simple (case-lambda
  ((obj)
    (write-simple obj (current-output-port)))
  ((obj port)
    (cond
      ((null? obj)
        (write-string "()" port))
      ((pair? obj)
        (write-pair obj #t port))
      ((string? obj)
        (write-string "\"" port)
        (write-escaped-string obj port)
        (write-string "\"" port))
      ((symbol? obj)
        (write-symbol obj port))
      ((eq? #t obj)
        (write-string "#t" port))
      ((eq? #f obj)
        (write-string "#f" port))
      ((char? obj)
        (write-string "#\\" port)
        (write-char obj port))
      ((number? obj)
        (write-string (number->string obj) port))
      ((eof-object? obj)
        (write-string "#<eof>" port))
      ((procedure? obj)
        (write-string "#<procedure>" port))
      ((port? obj)
        (write-string "#<port" port)
        (if (input-port? obj)
          (write-string " input" port))
        (if (output-port? obj)
          (write-string " output" port))
        (write-string ">" port))
      ((vector? obj)
        (let ((length (vector-length obj)))
          (write-string "#(" port)
          (do ((i 0 (+ i 1)))
              ((>= i length))
            (write-simple (vector-ref obj i) port)
            (if (< i (- length 1)) (write-char #\space port)))
          (write-char #\) port)))
      ((bytevector? obj)
        (let ((length (bytevector-length obj)))
          (write-string "#u8(" port)
          (do ((i 0 (+ i 1)))
              ((>= i length))
            (write-string (number->string (bytevector-u8-ref obj i)) port)
            (if (< i (- length 1)) (write-char #\space port)))
          (write-char #\) port)))
      ((exception? obj)
        (write-string "#<exception " port)
        (write-simple (exception-message obj) port)
        (write-char #\space port)
        (for-each (lambda (irritant)
                  (write irritant port)
                  (write-char #\space port))
                  (exception-irritants obj))
        (write-string ">" port))
      (else
        (letrec ((writer (case-lambda
          ((x) (writer x #f))
          ((x display?)
            (if display? (display x port)
                         (write-simple x port))))))
        (print-object obj writer port)))))))


;;;; srfi-38.scm - reading and writing shared structures
;;
;; This code was written by Alex Shinn in 2009 and placed in the
;; Public Domain.  All warranties are disclaimed.

(define (extract-shared-objects x cyclic-only?)
  (let ((seen (make-hash-table eq?)))
    ;; find shared references
    (let find ((x x))
      (cond ;; only interested in pairs and vectors
       ((or (pair? x) (vector? x))
        ;; increment the count
        (hash-table-update!/default seen x (lambda (n) (+ n 1)) 0)
        ;; walk if this is the first time
        (cond
         ((> (hash-table-ref seen x) 1) #f)
         ((pair? x)
          (find (car x))
          (find (cdr x)))
         ((vector? x)
          (do ((i 0 (+ i 1)))
              ((= i (vector-length x)))
            (find (vector-ref x i)))))
        ;; delete if this shouldn't count as a shared reference
        (if (and cyclic-only?
                 (<= (hash-table-ref/default seen x 0) 1))
            (hash-table-delete! seen x)))))
    ;; extract shared references
    (let ((res (make-hash-table eq?)))
      (hash-table-walk
       seen
       (lambda (k v) (if (> v 1) (hash-table-set! res k #t))))
      res)))

(define (write-with-shared-structure x out cyclic-only?)
  (let ((shared (extract-shared-objects x cyclic-only?))
        (count 0))
    (define (check-shared x prefix cont)
      (let ((index (hash-table-ref/default shared x #f)))
        (cond ((integer? index)
               (display prefix out)
               (display "#" out)
               (write index out)
               (display "#" out))
              (else
               (cond (index
                      (display prefix out)
                      (display "#" out)
                      (write count out)
                      (display "=" out)
                      (hash-table-set! shared x count)
                      (set! count (+ count 1))))
               (cont x index)))))
    (let wr ((x x))
      (define (wr-one x shared?)
        (cond
         ((pair? x)
          (display "(" out)
          (wr (car x))
          (let lp ((ls (cdr x)))
            (check-shared
             ls
             " . "
             (lambda (ls shared?)
               (cond ((null? ls))
                     ((pair? ls)
                      (cond
                       (shared?
                        (display "(" out)
                        (wr (car ls))
                        (check-shared
                         (cdr ls)
                         " . "
                         (lambda (ls shared?) (lp ls)))
                        (display ")" out))
                       (else
                        (display " " out)
                        (wr (car ls))
                        (lp (cdr ls)))))
                     (shared?  ;; shared dotted tail
                      (wr-one ls #f))
                     (else
                      (display " . " out)
                      (wr ls))))))
          (display ")" out))
         (else (write-simple x out))))
      (check-shared x "" wr-one))))

(define write (case-lambda
  ((obj)
    (write-with-shared-structure obj (current-output-port) #t))
  ((obj port)
    (write-with-shared-structure obj port #t))))

(define write-shared (case-lambda
  ((obj)
    (write-with-shared-structure obj (current-output-port) #f))
  ((obj port)
    (write-with-shared-structure obj port #f))))

(define (display x . o)
  (let ((out (if (pair? o) (car o) (current-output-port))))
    (cond ((char? x) (write-char x out))
          ((string? x) (write-string x out))
          (else (write x out)))))

))
