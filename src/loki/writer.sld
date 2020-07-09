(define-library (loki writer)
(import (scheme base))
(import (scheme char))
(import (scheme case-lambda))
(cond-expand
  (chibi
    (import (rename (chibi ast) (type-printer-set! chibi-type-printer-set!))))
  (loki
    (import (core records))))
(export display write write-shared write-simple type-printer-set!
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
        (write-char char)))) str))

(define (write-symbol obj port)
  (let* ((string (symbol->string obj))
         (any-special-char? (memq #f
                              (map is-identifier-char? (string->list string)))))
    (if any-special-char?
      (write-escaped-string string port)
      (write-string string port))))

(define (write-pair obj in-maybe-list? port)
  (unless in-maybe-list? (write-char #\( port))
  (cond
    ((null? (cdr obj))
      (write-simple (car obj) port)
      (write-char #\) port))
    ((pair? (cdr obj))
      (write-simple (car obj) port)
      (write-char #\space port)
      (write-pair (cdr obj) #t port))
    (else
      (write-simple (car obj) port)
      (write-char #\space port)
      (write-char #\. port)
      (write-char #\space port)
      (write-pair (cdr obj) #f port))))

(cond-expand
  (chibi 
    (define (record? r) #f)
    (define (record-printer r) #f)))

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
      ((record? obj)
        ((record-printer obj) obj port))
      ((vector? obj)
        (let ((length (vector-length obj)))
          (write-string "#(" port)
          (do ((i 0 (+ i 1)))
              ((>= i length))
            (write-simple (vector-ref obj i) port))
          (write-char #\) port)))
      ((bytevector? obj)
        (let ((length (bytevector-length obj)))
          (write-string "#u8(" port)
          (do ((i 0 (+ i 1)))
              ((>= i length))
            (write-u8 (bytevector-u8-ref obj i) port))
          (write-char #\) port)))))))

(define write write-simple)
(define write-shared write-simple)

(define (display x . o)
  (let ((out (if (pair? o) (car o) (current-output-port))))
    (cond ((char? x) (write-char x out))
          ((string? x) (write-string x out))
          (else (write x out)))))

(define (type-printer-set! type printer)
  (cond-expand
    (chibi (chibi-type-printer-set! type 
      (lambda (x writer out) (printer x out))))
    (loki
      (record-type-printer-set! type printer))))

))
