(define-library 
    (p00_string2scheme)
    (import (scheme base))
    (import (scheme char))
    (import (scheme complex))
    (import (scheme write))
    (import (srfi 115))
    (import (srfi 159))
    (import (util))
    (import (shared))
    (export 
        p00_string2scheme 
        make-token
        token? 
        token->string 
        token->location)
(begin

(define *parens* (string->list "()"))
(define *whitespace* '(#\tab #\return #\newline #\space))
(define *semicolon* #\;)
(define *vertical* #\|)
(define *doublequote* #\")
(define *hash* #\#)
(define *backslash* #\\)

(define *char-literal-regexp* (rx (: bos #\# #\\ any eos)))
(define *char-name-regexp* (rx (: bos #\# #\\ 
    (or "alarm" "backspace" "delete" "escape" "newline" "null" "return" "space" "tab") eos)))
(define *char-scalar-regexp* (rx (: bos #\# #\\ #\x (+ hex-digit) eos)))
(define (char-literal? str) (regexp-search *char-literal-regexp* str))
(define (char-name? str) (regexp-search *char-name-regexp* str))
(define (char-scalar? str) (regexp-search *char-scalar-regexp* str))

(define (char-literal->char str buf) 
    (car (string->list (string-copy str 2 3))))
(define (char-name->char str)
    (cond
        ((equal? str "#\\alarm") #\alarm)
        ((equal? str "#\\backspace") #\backspace)
        ((equal? str "#\\delete") #\delete)
        ((equal? str "#\\escape") #\escape)
        ((equal? str "#\\newline") #\newline)
        ((equal? str "#\\null") #\null)
        ((equal? str "#\\return") #\return)
        ((equal? str "#\\space") #\space)
        ((equal? str "#\\tab") #\tab)
        (else (raise "unknown character name"))))
(define (char-scalar->char str)
    (let ((scalar (string-copy str 3 (string-length str))))
        (integer->char (real-string->number scalar 16 #t))))

(define *num-infnan-sre* '(or "+inf.0" "-inf.0" "+nan.0" "-nan.0"))
(define *num-sign-sre* '(or #\+ #\-))
(define *num-exactness-sre* '(? (or "#i" "#e")))

(define *num-radix-02-sre* "#b")
(define *num-radix-08-sre* "#o")
(define *num-radix-10-sre* '(? "#d"))
(define *num-radix-16-sre* "#x")

(define *num-digit-02-sre* '(or #\0 #\1))
(define *num-digit-08-sre* '(or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7))
(define *num-digit-10-sre* 'num)
(define *num-digit-16-sre* 'hex-digit)

(define (define-num-sre digit-sre radix-sre)
    (define num-uinteger-sre `(+ ,digit-sre))
    (define num-ureal-sre
        `(or 
            ,num-uinteger-sre
            (: ,num-uinteger-sre #\/ ,num-uinteger-sre)))
    (define num-real-sre
        `(or
            (: (? ,*num-sign-sre*) ,num-ureal-sre)
            ,*num-infnan-sre*))
    (define num-complex-sre
        `(or
            (-> real ,num-real-sre)
            (: (-> real ,num-real-sre) (-> imag (: ,*num-sign-sre* (? ,num-ureal-sre))) #\i)
            (: (-> real ,num-real-sre) (-> imag ,*num-infnan-sre*) #\i)
            (: (-> x ,num-real-sre) #\@ (-> y ,num-real-sre))
            (: (-> imag ,*num-sign-sre* ,num-ureal-sre) #\i)
            (: (-> imag ,*num-infnan-sre*) #\i)
            (: (-> imag ,*num-sign-sre*) #\i)))
    (define num-prefix-sre
        `(or
            (: (-> radix ,radix-sre) (-> exact ,*num-exactness-sre*))
            (: (-> exact ,*num-exactness-sre*) (-> radix ,radix-sre))))
    `(: ,num-prefix-sre ,num-complex-sre))

(define *num-02-sre* (define-num-sre *num-digit-02-sre* *num-radix-02-sre*))
(define *num-08-sre* (define-num-sre *num-digit-08-sre* *num-radix-08-sre*))
(define *num-10-sre* (define-num-sre *num-digit-10-sre* *num-radix-10-sre*))
(define *num-16-sre* (define-num-sre *num-digit-16-sre* *num-radix-16-sre*))

(define *num-02-regexp* (regexp *num-02-sre*))
(define *num-08-regexp* (regexp *num-08-sre*))
(define *num-10-regexp* (regexp *num-10-sre*))
(define *num-16-regexp* (regexp *num-16-sre*))
(define *num-regexp* (regexp 
    `(or 
        ,*num-10-sre*
        ,*num-02-sre*
        ,*num-08-sre*
        ,*num-16-sre*
        )))
(define (parse-num str) 
    (or
        (regexp-matches *num-10-regexp* str)
        (regexp-matches *num-16-regexp* str)
        (regexp-matches *num-02-regexp* str)
        (regexp-matches *num-08-regexp* str)))

(define (char-hex-letter? char)
    (let ((int (char->integer char)))
        (or
            (and (>= int 65) (<= int 70))
            (and (>= int 97) (<= int 102)))))
(define (char-hex-letter->number char)
    (let ((int (char->integer char)))
        (+ (if (and (>= int 65) (<= int 70))
            (- int 65)
            (- int 97)) 10)))
(define (radix->base radix)
    (cond
        ((equal? radix "#b") 2)
        ((equal? radix "#o") 8)
        ((equal? radix "#d") 10)
        ((equal? radix "") 10)
        ((equal? radix "#x") 16)
        (else (raise "unknown radix"))))
(define (real-string->number str base is-exact)
    (if str
        (cond
            ((equal? str "+inf.0") +inf.0)
            ((equal? str "-inf.0") -inf.0)
            ((equal? str "+nan.0") +nan.0)
            ((equal? str "-nan.0") -nan.0)
            (else
                (let* ((chars (string->list str))
                    (sign 1)
                    (num (fold-left (lambda (char num)
                        (cond 
                            ((char-numeric? char) (+ (* num base) (digit-value char)))
                            ((char-hex-letter? char) (+ (* num base) (char-hex-letter->number char)))
                            ((equal? char #\-) (set! sign -1) num)
                            ((equal? char #\+) num)
                            ((equal? char #\/) (raise "rational literals unimplemented"))
                            (else (raise "unhandled character in real number string")))) 0 chars)))
                    (* sign (if is-exact (exact num) (inexact num))))))
                0))
(define (imag-string->number str base is-exact)
    (cond
        ((equal? str "-") (if is-exact (exact -1) (inexact -1)))
        ((equal? str "+") (if is-exact (exact 1) (inexact 1)))
        ((equal? str "") (if is-exact (exact 1) (inexact 1)))
        (else (real-string->number str base is-exact))))

(define (lexed-num->num str matches)
    (let* ((real (regexp-match-submatch matches 'real))
          (imag (regexp-match-submatch matches 'imag))
          (x (regexp-match-submatch matches 'x))
          (y (regexp-match-submatch matches 'y))
          (radix (regexp-match-submatch matches 'radix))
          (exactness (regexp-match-submatch matches 'exact))
          (exact (if (equal? exactness "#i") #f #t))
          (base (radix->base radix))
          (realnum (real-string->number real base exact))
          (imagnum (imag-string->number imag base exact))
          (xnum (real-string->number x base exact))
          (ynum (imag-string->number y base exact)))
        (if (or x y)
            (make-polar xnum ynum)
            (make-rectangular realnum imagnum))))

(define (paren? c) (member c *parens*))
(define (whitespace? c) (member c *whitespace*))
(define (semicolon? c) (equal? c *semicolon*))
(define (doublequote? c) (equal? c *doublequote*))
(define (hash? c) (equal? c *hash*))
(define (vertical? c) (equal? c *vertical*))
(define (delimiter? c)
    (or
        (eof-object? c)
        (paren? c)
        (whitespace? c)
        (vertical? c)
        (doublequote? c)
        (semicolon? c)))

(define-record-type <tchar>
    (make-tchar char location)
    tchar?
    (char tchar->char)
    (location tchar->location))

(define-record-type <token>
    (make-token string type value location)
    token?
    (string token->string)
    (type token->type)
    (value token->value)
    (location token->location))

(define (tchar->token tchar type value)
    (make-token 
        (list->string (list (tchar->char tchar)))
        type
        value
        (tchar->location tchar)))
(define (tchars->string tchars) (list->string (map tchar->char tchars)))
(define (tchars->token tchars type value)
    (let ((chars (map tchar->char tchars)))
        (make-token
            (list->string chars)
            type
            value
            (tchar->location (car tchars)))))

(define-record-type <reader>
    (make-reader port saves line col)
    reader?
    (port reader->port)
    (saves reader->saves set-reader-saves)
    (line reader->line set-reader-line)
    (col reader->col set-reader-col))

(define (port->reader port)
    (make-reader port '() 1 1))

(define (read-reader reader)
    (let ((port (reader->port reader))
          (saves (reader->saves reader)) 
          (line (reader->line reader)) 
          (col (reader->col reader)))
        (if (null? saves)
            (let ((tchar (make-tchar
                  (read-char port)
                  (make-source-location line col))))
                (if (eq? (tchar->char tchar) #\newline)
                    (begin
                        (set-reader-line reader (+ line 1))
                        (set-reader-col reader 1)
                        tchar)
                    (begin
                        (set-reader-col reader (+ col 1))
                        tchar)))
            (let ((save (car saves)))
                (set-reader-saves reader (cdr saves))
                save))))

(define (peek-reader reader)
    (let ((tchar (read-reader reader)))
        (roll-back-reader reader tchar)
        tchar))
            
(define (roll-back-reader reader tchar)
    (set-reader-saves reader (cons tchar (reader->saves reader))))
                
(define (add-token token tokens) (cons token tokens))

(define (p00_string2scheme port)
    (let* ((raw-reader (port->reader port))
           (tokens '())
           (buffer '())
           (should-roll-back #f))
        (define (emit-tchar tchar type value)
            (set! tokens (cons (tchar->token tchar type value) tokens)))
        (define (emit-string string type value)
            (set! tokens (cons (tchars->token (reverse buffer) type value) tokens))
            (set! buffer '()))
        (define (push-buffer tchar)
            (set! buffer (cons tchar buffer)))
        (define (reader) (read-reader raw-reader))
        (define (roll-back tchar)
            (roll-back-reader raw-reader tchar))
        (define (buffer->string)
            (tchars->string (reverse buffer)))
        (define (error-with-value msg value)
            (error (string-append 
                "error! "
                msg
                ": "
                value)))

        (define (lex-ready)
            (let* ((tchar (reader)) (char (tchar->char tchar)))
                (cond
                    ((eof-object? char) #f)
                    ((whitespace? char) (lex-ready))
                    ((doublequote? char) 
                        (push-buffer tchar)
                        (lex-string))
                    ((paren? char)
                        (emit-tchar tchar 'paren #f)
                        (lex-ready))
                    (else 
                        (push-buffer tchar)
                        (lex-reading)))))

        (define (lex-reading)
            (let* ((tchar (reader)) (char (tchar->char tchar)))
                (if (delimiter? char)
                    (let* ((string (buffer->string)) (num-matches (parse-num string)))
                        (roll-back tchar)
                        (cond
                            (num-matches (emit-string string 'number (lexed-num->num string num-matches)))
                            ((equal? string "#t") (emit-string string 'boolean #t))
                            ((equal? string "#true") (emit-string string 'boolean #t))
                            ((equal? string "#f") (emit-string string 'boolean #f))
                            ((equal? string "#false") (emit-string string 'boolean #f))
                            ((char-literal? string) (emit-string string 'char (char-literal->char string buffer)))
                            ((char-name? string) (emit-string string 'char (char-name->char string)))
                            ((char-scalar? string) (emit-string string 'char (char-scalar->char string)))
                            (else (error-with-value "unknown value" string)))
                        (lex-ready))
                    (begin
                        (push-buffer tchar)
                        (lex-reading)))))

        ; escapes need to be re-handled
        ; as well as multi line strings with \
        (define (lex-string)
            (let* ((tchar (reader)) (char (tchar->char tchar)))
                (cond
                    ((eof-object? char) (error "unterminated string!!!"))
                    ((equal? char *doublequote*) 
                        (push-buffer tchar)
                        (let ((str (buffer->string)))
                            (emit-string str 'string (string-copy str 1 (- (string-length str) 1)))
                            (lex-ready)))
                    (else 
                        (push-buffer tchar)
                        (lex-string)))))

        (lex-ready)
        (reverse tokens)))

))