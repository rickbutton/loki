(define-library 
    (p00_string2scheme)
    (import (scheme base))
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

(define *special-initials* (string->list "!$%&*/:<=>?^_~"))

(define *char-literal-regexp* (rx (: bos #\# #\\ any eos)))
(define *char-name-regexp* (rx (: bos #\# #\\ 
    (or "alarm" "backspace" "delete" "escape" "newline" "null" "return" "space" "tab") eos)))
(define *char-scalar-regexp* (rx (: bos #\# #\\ #\x (+ hex-digit) eos)))
(define (char-literal? str) (regexp-search *char-literal-regexp* str))
(define (char-name? str) (regexp-search *char-name-regexp* str))
(define (char-scalar? str) (regexp-search *char-scalar-regexp* str))

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
    (make-token string type location)
    token?
    (string token->string)
    (type token->type)
    (location token->location))

(define (tchar->token tchar type)
    (make-token 
        (list->string (list (tchar->char tchar)))
        type
        (tchar->location tchar)))
(define (tchars->string tchars) (list->string (map tchar->char tchars)))
(define (tchars->token tchars type)
    (let ((chars (map tchar->char tchars)))
        (make-token
            (list->string chars)
            type
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
        (define (emit-tchar tchar type)
            (set! tokens (cons (tchar->token tchar type) tokens)))
        (define (emit-buffer type)
            (if (not (null? buffer))
                (begin 
                    (set! tokens (cons (tchars->token (reverse buffer) type) tokens))
                    (set! buffer '()))))
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
                        (emit-tchar tchar 'paren)
                        (lex-ready))
                    (else 
                        (push-buffer tchar)
                        (lex-reading)))))

        (define (lex-reading)
            (let* ((tchar (reader)) (char (tchar->char tchar)))
                (if (delimiter? char)
                    (let ((string (buffer->string)))
                        (roll-back tchar)
                        (cond
                            ((parse-num string) (emit-buffer 'number))
                            ((equal? string "#t") (emit-buffer 'true))
                            ((equal? string "#true") (emit-buffer 'true))
                            ((equal? string "#f") (emit-buffer 'false))
                            ((equal? string "#false") (emit-buffer 'false))
                            ((char-literal? string) (emit-buffer 'char))
                            ((char-name? string) (emit-buffer 'char))
                            ((char-scalar? string) (emit-buffer 'char))
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
                        (emit-buffer 'string)
                        (lex-ready))
                    (else 
                        (push-buffer tchar)
                        (lex-string)))))

        (lex-ready)
        (reverse tokens)))

))