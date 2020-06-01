;;
;; loki reader
;;
;; This is a port of the r7rs/r6rs reader located at:
;; https://github.com/weinholt/laesare
;; with r6rs support removed.
;;
;; Original License:
;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2017, 2018, 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.
(define-library (loki reader)
(import (scheme base))
(import (scheme char))
(import (loki compat))
(import (loki shared))
(import (loki util))
(import (srfi 69))
(export make-reader read-annotated)
(begin

; don't currently support unicode
; return an invalid category
(define (char-general-category c) 'ZZ)

;; Peek at the next char from the reader.
(define (lookahead-char reader)
  (let ((next-char (reader-next-char reader)))
    (if next-char
        next-char
        (let ((c (get-next-char reader)))
            (reader-next-char-set! reader c)
            c))))

(define (get-next-char reader)
  (let ((next-char (reader-next-char reader)))
    (if next-char 
        (begin
            (reader-next-char-set! reader #f)
            next-char)
        (read-char (reader-port reader)))))

;; Get a char from the reader.
(define (get-char reader)
  (let ((c (get-next-char reader)))
    (when (eqv? c #\newline)
      (reader-line-set! reader (+ (reader-line reader) 1))
      (reader-column-set! reader -1))
    (reader-column-set! reader (+ (reader-column reader) 1))
    c))

;; Detects the (intended) type of Scheme source: r7rs-library,
;; r7rs-program, empty or unknown.
(define (detect-scheme-file-type port)
  (let ((reader (make-reader port "<unknown>")))
    (let-values (((type lexeme) (get-lexeme reader)))
      (case type
        ((eof)
         'empty)
        ((openp)                  ;a pair
         (let-values (((type lexeme) (get-lexeme reader)))
           (case type
             ((identifier)
              (case lexeme
                ((import) 'r7rs-program)
                ((define-library) 'r7rs-library)
                (else 'unknown)))
             (else 'unknown))))
        (else 'unknown)))))

(define-record-type <reader>
    (make-reader-record 
        port filename next-char line column saved-line saved-column fold-case? tolerant?)
    reader?
    (port reader-port)
    (filename reader-filename)
    (next-char reader-next-char reader-next-char-set!)
    (line reader-line reader-line-set!)
    (column reader-column reader-column-set!)
    (saved-line reader-saved-line reader-saved-line-set!)
    (saved-column reader-saved-column reader-saved-column-set!)
    (fold-case? reader-fold-case? reader-fold-case?-set!)
    (tolerant? reader-tolerant? reader-tolerant?-set!))
(define (make-reader port filename)
    (make-reader-record port filename #f 1 0 1 0 #f #f))

(define (reader-mark reader)
  (reader-saved-line-set! reader (reader-line reader))
  (reader-saved-column-set! reader (reader-column reader)))

(define (reader-source reader)
  (make-source (reader-filename reader)
          (reader-saved-line reader)
          (reader-saved-column reader)))

(define-record-type <reference>
    (make-reference label)
    reference?
    (label reference-label))

(define (read-annotated reader)
  (assert (reader? reader))
  (let ((labels (make-labels)))
    (let*-values (((type x) (get-lexeme reader))
                  ((d) (handle-lexeme reader type x labels #f)))
      (resolve-labels reader labels)
      d)))

;;; Lexeme reader

; TODO - use irritants
(define (reader-error reader msg . irritants)
  ;; Non-recoverable errors.
  (raise-loki-error
    (make-source
      (reader-filename reader)
      (reader-saved-line reader)
      (reader-saved-column reader))
    msg))

; TODO - use irritants
(define (reader-warning reader msg . irritants)
  ;; Recoverable if the reader is tolerant.
  (if (reader-tolerant? reader)
      (raise-loki-error
        (make-source
          (reader-filename reader)
          (reader-saved-line reader)
          (reader-saved-column reader))
        msg)
      (apply reader-error reader msg irritants)))

(define (eof-warning reader)
  (reader-warning reader "Unexpected EOF" (eof-object)))

(define (unicode-scalar-value? sv)
  (and (<= 0 sv #x10FFFF)
       (not (<= #xD800 sv #xDFFF))))

(define (char-delimiter? reader c)
  ;; Treats the eof-object as a delimiter
  (or (eof-object? c)
      (char-whitespace? c)
      (memv c '(#\( #\) #\" #\; #\|))))

;; Get a line from the reader.
(define (get-line reader)
  (call-with-string-output-port
   (lambda (out)
     (do ((c (get-char reader) (get-char reader)))
         ((or (eqv? c #\newline) (eof-object? c)))
       (write-char c out)))))

;; Gets whitespace from the reader.
(define (get-whitespace reader char)
  (call-with-string-output-port
   (lambda (out)
     (let lp ((char char))
       (write-char char out)
       (let ((char (lookahead-char reader)))
         (when (and (char? char) (char-whitespace? char))
           (lp (get-char reader))))))))

;; Get an inline hex escape (escaped character inside an identifier).
(define (get-inline-hex-escape p)
  (reader-mark p)
  (let lp ((digits '()))
    (let ((c (get-char p)))
      (cond ((eof-object? c)
             (eof-warning p)
             #\xFFFD)
            ((or (char<=? #\0 c #\9)
                 (char-ci<=? #\a c #\f))
             (lp (cons c digits)))
            ((and (char=? c #\;) (pair? digits))
             (let ((sv (string->number (list->string (reverse digits)) 16)))
               (cond ((unicode-scalar-value? sv)
                      (integer->char sv))
                     (else
                      (reader-warning p "Inline hex escape outside valid range" sv)
                      #\xFFFD))))
            (else
             (reader-warning p "Invalid inline hex escape" c)
             #\xFFFD)))))

(define (get-identifier p initial-char pipe-quoted?)
  (let lp ((chars (if initial-char (list initial-char) '())))
    (let ((c (lookahead-char p)))
      (cond
        ((and (char? c)
              (or (char-ci<=? #\a c #\Z)
                  (char<=? #\0 c #\9)
                  (memv c '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~
                            #\+ #\- #\. #\@ #\x200C #\x200D))
                (and (> (char->integer c) 127)
                    (memq (char-general-category c) ;XXX: could be done faster
                           '(Lu Ll Lt Lm Lo Mn Nl No Pd Pc Po Sc Sm Sk So Co Nd Mc Me)))))
         (lp (cons (get-char p) chars)))
        ((and pipe-quoted? (char? c) (not (memv c '(#\| #\\))))
         (lp (cons (get-char p) chars)))
        ((or (char-delimiter? p c) (and pipe-quoted? (eqv? c #\|)))
         (when (eqv? c #\|)
           (get-char p))
         (let ((id (list->string (reverse chars))))
           (if (reader-fold-case? p)
               (values 'identifier (string->symbol (string-foldcase id)))
               (values 'identifier (string->symbol id)))))
        ((char=? c #\\)           ;\xUUUU;
         (get-char p)             ;consume #\\
         (let ((c (get-char p)))  ;should be #\x
           (cond ((eqv? c #\x)
                  (lp (cons (get-inline-hex-escape p) chars)))
                 ((and pipe-quoted?
                       (assv c '((#\" . #\")
                                 (#\\ . #\\)
                                 (#\a . #\alarm)
                                 (#\b . #\backspace)
                                 (#\t . #\tab)
                                 (#\n . #\newline)
                                 (#\r . #\return)
                                 (#\| . #\|))))
                  => (lambda (c) (lp (cons (cdr c) chars))))
                 (else
                  (if (eof-object? c)
                      (eof-warning p)
                      (reader-warning p "Invalid character following \\"))
                  (lp chars)))))
        (else
         (reader-warning p "Invalid character in identifier" c)
         (get-char p)
         (lp chars))))))

;; Get a number from the reader.
(define (get-number p initial-chars)
  (let lp ((chars initial-chars))
    (let ((c (lookahead-char p)))
      (cond ((and (not (eqv? c #\#)) (char-delimiter? p c))
             ;; TODO: some standard numbers are not supported
             ;; everywhere, should use a number lexer.
             (let ((str (list->string (reverse chars))))
               (cond ((string->number str) =>
                      (lambda (num)
                        (values 'value num)))
                                ;; TODO: This is incomplete.
                     ((not (and (pair? initial-chars)
                                (char<=? #\0 (car initial-chars) #\9)))
                      (values 'identifier (string->symbol str)))
                     (else
                      (reader-warning p "Invalid number syntax" str)
                      (values 'identifier (string->symbol str))))))
            (else
             (lp (cons (get-char p) chars)))))))

;; Get a string datum from the reader.
(define (get-string p)
  (let lp ((chars '()))
    (let ((c (lookahead-char p)))
      (cond ((eof-object? c)
             (eof-warning p)
             c)
            ((char=? c #\")
             (get-char p)
             (list->string (reverse chars)))
            ((char=? c #\\)           ;escapes
             (get-char p)             ;consume #\\
             (let ((c (lookahead-char p)))
               (cond ((eof-object? c)
                      (eof-warning p)
                      c)
                     ((or (memv c '(#\tab #\newline #\x85 #\x2028))
                          (eq? (char-general-category c) 'Zs))
                      ;; \<intraline whitespace>*<line ending>
                      ;; <intraline whitespace>*
                      (letrec ((skip-intraline-whitespace*
                                (lambda ()
                                  (let ((c (lookahead-char p)))
                                    (cond ((eof-object? c)
                                           (eof-warning p)
                                           c)
                                          ((or (char=? c '#\tab)
                                               (eq? (char-general-category c) 'Zs))
                                           (get-char p)
                                           (skip-intraline-whitespace*))))))
                               (skip-newline
                                (lambda ()
                                  (let ((c (get-char p)))
                                    (cond ((eof-object? c) c)
                                          ((memv c '(#\newline #\x85 #\x2028)))
                                          ((char=? c #\return)
                                           (when (memv (lookahead-char p)
                                                       '(#\newline #\x85))
                                             (get-char p)))
                                          (else
                                           (reader-warning p "Expected a line ending" c)))))))
                        (skip-intraline-whitespace*)
                        (skip-newline)
                        (skip-intraline-whitespace*)
                        (lp chars)))
                     (else
                      (lp (cons
                           (case (get-char p)
                             ((#\") #\")
                             ((#\\) #\\)
                             ((#\a) #\alarm)
                             ((#\b) #\backspace)
                             ((#\t) #\tab)
                             ((#\n) #\newline)
                             ((#\r) #\return)
                             ((#\|) #\|)
                             ((#\x) (get-inline-hex-escape p))
                             (else
                              (reader-warning p "Invalid escape in string" c)
                              #\xFFFD))
                           chars))))))
            (else
             (lp (cons (get-char p) chars)))))))

;; Gets a nested comment from the reader.
(define (get-nested-comment reader)
  ;; The reader is immediately after "#|".
  (call-with-string-output-port
   (lambda (out)
     (let lp ((levels 1) (c0 (get-char reader)))
       (let ((c1 (get-char reader)))
         (cond ((eof-object? c0)
                (eof-warning reader))
               ((and (eqv? c0 #\|) (eqv? c1 #\#))
                (unless (eqv? levels 1)
                  (write-char c0 out)
                  (write-char c1 out)
                  (lp (- levels 1) (get-char reader))))
               ((and (eqv? c0 #\#) (eqv? c1 #\|))
                (write-char c0 out)
                (write-char c1 out)
                (lp (+ levels 1) (get-char reader)))
               (else
                (write-char c0 out)
                (lp levels c1))))))))

;; Get a comment from the reader (including the terminating whitespace).
(define (get-comment reader)
  ;; The reader is immediately after #\;.
  (call-with-string-output-port
   (lambda (out)
     (let lp ()
       (let ((c (get-char reader)))
         (unless (eof-object? c)
           (write-char c out)
           (cond ((memv c '(#\newline #\x85 #\x2028 #\x2029)))
                 ((char=? c #\return)
                  ;; Weird line ending. This lookahead is what forces
                  ;; the procedure to include the terminator.
                  (when (memv (lookahead-char reader) '(#\newline #\x85))
                    (write-char (get-char reader) out)))
                 (else
                  (lp)))))))))

;; Whitespace and comments can appear anywhere.
(define (atmosphere? type)
  (memq type '(directive whitespace comment inline-comment nested-comment)))

;; Get the next lexeme from the reader, ignoring anything that is
;; like a comment.
(define (get-lexeme p)
  (let-values (((type lexeme) (get-token p)))
    (if (atmosphere? type)
        (get-lexeme p)
        (values type lexeme))))

;; Get the next token. Can be a lexeme, directive, whitespace or comment.
(define (get-token p)
  (assert (reader? p))
  (reader-mark p)
  (let ((c (get-char p)))
    (cond
      ((eof-object? c)
       (values 'eof c))
      ((char-whitespace? c)
       (values 'whitespace (get-whitespace p c)))
      ((char=? c #\;)                 ;a comment like this one
       (values 'comment (get-comment p)))
      ((char=? c #\#)                 ;the mighty octothorpe
       (let ((c (get-char p)))
         (case c
           ((#\() (values 'vector #f))
           ((#\') (values 'abbrev 'syntax))
           ((#\`) (values 'abbrev 'quasisyntax))
           ((#\,)
            (case (lookahead-char p)
              ((#\@)
               (get-char p)
               (values 'abbrev 'unsyntax-splicing))
              (else (values 'abbrev 'unsyntax))))
           ((#\u #\U)                   ;r7rs
            (let* ((c1 (and (eqv? (lookahead-char p) #\8) (get-char p)))
                   (c2 (and (eqv? c1 #\8) (eqv? (lookahead-char p) #\() (get-char p))))
              (cond ((and (eqv? c1 #\8) (eqv? c2 #\())
                     (values 'bytevector #f))
                    (else
                     (reader-warning p "Expected #u8(")
                     (get-token p)))))
           ((#\;)                     ;s-expr/datum comment
            (let lp ((atmosphere '()))
              (let-values (((type token) (get-token p)))
                (cond ((eq? type 'eof)
                       (eof-warning p)
                       (values 'inline-comment (cons (reverse atmosphere) p)))
                      ((atmosphere? type)
                       (lp (cons (cons type token) atmosphere)))
                      (else
                       (let-values (((d __) (handle-lexeme p type token #f #t)))
                         (values 'inline-comment (cons (reverse atmosphere) d))))))))
           ((#\|)                     ;nested comment
            (values 'nested-comment (get-nested-comment p)))
           ((#\!)                     ;#!r6rs etc
            (let ((next-char (lookahead-char p)))
              (cond ((and (= (reader-saved-line p) 1) (memv next-char '(#\/ #\space)))
                     (let ((line (reader-saved-line p))
                           (column (reader-saved-column p)))
                       (values 'shebang `(,line ,column ,(get-line p)))))
                    ((and (char? next-char) (char-alphabetic? next-char))
                     (let-values (((type id) (get-token p)))
                       (cond
                         ((eq? type 'identifier)
                          (case id
                            ((fold-case)     ;r6rs-app.pdf
                             (reader-fold-case?-set! p #t))
                            ((no-fold-case)  ;r6rs-app.pdf
                             (reader-fold-case?-set! p #f))
                            (else
                             (reader-warning p "Invalid directive" type id)))
                          (cond ((assq id '((false . #f) (true . #t)))
                                 => (lambda (x) (values 'value (cdr x))))
                                (else
                                 (values 'directive id))))
                         (else
                          (reader-warning p "Expected an identifier after #!")
                          (get-token p)))))
                    (else
                     (reader-warning p "Expected an identifier after #!")
                     (get-token p)))))
           ((#\b #\B #\o #\O #\d #\D #\x #\X #\i #\I #\e #\E)
            (get-number p (list c #\#)))
           ((#\t #\T)
            (unless (char-delimiter? p (lookahead-char p))
              (let* ((c1 (and (memv (lookahead-char p) '(#\r #\R)) (get-char p)))
                     (c2 (and c1 (memv (lookahead-char p) '(#\u #\U)) (get-char p)))
                     (c3 (and c2 (memv (lookahead-char p) '(#\e #\E)) (get-char p))))
                (unless (and c1 c2 c3 (char-delimiter? p (lookahead-char p)))
                  (reader-warning p "Expected #true"))))
            (values 'value #t))
           ((#\f #\F)
            (unless (char-delimiter? p (lookahead-char p))
              (let* ((c1 (and (memv (lookahead-char p) '(#\a #\A)) (get-char p)))
                     (c2 (and c1 (memv (lookahead-char p) '(#\l #\L)) (get-char p)))
                     (c3 (and c2 (memv (lookahead-char p) '(#\s #\S)) (get-char p)))
                     (c4 (and c3 (memv (lookahead-char p) '(#\e #\E)) (get-char p))))
                (unless (and c1 c2 c3 c4 (char-delimiter? p (lookahead-char p)))
                  (reader-warning p "Expected #false" c1 c2 c3 c4))))
            (values 'value #f))
           ((#\\)
            (let lp ((char* '()))
              (let ((c (lookahead-char p)))
                (cond ((and (pair? char*) (char-delimiter? p c))
                       (let ((char* (reverse char*)))
                         (cond ((null? char*)
                                (reader-warning p "Empty character name")
                                (values 'value #\xFFFD))
                               ((null? (cdr char*)) (values 'value (car char*)))
                               ((char=? (car char*) #\x)
                                (cond ((for-all (lambda (c)
                                                  (or (char<=? #\0 c #\9)
                                                      (char-ci<=? #\a c #\f)))
                                                (cdr char*))
                                       (let ((sv (string->number (list->string (cdr char*)) 16)))
                                         (cond ((unicode-scalar-value? sv)
                                                (values 'value (integer->char sv)))
                                               (else
                                                (reader-warning p "Hex-escaped character outside valid range" sv)
                                                (values 'value #\xFFFD)))))
                                      (else
                                       (reader-warning p "Invalid character in hex-escaped character"
                                                       (list->string (cdr char*)))
                                       (values 'value #\xFFFD))))
                               (else
                                (let ((char-name (list->string char*))
                                      (char-names '(("null" #\null)
                                                    ("alarm" #\alarm)
                                                    ("backspace" #\backspace)
                                                    ("tab" #\tab)
                                                    ("newline" #\newline)
                                                    ("return" #\return)
                                                    ("escape" #\escape)
                                                    ("space" #\space)
                                                    ("delete" #\delete))))
                                  (cond
                                    ((or (assoc char-name char-names)
                                         (and (reader-fold-case? p)
                                              (assoc (string-foldcase char-name)
                                                     char-names)))
                                     => (lambda (char-data)
                                          (values 'value (cadr char-data))))
                                    (else
                                     (reader-warning p "Invalid character name" char-name)
                                     (values 'value #\xFFFD))))))))
                      ((and (null? char*) (eof-object? c))
                       (eof-warning p)
                       (values 'value #\xFFFD))
                      (else
                       (lp (cons (get-char p) char*)))))))
           ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
            (let lp ((char* (list c)))
              (let ((next (lookahead-char p)))
                (cond
                  ((eof-object? next)
                   (eof-warning p)
                   (get-char p))
                  ((char<=? #\0 next #\9)
                   (lp (cons (get-char p) char*)))
                  ((char=? next #\=)
                   (get-char p)
                   (values 'label (string->number (list->string (reverse char*)) 10)))
                  ((char=? next #\#)
                   (get-char p)
                   (values 'reference (string->number (list->string (reverse char*)) 10)))
                  (else
                   (reader-warning p "Expected #<n>=<datum> or #<n>#" next)
                   (get-token p))))))
           (else
            (reader-warning p "Invalid #-syntax" c)
            (get-token p)))))
      ((char=? c #\")
       (values 'value (get-string p)))
      ((memv c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
       (get-number p (list c)))
      ((memv c '(#\- #\+))            ;peculiar identifier
       (cond ((and (char=? c #\-) (eqv? #\> (lookahead-char p))) ;->
              (get-identifier p c #f))
             ((char-delimiter? p (lookahead-char p))
              (values 'identifier (if (eqv? c #\-) '- '+)))
             (else
              (get-number p (list c)))))
      ((char=? c #\.)                 ;peculiar identifier
       (if (char-delimiter? p (lookahead-char p))
         (values 'dot #f)
         (get-number p (list c))))
      ((or (char-ci<=? #\a c #\Z) ;<constituent> and <special initial>
           (memv c '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~))
           (or (eqv? c #\@) (memv c '(#\x200C #\x200D)))
           (and (> (char->integer c) 127)
                (memq (char-general-category c)
                      '(Lu Ll Lt Lm Lo Mn Nl No Pd Pc Po Sc Sm Sk So Co))))
       (get-identifier p c #f))
      ((char=? c #\\)                 ;<inline hex escape>
       (let ((c (get-char p)))
         (cond ((eqv? c #\x)
                (get-identifier p (get-inline-hex-escape p) #f))
               (else
                (cond ((eof-object? c)
                       (eof-warning p))
                      (else
                       (reader-warning p "Invalid character following \\")))
                (get-token p)))))
      (else
       (case c
         ((#\() (values 'openp #f))
         ((#\)) (values 'closep #f))
         ;((#\[) (values 'openb #f))
         ;((#\]) (values 'closeb #f))
         ((#\') (values 'abbrev 'quote))
         ((#\`) (values 'abbrev 'quasiquote))
         ((#\,)
          (case (lookahead-char p)
            ((#\@)
             (get-char p)
             (values 'abbrev 'unquote-splicing))
            (else (values 'abbrev 'unquote))))
         ((#\|) (get-identifier p #f 'pipe))
         (else
          (reader-warning p "Invalid leading character" c)
          (get-token p)))))))

;;; Datum reader

;; <datum> → <lexeme datum>
;;          | <compound datum>
;; <lexeme datum> → <boolean> | <number>
;;          | <character> | <string> | <symbol>
;; <symbol> → <identifier>
;; <compound datum> → <list> | <vector> | <bytevector>
;; <list> → (<datum>*) | [<datum>*]
;;          | (<datum>+ . <datum>) | [<datum>+ . <datum>]
;;          | <abbreviation>
;; <abbreviation> → <abbrev prefix> <datum>
;; <abbrev prefix> → ' | ` | , | ,@
;;          | #' | #` | #, | #,@
;; <vector> → #(<datum>*)
;; <bytevector> → #vu8(<u8>*)
;; <u8> → 〈any <number> representing an exact
;;                    integer in {0, ..., 255}〉

(define (get-compound-datum p src terminator type labels)
  (define vec #f)                     ;TODO: ugly, should be rewritten
  (let lp ((head '()) (prev #f) (len 0))
    (let-values (((lextype x) (get-lexeme p)))
      (case lextype
        ((closep closeb eof)
         (unless (eq? lextype terminator)
           (if (eof-object? x)
               (eof-warning p)
               (reader-warning p "Mismatched parenthesis/brackets" lextype x terminator)))
         (case type
           ((vector)
            (let ((s (list->vector head)))
              (set! vec s)
              vec))
           ((list)
            head)
           ((bytevector)
            (apply bytevector head))
           (else
            (reader-error p "Internal error in get-compound-datum" type))))
        ((dot)                          ;a dot like in (1 . 2)
         (cond
           ((eq? type 'list)
            (let*-values (((lextype x) (get-lexeme p))
                          ((d) (handle-lexeme p lextype x labels #t)))
              (let-values (((termtype __) (get-lexeme p)))
                (cond ((eq? termtype terminator))
                      ((eq? termtype 'eof)
                       (eof-warning p))
                      (else
                       (reader-warning p "Improperly terminated dot list"))))
              (cond ((pair? prev)
                     (cond ((reference? d)
                            (register-reference p labels d
                                                (lambda (d) (set-cdr! prev d))))
                           (else
                            (set-cdr! prev d))))
                    (else
                     (reader-warning p "Unexpected dot")))
              head))
           (else
            (reader-warning p "Dot used in non-list datum")
            (lp head prev len))))
        (else
         (let ((d (handle-lexeme p lextype x labels #t)))
           (cond
             ((and (eq? type 'bytevector)
                   (or (reference? d)
                       (not (and (integer? (reference-label d)) (<= 0 d 255)))))
              (reader-warning p "Invalid datum in bytevector" x)
              (lp head prev len))
             (else
              (let ((new-prev (cons d '())))
                (when (pair? prev)
                  (set-cdr! prev new-prev))
                (when (reference? d)
                  (register-reference p labels d
                                      (if (eq? type 'vector)
                                          (lambda (d)
                                            (vector-set! vec len d))
                                          (lambda (d)
                                            (set-car! new-prev d)))))
                (if (pair? head)
                    (lp head new-prev (+ len 1))
                    (lp new-prev new-prev (+ len 1))))))))))))

(define (handle-lexeme p lextype x labels allow-refs?)
  (let ((src (reader-source p)))
    (case lextype
      ((openp)
       (get-compound-datum p src 'closep 'list labels))
      ((vector)
       (get-compound-datum p src 'closep 'vector labels))
      ((bytevector)
       ;; TODO: open-bytevector-output-port would be faster
       (get-compound-datum p src 'closep 'bytevector labels))
      ((value eof identifier)
       (annotate lextype src x))
      ((abbrev)
       (let-values (((type lex) (get-lexeme p)))
         (cond ((eq? type 'eof)
                (eof-warning p)
                  lex)
               (else
                (let ((d (handle-lexeme p type lex labels #t)))
                  (list (annotate 'identifier src x) d))))))
      ((label)
       ;; The object that follows this label can be referred
       ;; back from elsewhere.
       (let*-values (((lextype lexeme) (get-lexeme p))
                     ((d) (handle-lexeme p lextype lexeme labels allow-refs?)))
         (register-label p labels x d)
        d))
      (else
       (cond ((and allow-refs? (eq? lextype 'reference))
              (make-reference x))
             (else
              ;; Ignore the shebang ("#!/" or "#! " at the start of files).
              ;; FIXME: should only work for programs.
              (unless (and (eq? lextype 'shebang) (eqv? (car x) 1) (eqv? (cadr x) 0))
                (reader-warning p "Unexpected lexeme" lextype x))
              (call-with-values 
                (lambda () (get-lexeme p))
                (lambda (lextype x) (handle-lexeme p lextype x labels allow-refs?)))))))))

;;; Shared/circular data

(define (make-labels)
  (make-hash-table eq?))

(define (register-label p labels label datum)
  (when labels
    (hash-table-update!/default labels label (lambda (old)
                                      (when (car old)
                                        (reader-warning p "Duplicate label" label))
                                      (cons datum (cdr old)))
                       (cons #f '()))))

(define (register-reference _p labels reference setter)
  (when labels
    (hash-table-update!/default labels (reference-label reference) (lambda (old)
                                      (cons (car old)
                                            (cons setter (cdr old))))
                       (cons #f '()))))

(define (resolve-labels p labels)
  (let ((entries (hash-table->alist labels)))
    (for-each
     (lambda (entry)
       (let ((id (car entry))
             (datum (cadr entry))
             (refs (cddr entry)))
         (unless datum
           (reader-warning p "Missing label" id))
         (for-each (lambda (ref) (ref datum))
                   refs)))
     entries)))

))
