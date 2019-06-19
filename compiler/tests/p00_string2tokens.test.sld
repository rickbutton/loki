(define-library 
    (p00_string2tokens.test)
    (import (scheme base))
    (import (scheme write))
    (import (srfi 159))
    (import (unit))
    (import (shared))
    (import (p00_string2tokens))
    (export test_p00_string2tokens)
(begin

(define-syntax check-with-name
    (syntax-rules ()
        ((check name in out)
            (test-equal name out (p00_string2tokens (open-input-string in))))))

(define-syntax check
    (syntax-rules ()
        ((check in out)
            (test-equal in out (p00_string2tokens (open-input-string in))))))

(define-syntax check-error
    (syntax-rules ()
        ((check-error in)
            (test-error in (p00_string2tokens (open-input-string in))))))

(define (t string type value)
    (make-token string type value (make-source-location 1 1 0)))
(define (tl string type line col offset value)
    (make-token string type value (make-source-location line col offset)))

(define (test_p00_string2tokens)
    (test-group "p00_string2tokens"
        (check-with-name "lf" "123\n456" (list (t "123" 'number 123) (tl "456" 'number 2 1 4 456)))
        (check-with-name "cr" "123\r456" (list (t "123" 'number 123) (tl "456" 'number 2 1 4 456)))
        (check-with-name "crlf" "123\r\n456" (list (t "123" 'number 123) (tl "456" 'number 2 1 5 456)))
        (check-with-name "lfcr" "123\n\r456" (list (t "123" 'number 123) (tl "456" 'number 3 1 5 456)))
        (check-with-name "crcr" "123\r\r456" (list (t "123" 'number 123) (tl "456" 'number 3 1 5 456)))
        (check-with-name "lflf" "123\n\n456" (list (t "123" 'number 123) (tl "456" 'number 3 1 5 456)))
        (check-with-name "lfcrlf" "123\n\r\n456" (list (t "123" 'number 123) (tl "456" 'number 3 1 6 456)))
        (check-with-name "crlflf" "123\r\n\n456" (list (t "123" 'number 123) (tl "456" 'number 3 1 6 456)))

        (check "identifier" (list (t "identifier" 'id 'identifier)))
        (check "!id" (list (t "!id" 'id '!id)))
        (check "+" (list (t "+" 'id '+)))
        (check "+id" (list (t "+id" 'id '+id)))
        (check "+.id" (list (t "+.id" 'id '+.id)))
        (check "+..id" (list (t "+..id" 'id '+..id)))
        (check "|+0id|" (list (t "|+0id|" 'id '|+0id|)))
        (check-error "0id")
        (check-error "+0id")

        (map 
            (lambda (in value)
                (let ((with-prefix (string-append "#d" in)))
                    (check in (list (t in 'number value)))
                    (check with-prefix 
                        (list (t with-prefix 'number value)))))
            '("123" "#i123" "#e123" "123+456i" "-123+456i" 
              "123-456i" "-123-456i" "123+i" "123-i" "123-inf.0i" "123+inf.0i" "+123i" "-123i")
            '(123 #i123 #e123 123+456i -123+456i
              123-456i -123-456i 123+i 123-i 123-inf.0i 123+inf.0i +123i -123i))
        (map 
            (lambda (in value) (check in (list (t in 'number value))))
            '("#b01" "#o01234567" "#x01234567890abcdefABCDEF" "#xDEADBEEF")
            '(#b01 #o01234567 #x01234567890abcdefABCDEF #xDEADBEEF))
        (map 
            (lambda (in value) (check in (list (t in 'number value))))
            '("+inf.0" "-inf.0" "+nan.0" "-nan.0" "+inf.0i" "-inf.0i" "+i" "-i")
            '(+inf.0 -inf.0 +nan.0 -nan.0 +inf.0i -inf.0i +i -i))

        (map 
            (lambda (in) (check-error in))
            '("#b1012" "#b2" "#b456"
              "#o8" "#o9" "#oA"
              "#dDEAD #ddead"
              "#xZZZ"))

        (check "(" (list (t "(" 'lparen #f)))
        (check ")" (list (t ")" 'rparen #f)))

        (check "()" 
            (list (t "(" 'lparen #f)
                  (tl ")" 'rparen 1 2 1 #f)))

        (check "#t" (list (t "#t" 'boolean #t)))
        (check "#true" (list (t "#true" 'boolean #t)))
        (check "#f" (list (t "#f" 'boolean #f)))
        (check "#false" (list (t "#false" 'boolean #f)))

        (check "#t #f #true #false" 
            (list 
                (tl "#t" 'boolean 1 1 0 #t)
                (tl "#f" 'boolean 1 4 3 #f)
                (tl "#true" 'boolean 1 7 6 #t)
                (tl "#false" 'boolean 1 13 12 #f)))

        (check-error "#a")
        (check-error "#ture")
        (check-error "#truea")
        (check-error "#fasle")
        (check-error "#falsea")

        (check "#\\a" (list (t "#\\a" 'char #\a)))

        (map (lambda (name value)
            (check name (list (t name 'char value))))
            '("#\\alarm" "#\\backspace" "#\\delete" "#\\escape" "#\\newline" "#\\null" "#\\return" "#\\space" "#\\tab")
            '(#\alarm #\backspace #\delete #\escape #\newline #\null #\return #\space #\tab))

        (check-error "#\\fake")
        (check-error "#\\alarma")
        (check-error "#\\spacet")

        (check "#\\x123" (list (t "#\\x123" 'char #\x123)))
        (check "#\\xABC" (list (t "#\\xABC" 'char #\xABC)))
        (check "#\\xabc" (list (t "#\\xabc" 'char #\xabc)))
        (check-error "#\\xaq")

        (check "\"this is a test\"" (list (t "\"this is a test\"" 'string "this is a test")))

        (check-error "\"this is a test")

        (check "(#f)" 
            (list 
                (tl "(" 'lparen 1 1 0 #f)
                (tl "#f" 'boolean 1 2 1 #f)
                (tl ")" 'rparen 1 4 3 #f)))

        (check "( #f )"
            (list 
                (tl "(" 'lparen 1 1 0 #f)
                (tl "#f" 'boolean 1 3 2 #f)
                (tl ")" 'rparen 1 6 5 #f)))

        (check "(one . two)" 
            (list 
                (tl "(" 'lparen 1 1 0 #f)
                (tl "one" 'id 1 2 1 'one)
                (tl "." 'dot 1 6 5 #f)
                (tl "two" 'id 1 8 7 'two)
                (tl ")" 'rparen 1 11 10 #f)))

        (check "#()"
            (list 
                (tl "#(" 'lvector 1 1 0 #f)
                (tl ")" 'rparen 1 3 2 #f)))

        (check "#u8()"
            (list 
                (tl "#u8(" 'lbytevector 1 1 0 #f)
                (tl ")" 'rparen 1 5 4 #f)))

        (check "'" (list (t "'" 'quote #f)))
        (check "'123" (list 
            (tl "'" 'quote 1 1 0 #f)
            (tl "123" 'number 1 2 1 123)))

        (check "'(123)" (list 
            (tl "'" 'quote 1 1 0 #f)
            (tl "(" 'lparen 1 2 1 #f)
            (tl "123" 'number 1 3 2 123)
            (tl ")" 'rparen 1 6 5 #f)))

        (check "'#(123)" (list 
            (tl "'" 'quote 1 1 0 #f)
            (tl "#(" 'lvector 1 2 1 #f)
            (tl "123" 'number 1 4 3 123)
            (tl ")" 'rparen 1 7 6 #f)))

        (check "'#u8(123)" (list 
            (tl "'" 'quote 1 1 0 #f)
            (tl "#u8(" 'lbytevector 1 2 1 #f)
            (tl "123" 'number 1 6 5 123)
            (tl ")" 'rparen 1 9 8 #f)))

        (check "`(+ ,one ,two)" (list 
            (tl "`" 'quasiquote 1 1 0 #f)
            (tl "(" 'lparen 1 2 1 #f)

            (tl "+" 'id 1 3 2 '+)

            (tl "," 'unquote 1 5 4 #f)
            (tl "one" 'id 1 6 5 'one)

            (tl "," 'unquote 1 10 9 #f)
            (tl "two" 'id 1 11 10 'two)
            (tl ")" 'rparen 1 14 13 #f)))

        (check "`(+ ,@one ,two)" (list 
            (tl "`" 'quasiquote 1 1 0 #f)
            (tl "(" 'lparen 1 2 1 #f)

            (tl "+" 'id 1 3 2 '+)

            (tl ",@" 'unquote-splice 1 5 4 #f)
            (tl "one" 'id 1 7 6 'one)

            (tl "," 'unquote 1 11 10 #f)
            (tl "two" 'id 1 12 11 'two)
            (tl ")" 'rparen 1 15 14 #f)))
    ))
))