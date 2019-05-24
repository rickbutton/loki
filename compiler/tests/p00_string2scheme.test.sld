(define-library 
    (p00_string2scheme.test)
    (import (scheme base))
    (import (scheme write))
    (import (srfi 159))
    (import (unit))
    (import (shared))
    (import (p00_string2scheme))
    (export test_p00_string2scheme)
(begin

(define-syntax check
    (syntax-rules ()
        ((check name in out)
            (test-equal name out (p00_string2scheme (open-input-string in))))))

(define-syntax check-error
    (syntax-rules ()
        ((check-error name in)
            (test-error name (p00_string2scheme (open-input-string in))))))

(define (t string type line col)
    (make-token string type (make-source-location line col)))

(define (test_p00_string2scheme)
    (test-group "p00_string2scheme"
        (map 
            (lambda (in)
                (let ((with-prefix (string-append "#d" in)))
                    (check (string-append "valid number: " in) in (list (t in 'number 1 1)))
                    (check (string-append "valid number: " with-prefix) with-prefix 
                        (list (t with-prefix 'number 1 1)))))
            '("123" "#i123" "#e123"
              "123@456" "-123@-456" "123+456i" "-123+456i" "123-456i" "-123-456i" 
              "123+i" "123-i" "123-inf.0i" "123+inf.0i" "123-nan.0i" "123+nan.0i"
              "+123i" "-123i" 
              ))
        (map 
            (lambda (in) (check (string-append "valid number: " in) in (list (t in 'number 1 1))))
            '("#b01" "#o01234567" "#x01234567890abcdefABCDEF" "#xDEADBEEF"))
        (map 
            (lambda (in) (check (string-append "valid number: " in) in (list (t in 'number 1 1))))
            '("+inf.0" "-inf.0" "+nan.0" "-nan.0"
              "+inf.0i" "-inf.0i" "+nan.0i" "-nan.0i" "+i" "-i"))

        (map 
            (lambda (in) (check-error (string-append "invalid number: " in) in))
            '("#b1012" "#b2" "#b456"
              "#o8" "#o9" "#oA"
              "#dDEAD #ddead"
              "#xZZZ"))

        (check "left paren" "(" (list (t "(" 'paren 1 1)))
        (check "right paren" ")" (list (t ")" 'paren 1 1)))

        (check "null" 
            "()" 
            (list (t "(" 'paren 1 1)
                  (t ")" 'paren 1 2)))

        (check "#t" "#t" (list (t "#t" 'true 1 1)))
        (check "#true" "#true" (list (t "#true" 'true 1 1)))
        (check "#f" "#f" (list (t "#f" 'false 1 1)))
        (check "#false" "#false" (list (t "#false" 'false 1 1)))

        (check "#t #f #true #false" "#t #f #true #false" 
            (list 
                (t "#t" 'true 1 1)
                (t "#f" 'false 1 4)
                (t "#true" 'true 1 7)
                (t "#false" 'false 1 13)))

        (check-error "#a is invalid" "#a")
        (check-error "#ture is invalid" "#ture")
        (check-error "#truea is invalid" "#truea")
        (check-error "#fasle is invalid" "#fasle")
        (check-error "#falsea is invalid" "#falsea")

        (check "#\\a" "#\\a" (list (t "#\\a" 'char 1 1)))

        (map (lambda (name)
            (check name name (list (t name 'char 1 1))))
            '("#\\alarm"
              "#\\backspace"
              "#\\delete"
              "#\\escape"
              "#\\newline"
              "#\\null"
              "#\\return"
              "#\\space"
              "#\\tab"))

        (check-error "#\\fake is invalid" "#\\fake")
        (check-error "#\\alarma is invalid" "#\\alarma")
        (check-error "#\\spacet is invalid" "#\\spacet")

        (check "#\\x123" "#\\x123" (list (t "#\\x123" 'char 1 1)))
        (check "#\\xABC" "#\\xABC" (list (t "#\\xABC" 'char 1 1)))
        (check "#\\xabc" "#\\xabc" (list (t "#\\xabc" 'char 1 1)))
        (check-error "#\\xaq is invalid" "#\\xaq")

        (check "simple string" 
            "\"this is a test\"" 
            (list (t "\"this is a test\"" 'string 1 1)))

        (check-error "unterminated string"  
            "\"this is a test")

        (check "basic list" "(#f)"
            (list 
                (t "(" 'paren 1 1)
                (t "#f" 'false 1 2)
                (t ")" 'paren 1 4)))

        (check "basic list with spaces" "( #f )"
            (list 
                (t "(" 'paren 1 1)
                (t "#f" 'false 1 3)
                (t ")" 'paren 1 6)))

    ))
))