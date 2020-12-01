(define-library (scheme base)
    (import (for (except (loki core primitives) environment eval load) run expand)
            (for (loki core let)                     expand run)
            (for (loki core control)                 expand run)
            (for (loki core records)                 expand run)
            (for (loki core derived)                 expand run)
            (for (loki core quasiquote)              expand run)
            (for (loki core let-values)              expand run)
            (for (loki core syntax-rules)            expand run) 
            (for (loki core number)                  expand run)
            (for (loki core bool)                    expand run)
            (for (loki core list)                    expand run)
            (for (loki core vector)                  expand run)
            (for (loki core string)                  expand run)
            (for (loki core char)                    expand run)
            (for (loki core cond-expand)             expand run)
            (for (loki core values)                  expand run)
            (for (loki core apply)                   expand run)
            (for (loki core math)                    expand run)
            (for (loki core string)                  expand run)
            (for (loki core exception)               expand run)
            (for (loki core syntax-error)            expand run)
            (for (loki core dynamic)                 expand run)
            (for (loki core io)                      expand run))
    (export 
          * + - ... / < <= = => > >= _ abs and append apply assoc assq
          assv begin binary-port?  boolean=?  boolean?  bytevector
          bytevector-append bytevector-copy bytevector-copy! bytevector-length
          bytevector-u8-ref bytevector-u8-set!  bytevector?  caar cadr
          call-with-current-continuation call-with-port call-with-values call/cc
          car case cdar cddr cdr ceiling char->integer char-ready?  char<=?
          char<?  char=?  char>=?  char>?  char?  close-input-port
          close-output-port close-port complex?  cond cond-expand
          cons current-error-port current-input-port current-output-port
          define define-record-type define-syntax define-values
          denominator do
          dynamic-wind else eof-object?  equal?  error error-object-message
          even?  exact-integer-sqrt exact?  features
          floor floor-remainder
          flush-output-port gcd get-output-string if include-ci inexact?
          input-port?  integer?  lcm let let*-values let-values letrec* list
          list->vector list-ref list-tail make-bytevector make-parameter
          make-vector max memq min negative?  not number->string numerator
          open-input-bytevector open-output-bytevector or output-port?
          parameterize peek-u8 positive?  quasiquote quotient raise-continuable
          rationalize read-bytevector!  read-error?  read-string reverse
          set!  set-cdr!  string string->number string->utf8 string-append
          eof-object eq?  eqv?  error-object-irritants error-object?  exact
          exact-integer?  expt file-error?  floor-quotient floor/ for-each
          get-output-bytevector guard include inexact input-port-open?
          integer->char lambda length let* let-syntax
          letrec letrec-syntax
          list->string list-copy list-set!  list?  make-list make-string map
          member memv modulo newline null?  number?  odd?  open-input-string
          open-output-string output-port-open?  pair?  peek-char port?
          procedure?  quote raise rational?  read-bytevector read-char read-line
          read-u8 real? remainder round set-car!  square string->list string->symbol
          string->vector string-copy string-copy!  string-for-each string-map
          string-set!  string<?  string>=?  string?  symbol->string symbol?
          syntax-rules truncate truncate-remainder u8-ready?  unquote
          utf8->string vector vector->string vector-copy vector-fill!
          vector-length vector-ref vector?  with-exception-handler
          write-char write-u8 string-fill!  string-length string-ref string<=?
          string=?  string>?  substring symbol=?  syntax-error textual-port?
          truncate-quotient truncate/ unless unquote-splicing values
          vector->list vector-append vector-copy!  vector-for-each vector-map
          vector-set!  when write-bytevector write-string zero?))

