(define-library (loki compiler macro)
(import (scheme base))
(import (loki util))
(import (srfi 128))
(import (srfi 146 hash))
(export macro?
        macro-type
        macro-proc
        macro-source
        make-expander
        make-transformer
        binding-name->macro
        register-macro!)
(begin

;; global table mapping <binding name> of keyword to <macro> object
(define *macro-table*      (hashmap (make-default-comparator)))

;;=========================================================================
;;
;; Macro objects:
;;
;;=========================================================================

;; Expanders are system macros that fully expand
;; their arguments to core Scheme, while
;; transformers and variable transformers are
;; user macros.

;; <type> ::= expander | transformer | variable-transformer

(define-record-type <macro>
  (make-macro type proc source)
  macro?
  (type macro-type)
  (proc macro-proc)
  (source macro-source))

(define (make-expander proc source)             (make-macro 'expander proc source))
(define (make-transformer proc source)          (make-macro 'transformer proc source))

;; Returns <macro>.
(define (binding-name->macro binding-name t)
  (hashmap-ref *macro-table*
               binding-name
               (lambda () (raise "Reference to macro keyword out of context"))))

;; Registering macro.
(define (register-macro! binding-name procedure-or-macro)
  (set! *macro-table* (hashmap-set *macro-table* binding-name procedure-or-macro)))

))
