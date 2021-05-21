(define-library (loki compiler features)
  (import (scheme base))
  (export loki-features feature?)
  (begin
   
   ;;===================================================================
   ;;
   ;; Language features, for cond-expand.
   ;;
   ;;===================================================================
   
   (define static-features '(
                             loki
                             wasm
                             r7rs
                             posix))
   (define (loki-features) (list-copy static-features))
   (define (feature? feature)
     (and (symbol? feature) (member feature static-features)))
   
   ))
