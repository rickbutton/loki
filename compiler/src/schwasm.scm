(import (scheme base))
(import (scheme write))
(import (scheme read))
(import (scheme file))
(import (scheme repl))
(import (scheme process-context))
(import (util))

(import (p01_scheme2cps))
(import (p02_markvars))
(import (p03_flattencps))
(import (p04_closes2funcs))
(import (p05_lift_rodatas))
(import (p06_funcs2wat))

(import (srfi 159))
(import (chibi show pretty))

(define (print-help-and-exit) 
        (display "arguments: schwasm.scm [input.scm] [out.wat]")
        (exit))

(if (not (eq? (length (command-line)) 3))
    (print-help-and-exit))

(define input-file (list-ref (command-line) 1))
(define output-file (list-ref (command-line) 2))

(define (read-all)
    (let ((o (read)))
        (if (eof-object? o) '() (cons o (read-all)))))

(define (read-input-file)
    (with-input-from-file input-file read-all))

(define (write-file output file)
    (if (file-exists? file) (delete-file file))
    (let ((p (open-output-file file)))
        (show p (pretty output))
        (close-output-port p)))

(define (compile prog)
    (let ((p01 (p01_scheme2cps prog)))
    (let ((p02 (p02_markvars p01)))
    (let ((p03 (p03_flattencps p02)))
    (let ((p04 (p04_closes2funcs p03)))
    (let ((p05 (p05_lift_rodatas p04)))
    (display (show #f (pretty p05)))
    (let ((p06 (p06_funcs2wat p05)))
    ;(display (show #f (pretty p06)))
        p06)))))))

(define (main)
    (let* ((input (read-input-file))
           (wat (compile input)))
            (write-file wat output-file)))

(main)