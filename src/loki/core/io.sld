(define-library (loki core io)
  (import (loki core primitives))
  (import (loki core let))
  (import (loki core derived))
  (import (loki core intrinsics))
  (import (loki core string))
  (import (loki core char))
  (import (loki core vector))
  (import (loki core list))
  (import (loki core case-lambda))
  (import (loki core dynamic))
  (import (loki core exception))
  (import (loki core records))
  (import (loki core bool))
  (import (for (loki core syntax-rules) expand))
  (import (rename (loki core intrinsics) (%port? port?)
                  (%delete-file delete-file)
                  (%file-exists? file-exists?)
                  (%input-port-open? input-port-open?)
                  (%output-port-open? output-port-open?)
                  (%close-output-port close-output-port)
                  (%close-input-port close-input-port)
                  (%eof-object eof-object)
                  (%eof-object? eof-object?)
                  (%get-output-string get-output-string)
                  (%get-output-bytevector get-output-bytevector)
                  (%open-output-string open-output-string)
                  (%open-input-string open-input-string)
                  (%open-input-bytevector open-input-bytevector)
                  (%open-output-bytevector open-output-bytevector)
                  (%open-output-file open-output-file)
                  (%open-input-file open-input-file)
                  (%open-binary-input-file open-binary-input-file)
                  (%open-binary-output-file open-binary-output-file)
                  (%flush-output-port flush-output-port)))
  (export eof-object? eof-object
          port? binary-port? textual-port? input-port? output-port?
          char-ready? u8-ready? input-port-open? output-port-open?
          flush-output-port delete-file file-exists?
          close-input-port close-output-port close-port
          call-with-port call-with-input-file call-with-output-file
          newline get-output-string get-output-bytevector
          open-output-string open-input-string open-input-bytevector
          open-output-bytevector open-output-file open-input-file
          open-binary-input-file open-binary-output-file
          current-error-port current-input-port current-output-port
          with-input-from-file with-output-to-file
          peek-char peek-u8
          read-bytevector! read-bytevector read-string read-char
          read-line read-u8 write-bytevector write-string write-char
          write-u8)
  (begin
   
   ; TODO - i should make these all throw on invalid args
   ; for now, I'm too lazy
   
   (define-syntax
     to-bool
     (syntax-rules ()
                   ((to-bool e)
                    (if e #t #f))))
   
   (define (binary-port? port) (to-bool (and (port? port) (eq? (%port-type port) 'binary))))
   (define (textual-port? port) (to-bool (and (port? port) (eq? (%port-type port) 'textual))))
   
   (define (input-port? port) (to-bool (and (port? port) (%port-input port))))
   (define (output-port? port) (to-bool (and (port? port) (%port-output port))))
   
   (define (char-ready? port) (to-bool (and (textual-port? port) (%port-ready? port))))
   (define (u8-ready? port) (to-bool (and (binary-port? port) (%port-ready? port))))
   
   (define (close-port port)
     (close-input-port port)
     (close-output-port port))

   (define (call-with-port port proc)
     (let ((res (proc port)))
          (close-port port)
          res))
   
   (define (call-with-input-file file proc)
     (let* ((in (open-input-file file))
            (res (proc in)))
           (close-input-port in)
           res))
   
   (define (call-with-output-file file proc)
     (let* ((out (open-output-file file))
            (res (proc out)))
           (close-output-port out)
           res))
   
   (define (newline . o)
     (write-char #\newline (if (pair? o) (car o) (current-output-port))))
   
   (define current-error-port (make-parameter (%stderr)
                                              (lambda (port)
                                                      (if (output-port? port)
                                                          port
                                                          (raise "current-error-port: not an output port")))))
   
   (define current-input-port (make-parameter (%stdin)
                                              (lambda (port)
                                                      (if (input-port? port)
                                                          port
                                                          (raise "current-input-port: not an input port")))))
   
   (define current-output-port (make-parameter (%stdout)
                                               (lambda (port)
                                                       (if (output-port? port)
                                                           port
                                                           (raise "current-output-port: not an output port")))))
   
   (define (with-input-from-file string thunk)
     (parameterize ((current-input-port (open-input-file string)))
                   (thunk)
                   (close-port (current-input-port))))
   
   (define (with-output-to-file string thunk)
     (parameterize ((current-output-port (open-output-file string)))
                   (thunk)
                   (close-port (current-output-port))))
   
   (define read-bytevector! (case-lambda
                             ((bytevector)
                              (read-bytevector! bytevector (current-input-port) 0 (bytevector-length bytevector)))
                             ((bytevector port)
                              (read-bytevector! bytevector port 0 (bytevector-length bytevector)))
                             ((bytevector port start)
                              (read-bytevector! bytevector port start (bytevector-length bytevector)))
                             ((bytevector port start end)
                              (%read-bytevector! bytevector port start end))))
   
   (define peek-char (case-lambda
                      (()
                       (%peek-char (current-input-port)))
                      ((port)
                       (%peek-char port))))
   
   (define peek-u8 (case-lambda
                    (()
                     (%peek-u8 (current-input-port)))
                    ((port)
                     (%peek-u8 port))))
   
   (define read-bytevector (case-lambda
                            ((k)
                             (read-bytevector k (current-input-port)))
                            ((k port)
                             (%read-bytevector k port))))
   
   (define read-string (case-lambda
                        ((k)
                         (read-string k (current-input-port)))
                        ((k port)
                         (%read-string k port))))
   
   (define read-char (case-lambda
                      (()
                       (%read-char (current-input-port)))
                      ((port)
                       (%read-char port))))
   
   (define read-line (case-lambda
                      (()
                       (%read-line (current-input-port)))
                      ((port)
                       (%read-line port))))
   
   (define read-u8 (case-lambda
                    (()
                     (%read-u8 (current-input-port)))
                    ((port)
                     (%read-u8 port))))
   
   (define write-bytevector (case-lambda
                             ((bytevector)
                              (write-bytevector bytevector (current-output-port) 0 (bytevector-length bytevector)))
                             ((bytevector port)
                              (write-bytevector bytevector port 0 (bytevector-length bytevector)))
                             ((bytevector port start)
                              (write-bytevector bytevector port start (bytevector-length bytevector)))
                             ((bytevector port start end)
                              (%write-bytevector bytevector port start end))))
   
   (define write-string (case-lambda
                         ((string)
                          (write-string string (current-output-port) 0 (string-length string)))
                         ((string port)
                          (write-string string port 0 (string-length string)))
                         ((string port start)
                          (write-string string port start (string-length string)))
                         ((string port start end)
                          (%write-string string port start end))))
   
   
   (define write-char (case-lambda
                       ((char)
                        (%write-char char (current-output-port)))
                       ((char port)
                        (%write-char char port))))
   
   (define write-u8 (case-lambda
                     ((byte)
                      (%write-u8 byte (current-output-port)))
                     ((byte port)
                      (%write-u8 byte port))))
   
   ))
