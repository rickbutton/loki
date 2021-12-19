(define-library (loki path)
  (import (scheme base))
  (import (srfi 1))
  (import (loki core fs))
  (import (loki util))
  (export
   make-posix-path
   make-windows-path
   make-path
   wrap-path
   path?
   posix-path?
   windows-path?
   path-absolute?
   path-reserved?
   path=?
   path-drive
   path-root
   path-anchor
   path-parts
   path-parent
   path-filename
   path-suffix
   path-suffixes
   path-stem
   path->string
   path->slashed-string
   path->file-uri
   path-join
   path-match
   path-relative-to
   path-with-name
   path-with-suffix
   current-working-path)
  (begin
   
   (define (drop-last list)
     (cond
      ((null? (cdr list)) '())
      ((pair? list) (cons (car list) (drop-last (cdr list))))))
   
   (define (string-split str ch)
     (let ((len (string-length str)))
       (letrec
           ((split
             (lambda (a b)
               (cond
                ((>= b len) (if (= a b) '() (cons (substring str a b) '())))
                ((char=? ch (string-ref str b)) (if (= a b)
                                                    (split (+ 1 a) (+ 1 b))
                                                  (cons (substring str a b) (split b b))))
                (else (split a (+ 1 b)))))))
         (split 0 0))))
   
   
   
   
   (define-record-type <posix-path>
     (make-posix-path-record root parts)
     posix-path?
     (root posix-path-root)
     (parts posix-path-parts))
   
   (define-record-type <windows-path>
     (make-windows-path-record drive root parts)
     windows-path?
     (drive windows-path-drive)
     (root windows-path-root)
     (parts windows-path-parts))
   
   (define (starts-with? string prefix)
     (if (< (string-length string) (string-length prefix))
         #f
       (equal? prefix (substring string 0 (string-length prefix)))))
   
   (define (make-posix-path string)
     (let ((strlen (string-length string)))
       (if (zero? strlen)
           (make-posix-path-record "" '())
         (let ((root (if (equal? (string-ref string 0) #\/)
                         (if (and (> strlen 1)
                                  (equal? (string-ref string 1) #\/))
                             "//"
                           "/")
                       ""))
               (parts (string-split string #\/)))
           (make-posix-path-record root parts)))))
   
   (define (make-windows-path string)
     (error "make-windows-path: windows paths not implemented" string))
   
   (define (make-path-from-parts drive root parts)
     (cond-expand
      (windows (error "make-path-from-parts: windows paths not implemented"))
      (else (make-posix-path-record root parts))))
   
   (define (make-path string)
     (cond-expand
      (windows (make-windows-path string))
      (else (make-posix-path string))))
   
   (define (path? path)
     (or (posix-path? path) (windows-path? path)))
   
   (define (path-absolute? path)
     (unless (path? path) (error "path-absolute?: not a path" path))
     (and (eq? "" (path-drive path))
          (eq? "" (path-root path))))
   
   (define (path-reserved? path)
     (error "path-reserved?: not implemented" path))
   
   (define (path=? path1 path2)
     (equal? (path-parts path1) (path-parts path2)))
   
   (define (path-drive path)
     (cond
      ((posix-path? path) "")
      ((windows-path? path) (windows-path-drive path))
      (else (error "path-drive: not a path" path))))
   
   (define (path-root path)
     (cond
      ((posix-path? path) (posix-path-root path))
      ((windows-path? path) (windows-path-root path))
      (else (error "path-root: not a path" path))))
   
   (define (path-anchor path)
     (cond
      ((posix-path? path) (posix-path-root path))
      ((windows-path? path)
       (string-append (windows-path-drive path) (windows-path-root path)))
      (else (error "path-anchor: not a path" path))))
   
   (define (path-components path)
     (cond
      ((posix-path? path) (posix-path-parts path))
      ((windows-path? path) (windows-path-parts path))
      (else (error "path-components: not a path" path))))
   
   (define (path-parts path)
     (cond
      ((posix-path? path)
       (cons (posix-path-root path) (posix-path-parts path)))
      ((windows-path? path)
       (cons (path-anchor path) (windows-path-parts path)))
      (else (error "path-parts: not a path" path))))
   
   (define (path-parent path)
     (cond
      ((posix-path? path)
       (let ((parts (posix-path-parts path)))
         (if (null? parts)
             path
           (make-posix-path-record (path-root path) (drop-last parts)))))
      ((windows-path? path)
       (let ((parts (windows-path-parts path)))
         (if (null? parts)
             path
           (make-windows-path-record (path-drive path) (path-root path) (drop-last parts)))))
      (else (error "path-parent: not a path" path))))
   
   (define (path-filename path)
     (unless (path? path) (error "path-filename: not a path" path))
     (let ((parts (path-parts path)))
       (if (null? (cdr parts))
           ""
         (last (cdr parts)))))
   
   (define (path-suffix path)
     (unless (path? path) (error "path-suffix: not a path" path))
     (let ((filename (path-filename path)))
       (if (equal? filename "")
           ""
         (let ((parts (string-split filename #\.)))
           (if (null? (cdr parts))
               ""
             (last parts))))))
   
   (define (path-suffixes path)
     (unless (path? path) (error "path-suffixes: not a path" path))
     (let ((filename (path-filename path)))
       (if (equal? filename "")
           '()
         (let ((parts (string-split filename #\.)))
           (cdr parts)))))
   
   (define (path-stem path)
     (unless (path? path) (error "path-stem: not a path" path))
     (let ((filename (path-filename path)))
       (if (equal? filename "")
           ""
         (let ((parts (string-split filename #\.)))
           (if (null? parts)
               ""
             (car parts))))))
   
   (define (path->string* path sep)
     (let ((parts (path-parts path)))
       (if (> (string-length (car parts)) 0)
           (string-join parts sep)
         (string-join (cdr parts) sep))))
   
   (define *posix-sep* "/")
   (define *windows-sep* "\\")
   
   (define (path->string path)
     (cond
      ((posix-path? path) (path->string* path *posix-sep*))
      ((windows-path? path) (path->string* path *windows-sep*))
      ((string? path) path)
      (else (error "path->string: not a path" path))))
   
   (define (path->slashed-string path)
     (unless (path? path) (error "path->slashed-string: not a path" path))
     (path->string* path *posix-sep*))
   
   (define (wrap-path path-or-string)
     (cond
      ((path? path-or-string) path-or-string)
      ((string? path-or-string) (make-path path-or-string))
      (else (error "wrap-path: path-or-string not a path or string" path-or-string))))
   
   (define (path-join2 path path-or-string)
     (unless (path? path) (error "path-join: not a path" path))
     (let* ((path2 (wrap-path path-or-string))
            (path2-drive (path-drive path2))
            (path2-root (path-root path2))
            (path2-components (path-components path2)))
       (if (> (string-length path2-drive) 0)
           path2
         (if (> (string-length path2-root) 0)
             (make-path-from-parts (path-drive path)
                                   path2-root
                                   path2-components)
           (make-path-from-parts  (path-drive path)
                                 (path-root path)
                                 (append (path-components path) path2-components))))))
   
   (define (path-join path path-or-string . paths-or-strings)
     (if (null? paths-or-strings)
         (path-join2 path path-or-string)
       (apply path-join (path-join2 path path-or-string) paths-or-strings)))
   
   (define (path->file-uri path) (error "path->file-uri: not implemented"))
   (define (path-match path string) (error "path-match: not implemented"))
   (define (path-relative-to path1 path2) (error "path-relative-to: not implemented"))
   
   (define (path-with-name path name)
     (unless (path? path) (error "path-with-name: not a path" path))
     (let ((filename (path-filename path)))
       (if (eq? filename "") (error "path-with-name: path does not have a name" path))
       (path-join (path-parent path) name)))
   
   (define (path-with-suffix path suffix)
     (unless (path? path) (error "path-with-suffix: not a path" path))
     (let ((stem (path-stem path))
           (suffixes (path-suffixes path)))
       (if (eq? stem "") (error "path-with-suffix: path does not have a name" path))
       (if (null? suffixes)
           (path-with-name path (string-join (list stem suffix) "."))
         (path-with-name path (string-join (append (cons stem (drop-right suffixes 1)) (list suffix)) ".")))))
   
   (define (current-working-path)
     (make-path (current-directory)))
   
   ))
