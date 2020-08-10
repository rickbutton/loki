(define-library (srfi 146 gleckler hamt-map-test)
  (import (scheme base)
	  (loki test)
	  (srfi 146 gleckler hamt)
	  (srfi 146 gleckler hamt-map)
	  (srfi 146 gleckler hamt-misc)
	  (only (srfi 1) alist-delete fold)
	  (only (srfi 27) random-integer)
	  (only (srfi 113)
                set
		set-adjoin!
		set-delete!
		set-for-each)
	  (only (srfi 69)
		hash-table->alist
		hash-table-keys
		hash-table-delete!
		hash-table-walk
		hash-table-set!
		hash-table-size
		string-hash)
          (only (srfi 128) make-comparator)
	  (only (srfi 95) sort)
	  (only (srfi 151) bit-count))
  (export run-hamt-map-tests)
  (begin
    (define (list-sort < ls) (sort ls <))
    (define (hash-table-for-each proc hash-table)
      (hash-table-walk hash-table proc)))
  (include "hamt-map-test.scm"))

