(import (scheme base))
(import (scheme write))

(define |Völuspá|
  (list "An ash I know there stands,"
        "Yggdrasill is its name,"
        "a tall tree, showered"
        "with shining loam."
        "From there come the dews"
        "that drop in the valleys."
        "It stands forever green over"
        "Urðr's well."))

(for-each
  (lambda (line)
    (display line)
    (newline))
  |Völuspá|)

