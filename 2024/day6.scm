# Today's task is a navigation one: a guard starts on a map pointing
# in a direction, then moves forward, marking his path with X's,
# & whenever he hits an obstacle, indicated by #'s, he turns right
# & continues. When he leaves the map, we need to count the X's.
# . . . .
# . . # .
# . . . .
# . . ^ .
# . . . .
# In this example, he would go up once, then hit the obstale, turn
# right, advance, then leave the map: including the start position,
# that's 3 X's.
# We may reuse part of the code for day4, where we used cursor-strings
# library SRFI 130.
(import (scheme base)
        (utils)
        (srfi 1) ;; list: count
        (srfi 130)) ;; cursor-strings

(define lines (read-input-file "day6-example"))
;;(define lines (read-input-file "day6-input"))
(define width (string-length (car lines)))

(define grid (apply string-append lines))

(define (at pos) (string-ref/cursor grid pos))

(define (up pos) (string-cursor-next grid pos))
(define (down pos) (string-cursor-prev grid pos))
(define (left pos) (string-cursor-back grid pos width))
(define (right pos) (string-cursor-forward grid pos width))
