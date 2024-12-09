;; This day, we shall search for the word XMAS: it can be written in
;; all directions & ways, & letters can be reused:
;; ··X···
;; ·SAMX·
;; ·A··A·
;; XMAS·S
;; ·X····
;; The task is to find how many times XMAS appears.
;; As the input is bidirectional & we need to access elements across
;; both directions, a 2D data structure seems appropriate. Time to
;; learn about them in Scheme! Alternatively, I could manually make
;; a 2D data structure by using row*width + col to access an element.
;; Also, an edge case is near the border: XMAS cannot be written
;; upwards if the X is on the first line. A simple approach is to add
;; padding all around the edges, like three rows or columns full of
;; Z all around our input.
;; From there, the naive solution is to look for an X & check all 8
;; directions for the word XMAS.
(import (scheme base)
        (utils)
        (srfi 1) ;; list: count
        (srfi 130)) ;; cursor-strings

;;(define lines (read-input-file "day4-example"))
(define lines (read-input-file "day4-input"))
(define width (+ 6 (string-length (car lines)))) ;; include padding

(define (pad-line line)  (string-append "ZZZ" line "ZZZ"))
(define (pad-vertical lines)
  (append (list (make-string (* 3 width) #\Z))
          lines
          (list (make-string (* 3 width) #\Z))))

(define text (apply string-append (pad-vertical (map pad-line lines))))

(define start (string-cursor-forward text
                                     (string-cursor-start text)
                                     (+ (* 3 width) 3)))
(define end (string-cursor-back text
                                (string-cursor-end text)
                                (+ (* 3 width) 3)))

(define (at pos) (string-ref/cursor text pos))

(define (up pos) (string-cursor-next text pos))
(define (down pos) (string-cursor-prev text pos))
(define (left pos) (string-cursor-back text pos width))
(define (right pos) (string-cursor-forward text pos width))

(define (up-right pos) (up (right pos)))
(define (up-left pos) (up (left pos)))
(define (down-right pos) (down (right pos)))
(define (down-left pos) (down (left pos)))

(define directions (list up down left right
                         up-right up-left down-right down-left))

(define (-MAS? pos next)
  (and (char=? #\M (at (next pos)))
       (char=? #\A (at (next (next pos))))
       (char=? #\S (at (next (next (next pos)))))))
(define (count-XMAS pos)
  (if (not (char=? #\X (at pos)))
      0
      (count (lambda (dir) (-MAS? pos dir))
             directions)))

(let ((score 0))
  (let ((f (lambda (pos) (set! score (+ score (count-XMAS pos))))))
    (string-for-each-cursor f text start end)
    (displayln score)))


;; Part two is to find X-MAS, as in a cross of two MAS, rather than
;; the word XMAS:
;; M·S
;; ·A·
;; M·S
;; Now, we'll focus on finding As & determine wether they're part of
;; X-MAS.
;; Note: we now only need one Z of padding all around our input, but
;; I cannot be bothered to modify my code.
(define (X-MAS? pos)
  (and (char=? #\A (at pos))
       (and (or (and (char=? #\M (at (up-left pos)))
                     (char=? #\S (at (down-right pos))))
                (and (char=? #\S (at (up-left pos)))
                     (char=? #\M (at (down-right pos)))))
            (or (and (char=? #\M (at (up-right pos)))
                     (char=? #\S (at (down-left pos))))
                (and (char=? #\S (at (up-right pos)))
                     (char=? #\M (at (down-left pos))))))))

(let ((score 0))
  (let ((f (lambda (pos) (if (X-MAS? pos) (set! score (+ score 1))))))
    (string-for-each-cursor f text start end)
    (displayln score)))
