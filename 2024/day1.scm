;; The first task is to parse a double list, sort each one, compute
;; the absolute difference between each values at a given index, & sum
;; those differences.
;; Example:
;; 3   4
;; 4   3
;; 2   5
;; 1   3
;; 3   9
;; 3   3
;; Which would yield: [1 2 3 3 3 4] & [3 3 3 4 5 9],
;; then 2 + 1 + 0 + 1 + 2 + 5 = 11.
;; The problem seems rather easy, though parsing might be a chore.
(import (scheme base))  ;; R7RS
(import (chibi)) ;; IO
(import (srfi 115))  ;; regexp
(import (srfi 132))  ;; sort

(define (read-all-lines f)
  (let ((rl (read-line f)))
    (if (eof-object? rl)
        '()
        (cons rl (read-all-lines f)))))
(define input-list (call-with-input-file "day1-input" read-all-lines))

(define (extract-numeric s) (regexp-extract '(+ numeric) s))
(define extracted-input (map extract-numeric input-list))

(define left-input (map car extracted-input))
(define right-input (map cadr extracted-input))

(define (parse-sort-input l)
  (list-sort < (map string->number l)))
(define left-sorted (parse-sort-input left-input))
(define right-sorted (parse-sort-input right-input))

(define (difference a b) (abs (- a b)))
(define (sum-differences ls rs)
  (apply + (map difference ls rs)))

(display (sum-differences left-sorted right-sorted))
