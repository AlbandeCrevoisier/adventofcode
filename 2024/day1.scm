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
;; would yield: [1 2 3 3 3 4] & [3 3 3 4 5 9],
;; then 2 + 1 + 0 + 1 + 2 + 5 = 11.
;; The problem seems rather easy, though parsing might be a chore.

(import (scheme base)
        (utils)
        (srfi 132))  ;; sort

(define input-list (read-input-file "day1-example"))
;;(define input-list (read-input-file "day1-input"))

(define extracted-input (map extract-numeric input-list))

(define left-input (map car extracted-input))
(define right-input (map cadr extracted-input))

(define left-sorted (list-sort < left-input))
(define right-sorted (list-sort < right-input))

(define (difference a b) (abs (- a b)))
(define (sum-differences ls rs)
  (apply + (map difference ls rs)))

(displayln (sum-differences left-sorted right-sorted))


;; The second part of the problem is to compute a similarity score
;; between each list: we multiply each number in the left column by
;; the number of times it appears in the right one.
;; Example:
;; 3   4
;; 4   3
;; 2   5
;; 1   3
;; 3   9
;; 3   3
;; would yield: 3*3 + 4*1 + 2*0 + 1*0 + 3*3 + 3*3 = 31.
;; It quickly appears that this is, for each number in the left
;; column, the sum of number * n_left * n_right:
;; 3 3 3
;; 4 1 1
;; 2 1 0
;; 1 1 0
;; 5 0 1
;; 9 0 1
;; => 3*3*3 + 4*1*1 + 2*1*0 + 1*1*0 = 31
;; Therefore, it would probably be useful to transform our lists to
;; count each value, & to store that in a structure that makes it easy
;; to access an arbitrary value: a hash table (I miss my python data
;; tools, where this would have been a trivial group by & inner join!)

(import (srfi 69)) ;; hash tables

(define (input->hash-table input-list)
  (let ((input-ht (make-hash-table)))
    (let ((insert-value ;; updates value counts
           (lambda (val)
             (hash-table-update!/default input-ht
                                         val
                                         (lambda (x) (+ x 1)) 0))))
      (for-each insert-value input-list)
      input-ht)))

(define left-ht (input->hash-table left-input))
(define right-ht (input->hash-table right-input))

(define (update-similarity-score val count score)
  (+ score
     (* val count (hash-table-ref/default right-ht val 0))))
(displayln (hash-table-fold left-ht update-similarity-score 0))
