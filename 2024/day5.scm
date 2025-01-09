;; 75|13
;; 53|13

;; 75,47,61,53,29
;; 97,61,53,29,13
(import (scheme base)
        (utils)
        (srfi 1) ;; list processing
        (srfi 125) ;; hash tables
        (srfi 128)) ;; comparators, reco by 125

(define lines (read-input-file "day5-example"))

(define (string-empty? s) (string=? s ""))
(define line-break (list-index string-empty? lines))

(define orderings (take lines line-break))
(define manuals (drop lines (+ 1 line-break)))

(define t (make-hash-table (make-default-comparator)))

