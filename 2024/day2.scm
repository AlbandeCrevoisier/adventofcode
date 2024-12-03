;; We are presented with a list of reports (lines) consisting of
;; a list of levels (numbers).
;; A report is safe when both:
;; - its levels are strictly increasing or decreasing
;; - the distance between two consecutive levels is at most 3
;; Example:
;; 7 6 4 2 1  safe
;; 1 2 7 8 9  unsafe: increase of 5
;; 9 7 6 2 1  unsafe: decrease of 4
;; 1 3 2 4 5  unsafe: not monotonous
;; 8 6 4 4 1  unsafe: not strictly monotonous
;; 1 3 6 7 9  safe
;; -> 2 safe reports.
;; This task seems rather easy. In C, I would process each report as
;; it is read from the file & would thus hold nothing in memory, but
;; here I do not want to seek such optimisation as I believe it would
;; be clunky in Scheme, at least with my level.

(import (scheme base)  ;; R7RS
        (utils)
        (srfi 1)  ;; filter
        (srfi 132))  ;; sort

;;(define reports (map extract-numeric (read-input-file "day2-example")))
(define reports (map extract-numeric (read-input-file "day2-input")))

;; (list-sorted? comp list) returns #f <=> there exists an adjacent
;; pair x y in the list such that y comp x.
;; Therefore, using <= as the comparator allows us to check for
;; strictly monotonous lists.
(define (strictly-monotonous? report) (or
                                       (list-sorted? <= report)
                                       (list-sorted? >= report)))
(define (gradual? report)
  (if (null? (cdr report))
      #t
      (and (<= (difference (car report) (cadr report))
              3)
           (gradual? (cdr report)))))
(define (safe? report) (and (strictly-monotonous? report)
                            (gradual? report)))
(define (count-safe reports)
  (length (filter safe? reports)))

(displayln (count-safe reports))
