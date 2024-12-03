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
        (srfi 1)  ;; filter, count
        (srfi 132))  ;; sort

(define reports (map extract-numeric (read-input-file "day2-example")))
;;(define reports (map extract-numeric (read-input-file "day2-input")))

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

(displayln (count safe? reports))


;; The second task is trickier: it poses the same question as part 1,
;; but with the possibility to remove one level from the report.
;; 7 6 4 2 1 Safe without removing any level.
;; 1 2 7 8 9 Unsafe regardless of which level is removed.
;; 9 7 6 2 1 Unsafe regardless of which level is removed.
;; 1 3 2 4 5 Safe by removing the second level, 3.
;; 8 6 4 4 1 Safe by removing the third level, 4.
;; 1 3 6 7 9 Safe without removing any level.
;; => 4 safe levels now.
;; We may change the code by adding a case when the report is unsafe:
;; Generate the sub-reports with one level removed, & check them.
;; Note: in my input, there are between 5 & 8 levels per report.
(import (scheme eval))

(define (make-sub-reports report)
  (unfold (lambda (i) (>= i (length report)))
          (lambda (i) (append (take report i) (drop report (+ i 1))))
          (lambda (i) (+ i 1))
          0))

(define (sub-report-safe? report)
  (let ((sub-reports (make-sub-reports report)))
    (eval (append '(or) (map safe? sub-reports)))))  ;; Ugly, hacky, worky

(define (dampener-safe? report)
  (or (safe? report) (sub-report-safe? report)))

(displayln (count dampener-safe? reports))
