;; 75|13
;; 53|13

;; 75,47,61,53,29
;; 97,61,53,29,13

;; My plan is to build a table containing the page dependencies:
;; for a|c, a|b, have a: b, c .
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

(define (parse-orderings orderings) (map extract-numeric orderings))
(define (parse-manuals manuals) (map extract-numeric manuals))

(define dependencies (make-hash-table (make-default-comparator)))
(define (insert-dependency ordering)
  (let ((updater (lambda (deps) (cons (car ordering) deps))))
    (hash-table-update!/default dependencies
                                (cadr ordering)
                                updater
                                '())))
(define (insert-dependencies)
  (map insert-dependency (parse-orderings orderings)))
(insert-dependencies)
