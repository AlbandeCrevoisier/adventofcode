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
        (srfi 128) ;; comparators, reco by 125
        (chibi iset)) ;; integer sets

;;(define lines (read-input-file "day5-example"))
(define lines (read-input-file "day5-input"))

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

(define (get-dependencies page) (hash-table-ref/default dependencies
                                                        page
                                                        '()))

(define (valid-page? page following-pages dependencies)
  (iset-empty? (iset-intersection (list->iset following-pages)
                                  (list->iset dependencies))))
(define (valid-manual? manual)
  (let ((page (car manual))
        (following-pages (cdr manual)))
    (if (<= (length manual) 1)
        #t
        (and (valid-page? page following-pages (get-dependencies page))
             (valid-manual? following-pages)))))

(define (filter-valid-manuals manuals)
  (filter valid-manual? manuals))

(define (middle-page manual)
  (list-ref manual (floor (/ (length manual) 2))))
(define (middle-pages manuals)
  (map middle-page manuals))
(define (sum-middle-pages manuals)
  (apply + (middle-pages manuals)))

(define (sum-valid-manuals-mid-pages raw-manuals)
  (sum-middle-pages (filter-valid-manuals (parse-manuals raw-manuals))))
(displayln (sum-valid-manuals-mid-pages manuals))
