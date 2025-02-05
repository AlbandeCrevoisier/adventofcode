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
(define (find-line-break lines) (list-index string-empty? lines))

(define (parse-orderings orderings) (map extract-numeric orderings))
(define (get-orderings lines)
  (parse-orderings (take lines (find-line-break lines))))

(define (parse-manuals manuals) (map extract-numeric manuals))
(define (get-manuals lines)
  (parse-manuals (drop lines (+ (find-line-break lines) 1))))

(define dependencies (make-hash-table (make-default-comparator)))
(define (insert-dependency ordering)
  (let ((updater (lambda (deps) (cons (car ordering) deps))))
    (hash-table-update!/default dependencies
                                (cadr ordering)
                                updater
                                '())))
(define (insert-dependencies)
  (map insert-dependency (get-orderings lines)))
(insert-dependencies)

(define (get-dependencies page) (hash-table-ref/default dependencies
                                                        page
                                                        '()))

(define (valid-page? page other-pages)
  (let ((page-dependencies (get-dependencies page)))
    (iset-empty? (iset-intersection (list->iset other-pages)
                                    (list->iset page-dependencies)))))
(define (valid-manual? manual)
  (let ((page (car manual))
        (following-pages (cdr manual)))
    (if (<= (length manual) 1)
        #t
        (and (valid-page? page following-pages)
             (valid-manual? following-pages)))))

(define (filter-valid-manuals manuals)
  (filter valid-manual? manuals))

(define (middle-page manual)
  (list-ref manual (floor (/ (length manual) 2))))
(define (middle-pages manuals)
  (map middle-page manuals))
(define (sum-middle-pages manuals)
  (apply + (middle-pages manuals)))

(define (sum-valid-manuals-mid-pages manuals)
  (sum-middle-pages (filter-valid-manuals manuals)))
(displayln (sum-valid-manuals-mid-pages (get-manuals lines)))


;; Part 2 requires that we filter out the valid manuals, correct the invalid
;; ones, & sum their middle pages.

;; -> iteratively pick the page that is in conflict with none other, then
;; recurse on the list with this page removed
;; Needs something like "pick the first element in that list that satisfies
;; this predicate -> (any , I think? => (find , actually, or (list-index
;; Also: return the list with this value dropped
;; => use (find & (delete

(define (remove-valid-manuals manuals)
  (remove valid-manual? manuals))

(define (order manual)
  (if (<= (length manual) 1)
      manual
      (let ((first-page? (lambda (page) (valid-page? page manual))))
        (let ((first-page (find first-page? manual)))
          (cons first-page (order (delete first-page manual)))))))
(define (order-manuals manuals) (map order manuals))

(define (sum-invalid-manuals-mid-pages manuals)
  (sum-middle-pages (order-manuals (remove-valid-manuals manuals))))
(displayln (sum-invalid-manuals-mid-pages (get-manuals lines)))
