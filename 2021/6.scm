#|
  Lanternfish simulation:
  - every 7 days, a lanternfish produces a new lantern fish
  - fresh lanternfishes take 8 days to produce offsprings
  1/ How many laternfishes after 80 days?
  Note: at each step, if not zero decrement, otherwise spawn a 6 & a 8.
|#

(define (read-ages port)
  (let ((c (read-char port)))
    (if (eof-object? c)
        '()
        (let ((comma (read-char port)))
          (cons (digit-value c) (read-ages port))))))

(define (grow age)
  (if (= age 0)
      (list 6 8)
      (list (- age 1))))

(define (step ages)
  (apply append (map grow ages)))

(define (simulate ages n-steps)
  (if (= n-steps 0)
      ages
      (simulate (step ages) (- n-steps 1))))

(define (day6-part1)
  (length (simulate (call-with-input-file "6" read-ages) 80)))

#|
  2/ 256 days
  Cannot simulate, as it would break the stack.
  It's actualy just a circular buffer where 0 gets added to both 6 & 8 when
  descending.
|#

(define (increment-age-count age-counts age)
  (list-set! age-counts age (+ (list-ref age-counts age) 1)))

(define (count-ages ages)
  (let ((age-counts (make-list 9 0)))
    (for-each (lambda (a) (increment-age-count age-counts a)) ages)
    age-counts))

(define (step-counts age-counts)
  (let ((rotate-ages (append (cdr age-counts) (list (car age-counts)))))
    (list-set! rotate-ages 6 (+ (list-ref rotate-ages 6)
                                (list-ref rotate-ages 8)))
    rotate-ages))

(define (n-step-counts age-counts n-steps)
  (if (= n-steps 0)
      age-counts
      (n-step-counts (step-counts age-counts) (- n-steps 1))))

(define (day6-part2)
  (let ((age-counts (count-ages (call-with-input-file "6" read-ages))))
    (apply + (n-step-counts age-counts 256))))
