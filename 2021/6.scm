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
