#|
  Align crab submarines, which can only more horizontaly & spend 1 unit of fuel
  for each move.
  Example input: 16,1,2
  1/ What is the minimum fuel required to align all crabs?
  Note: the position is not the barycenter, as I thought. Screw that, let's use
  brut force.
|#

(define (read-number-string port)
  (let ((c (peek-char port)))
    (if (char-numeric? c)
        (let ((actually-read-the-char (read-char port)))
          (string-append (string c) (read-number-string port)))
        "")))

(define (read-positions port)
  (let ((c (peek-char port)))
    (cond ((eof-object? c) '())
          ((char-numeric? c)
           (let ((n (string->number (read-number-string port))))
             (cons n (read-positions port))))
          (else ; read the comma
            (read-char port)
            (read-positions port)))))

(define (enumerate-positions min max)
  (if (= min max)
      (list max)
      (cons min (enumerate-positions (+ min 1) max))))

(define (fuel-to-align positions pos)
  (apply + (map (lambda (p) (abs (- pos p))) positions)))

(define (day7-part1)
  (let ((positions (call-with-input-file "7" read-positions)))
    (let ((min-pos (apply min positions))
          (max-pos (apply max positions)))
      (apply min (map (lambda (p) (fuel-to-align positions p))
                      (enumerate-positions min-pos max-pos))))))
