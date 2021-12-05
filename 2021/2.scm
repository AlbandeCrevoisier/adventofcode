#|
  Submarine control:
  - forward X increases the horizontal position by X units.
  - down X increases the depth by X units.
  - up X decreases the depth by X units.
  Example:
  forward 5
  down 5
  forward 8
  up 3
  down 8
  forward 2
  Start at (0, 0), in the example end at (15, 10).
  1/ Find the final coordinates & multiply them (here, 150).
  Note: each command has a different number of letters, so to recognise
        "up", it is enough to know that the third character is a space.
|#

(define (move-sub port x y)
  (let ((line (read-line port)))
    (if (eof-object? line)
        (cons x y)
        (cond
          ((char-whitespace? (string-ref line 2)) ; "up X"
           (move-sub port x (- y (string->number (substring line 3)))))
          ((char-whitespace? (string-ref line 4)) ; "down X"
           (move-sub port x (+ y (string->number (substring line 5)))))
          ((char-whitespace? (string-ref line 7)) ; "forward X"
           (move-sub port (+ x (string->number (substring line 8))) y))))))

; Call on input
(display ((lambda (p) (* (car p) (cdr p)))
            (move-sub (open-input-file "2") 0 0)))
(newline)
