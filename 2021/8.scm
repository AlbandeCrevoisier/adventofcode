#|
  4-digit seven-segment dispay: each segment is indexed by a letter, a to g.
    0:      1:      2:      3:      4:
   aaaa    ....    aaaa    aaaa    ....
  b    c  .    c  .    c  .    c  b    c
  b    c  .    c  .    c  .    c  b    c
   ....    ....    dddd    dddd    dddd
  e    f  .    f  e    .  .    f  .    f
  e    f  .    f  e    .  .    f  .    f
   gggg    ....    gggg    gggg    ....

    5:      6:      7:      8:      9:
   aaaa    aaaa    aaaa    aaaa    aaaa
  b    .  b    .  .    c  b    c  b    c
  b    .  b    .  .    c  b    c  b    c
   dddd    dddd    ....    dddd    dddd
  .    f  e    f  .    f  e    f  .    f
  .    f  e    f  .    f  e    f  .    f
   gggg    gggg    ....    gggg    gggg
  The signal - segment wiring is mixed up independantly on each 4-digit
  display, but in the same way for each 7-segment digit.
  For each display, we have the list of the ten signals & 4-digit output value:
  be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb |
  fdgacbe cefdb cefbgd gcbe
  Note that it is a single line in the input.
  Digit segments number:
  0 -> 6: abcefg
  1 -> 2: cf
  2 -> 5: acdeg
  3 -> 5: acdfg
  4 -> 4: bcdf
  5 -> 5: abdfg
  6 -> 6: abdefg
  7 -> 3: acf
  8 -> 7: abcdefg
  9 -> 6: abcdfg
  Only 1, 4, 7, & 8 have a unique number of segments.
  1/ How many 1, 4, 7, & 8 in the output digits?
|#

(define (read-digit port)
  (let ((c (peek-char port)))
    (if (char-alphabetic? c)
        (let ((actually-read-the-char (read-char port)))
          (string-append (string c) (read-digit port)))
        "")))

(define (read-digits port)
  (let ((c (peek-char port)))
    (cond ((or (char=? c #\|) (char=? c #\newline))
           '())
          ((char-alphabetic? c)
            (let ((digit (read-digit port)))
             (cons digit (read-digits port))))
          (else ; #\space
            (read-char port)
            (read-digits port)))))

(define (make-note input output) (cons input output))
(define (note-input note) (car note))
(define (note-output note) (cdr note))

(define (display-note note)
  (display "input: ")
  (display (note-input note))
  (newline)
  (display "output: ")
  (display (note-output note))
  (newline))

(define (read-note port)
  (let ((input (read-digits port)))
    (read-char port) ; #\|
    (make-note input (read-digits port))))

(define (read-notes port)
  (let ((c (peek-char port)))
    (cond ((eof-object? c) '())
          ((char=? c #\newline)
           (read-char port) ; read the #\newline
           (read-notes port))
          (else ; beginning of a note
            (let ((note (read-note port)))
              (cons note (read-notes port)))))))

(define (is-1? digit) (= (string-length digit) 2))
(define (is-4? digit) (= (string-length digit) 4))
(define (is-7? digit) (= (string-length digit) 3))
(define (is-8? digit) (= (string-length digit) 7))

(define (count-digits digits criterion)
  (apply + (map (lambda (d) (if (criterion d) 1 0)) digits)))

(define (count-1-4-7-8 notes)
  (let ((outputs (apply append (map note-output notes))))
    (+ (count-digits outputs is-1?)
       (count-digits outputs is-4?)
       (count-digits outputs is-7?)
       (count-digits outputs is-8?))))

(define (day8-part1)
  (count-1-4-7-8 (call-with-input-file "8" read-notes)))
