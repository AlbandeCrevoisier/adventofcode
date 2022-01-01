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

#|
  2/ Decode all outputs & sum them.
  Let's use  the following strategy to identify each segment:
  1 -> cf
  7 -> a
  4 -> bd

  5-with-2-unknown -> 2
  5-with-1-unknown -> g

  2 -> e, c->f, d->b
  Let's use set substraction. I like sets.
  We thus now deal with char lists instead of strings.
  Note: I misunderstood the problem. There is no need to parse everything,
  just to identify the numbers.
  I guess it will be easier now anyway.
|#

; For sets of char.
(define (remove-element-from-set set el)
  (cond ((null? set) '())
        ((char=? (car set) el)
         (remove-element-from-set (cdr set) el))
        (else
         (cons (car set) (remove-element-from-set (cdr set) el)))))

(define (set-substraction set1 set2)
  (if (null? set2)
      set1
      (set-substraction (remove-element-from-set set1 (car set2))
                        (cdr set2))))

(define (set=? set1 set2)
  (and (null? (set-substraction set1 set2))
       (null? (set-substraction set2 set1))))

(define (find-digit digits criterion)
  (if (criterion (car digits))
      (car digits)
      (find-digit (cdr digits) criterion)))

(define (find-cf digits)
  (string->list (find-digit digits is-1?)))

(define (find-a digits cf)
  (set-substraction (string->list (find-digit digits is-7?)) cf))

(define (find-bd digits cf)
  (set-substraction (string->list (find-digit digits is-4?)) cf))

(define (make-is-2? cfabd)
  (lambda (digit)
    (and (= (length digit) 5)
         (= (length (set-substraction digit cfabd)) 2))))

(define (find-two digits cfabd)
  (find-digit (map string->list digits) (make-is-2? cfabd)))

(define (make-is-other-5-segments? cfabd)
  (lambda (digit)
    (and (= (length digit) 5)
         (= (length (set-substraction digit cfabd)) 1))))

(define (find-g digits cfabd)
  (set-substraction (find-digit (map string->list digits)
                                (make-is-other-5-segments? cfabd))
                    cfabd))

(define (find-e two cfabdg)
  (set-substraction two cfabdg))

(define (find-c two abdeg)
  (set-substraction two abdeg))

(define (find-f cf c)
  (set-substraction cf c))

(define (find-d two aceg)
  (set-substraction two aceg))

(define (find-b bd d)
  (set-substraction bd d))

; Ugliest code I have written in Scheme -- yet.
(define (make-parser digits)
  (let ((cf (find-cf digits)))
    (let ((a (find-a digits cf))
          (bd (find-bd digits cf)))
      (let ((two (find-two digits (append cf a bd))))
        (let ((g (find-g digits (append cf a bd))))
          (let ((e (find-e two (append cf a bd g))))
            (let ((c (find-c two (append a bd e g))))
              (let ((f (find-f cf c))
                    (d (find-d two (append a c e g))))
                (let ((b (find-b bd d)))
                  (lambda (digit)
                    (cond ((set=? (string->list digit) (append a b c e f g))
                           0)
                          ((is-1? digit) 1)
                          ((set=? (string->list digit) two) 2)
                          ((set=? (string->list digit) (append a c d f g)) 3)
                          ((is-4? digit) 4)
                          ((set=? (string->list digit) (append a b d f g)) 5)
                          ((set=? (string->list digit) (append a b d e f g))
                           6)
                          ((is-7? digit) 7)
                          ((is-8? digit) 8)
                          ((set=? (string->list digit) (append a b c d f g))
                           9))))))))))))

(define (parse-note note)
  (let ((output (map (make-parser (note-input note)) (note-output note))))
    (+ (* 1000 (car output))
       (* 100 (cadr output))
       (* 10 (caddr output))
       (cadddr output))))

(define (day8-part2)
  (let ((notes (call-with-input-file "8" read-notes)))
    (apply + (map parse-note notes))))
