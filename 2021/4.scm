#|
  Game of bingo.
  1/ Sum of all unmarked numbers of the winning card multiplyed
  by the last drawn number.
  input example:
  1,2,3

  1 2
  3 4

  5 6
  7 8
  Notes:
  - There is a space in front of single digit numbers to maintain
  alignment with double digits ones.
  - Grids are 5 by 5, with numbers in [0, 99].
  - Hypothesis: numbers can only appear once on a card.
  - Let's use complex number to denote crossed numbers.
|#

(define (read-draws port)
  (let ((c (read-char port)))
    (if (char=? c #\newline)
        '()
        (let ((cc (read-char port)))
          (cond ((char=? cc #\newline)
                 (list (digit-value c)))
                ((char=? cc #\,)
                 (cons (digit-value c) (read-draws port)))
                (else
                 (if (char=? #\, (peek-char port))
                     (read-char port)) ; read the #\, out of the port
                 (cons (+ (* 10 (digit-value c)) (digit-value cc))
                       (read-draws port))))))))

(define (read-card-number port)
  (let ((c (read-char port)))
    (let ((cc (read-char port)))
      (if (char=? c #\space)
          (digit-value cc)
          (+ (* 10 (digit-value c)) (digit-value cc))))))

(define (read-card-line port)
  (let ((n (read-card-number port)))
    (if (char=? (read-char port) #\space)
        (cons n (read-card-line port))
        (list n))))

(define (read-card port)
  (let ((l (read-card-line port)))
    (if (or (eof-object? (peek-char port))
            (char=? (peek-char port) #\newline))
        (list l)
        (cons l (read-card port)))))

(define (read-cards port)
  (if (eof-object? (read-char port))
      '()
      (let ((card (read-card port)))
        (cons card (read-cards port)))))

(define (remove-non-reals row)
  (if (null? row)
      '()
      (let ((c (car row)))
        (cons (if (= (imag-part c) 0) c 0)
              (remove-non-reals (cdr row))))))

(define (sum-row card)
  (map (lambda (x) (apply + x)) card))

(define (sum-col card)
  (if (null? (car card))
      '()
      (cons (apply + (map car card))
            (sum-col (map cdr card)))))

(define (card-sum card)
  (apply + (sum-row (map remove-non-reals card))))

(define (card-won? card)
  (= (length card)
     (max (apply max (map imag-part (sum-row card)))
          (apply max (map imag-part (sum-col card))))))

(define (mark-draw-in-row draw row)
  (cond ((null? row) '())
        ((= draw (car row))
         (cons (+ draw 1i) (mark-draw-in-row draw (cdr row))))
        (else (cons (car row) (mark-draw-in-row draw (cdr row))))))

(define (mark-draw draw card)
  (map (lambda (x) (mark-draw-in-row draw x)) card))

; Return the result if a card won, then stop.
(define (check-cards draw cards)
  (if (null? cards)
      '()
      (let ((c (mark-draw draw (car cards))))
        (if (card-won? c)
            (list (* draw (card-sum c)))
            (cons c (check-cards draw (cdr cards)))))))

; If a card won, it returned the result instead of the card.
(define (has-won? cards)
  (cond ((number? cards) cards) ; the first card won
        ((number? (car (reverse cards)))
         (car (reverse cards))); ugly, but...
        (else #f))) ; numbers count as #t, only #f counts as #f.

(define (play-bingo draws cards)
  (let ((marked (check-cards (car draws) cards)))
    (let ((res (has-won? marked)))
      (if res
          res
          (play-bingo (cdr draws) marked)))))

(define (play-bingo-with-input port)
  (let ((ds (read-draws port)))
    (play-bingo ds (read-cards port))))

(define (day4-part1)
  (call-with-input-file "4" play-bingo-with-input))

#|
  2/ What is the score of the last winning board?
  Let's drop all winning cards until there is only one left.
  Then, let's find its score.
|#

; Will only be used if cards is not '(number). Thus, drop won cards.
(define (check-cards-drop-won draw cards)
  (if (null? cards)
      '()
      (let ((c (mark-draw draw (car cards))))
        (if (card-won? c)
            (check-cards-drop-won draw (cdr cards))
            (cons c (check-cards-drop-won draw (cdr cards)))))))

; First, play until there is only one card.
; Then, find its score.
(define (last-score draws cards)
  (if (= (length cards) 1)
      (play-bingo draws cards) ; Work because it cannot have won yet.
      (last-score (cdr draws) (check-cards-drop-won (car draws) cards))))

(define (last-score-with-input port)
  (let ((ds (read-draws port)))
    (last-score ds (read-cards port))))

(define (day4-part2)
  (call-with-input-file "4" last-score-with-input))
