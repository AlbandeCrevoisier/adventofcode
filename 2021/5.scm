#|
  Segments in a (1,000 by 1,000?) grid are given as follows:
  x1,y1 -> x2,y2
  At first, the grid is filled with '.', then add 1 whenever a segment reaches
  these coordinates.
  Ex: . 1 .
      1 2 1
      . 1 .
  1/ How many spots are crossed by at least two segments? Only consider
  horizontal & straight segments.
  Let's use matrix orientation for the grid (i.e. 0,0 is the top left corner).
|#

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (display-point p)
  (display (x-point p))
  (display ",")
  (display (y-point p)))

; Let's say a segment is from left to right or top to bottom if vertical.
(define (make-segment p q)
  (cond ((> (x-point p) (x-point q)) ; so that p.x <= q.x
         (make-segment q p))
        ((and (= (x-point p) (x-point q))
              (> (y-point p) (y-point q))) ; so that p.y <= q.y for verticals.
         (make-segment q p))
        (else (cons p q))))

(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

(define (display-segment seg)
  (display-point (start-segment seg))
  (display " -> ")
  (display-point (end-segment seg)))

(define (vertical? seg)
  (= (x-point (start-segment seg))
     (x-point (end-segment seg))))

(define (horizontal? seg)
  (= (y-point (start-segment seg))
     (y-point (end-segment seg))))

(define (filter-in-vertical segments)
  (cond ((null? segments) '())
        ((vertical? (car segments))
         (cons (car segments) (filter-in-vertical (cdr segments))))
        (else
         (filter-in-vertical (cdr segments)))))

(define (filter-in-horizontal segments)
  (cond ((null? segments) '())
        ((horizontal? (car segments))
         (cons (car segments) (filter-in-horizontal (cdr segments))))
        (else
         (filter-in-horizontal (cdr segments)))))

(define (next-point-segment seg)
  (let ((start (start-segment seg))
        (end (end-segment seg)))
    (let ((x-start (x-point start))
          (y-start (y-point start))
          (x-end (x-point end))
          (y-end (y-point end)))
      (make-point (cond ((< x-start x-end)
                         (+ x-start 1))
                        ((= x-start x-end)
                         x-start)
                        ((> x-start x-end)
                         (- x-start 1)))
                   (cond ((< y-start y-end)
                         (+ y-start 1))
                        ((= y-start y-end)
                         y-start)
                        ((> y-start y-end)
                         (- y-start 1)))))))

(define (segment-points seg)
  (let ((next-point (next-point-segment seg)))
    (if (equal? next-point (start-segment seg))
        (list next-point)
        (cons (start-segment seg)
              (segment-points (make-segment next-point (end-segment seg)))))))

(define (make-grid dim)
  (map (lambda (x) (make-list dim 0))
       (make-list dim 0)))

(define (display-grid grid)
  (cond ((null? grid)
         (display "")) ; no-op, probably rather inelegant
        (else
         (display (car grid))
         (newline)
         (display-grid (cdr grid)))))

(define (draw-point! grid p)
  (let ((row (list-ref grid (y-point p)))
        (x (x-point p)))
    (list-set! row x (+ (list-ref row x) 1))))

(define (draw-points! grid points)
  (map (lambda (p) (draw-point! grid p)) points))

(define (draw-segment! grid seg)
  (draw-points! grid (segment-points seg)))

(define (draw-segments! grid segments)
  (map (lambda (seg) (draw-segment! grid seg)) segments))

(define (number-of-2+s-in-row row)
  (cond ((null? row) 0)
        ((>= (car row) 2)
         (+ 1 (number-of-2+s-in-row (cdr row))))
        (else
         (number-of-2+s-in-row (cdr row)))))

(define (number-of-2+s grid)
  (apply + (map number-of-2+s-in-row grid)))

; format: "x1,y1 -> x2,y2"
(define (read-number-string port)
  (let ((c (peek-char port)))
    (if (char-numeric? c)
        (let ((actually-read-the-char (read-char port)))
          (string-append (string c) (read-number-string port)))
        "")))

(define (read-segment port)
  (let ((x1 (string->number (read-number-string port))))
    (read-char port) ; read the comma
    (let ((y1 (string->number (read-number-string port))))
      (read-char port) ; " -> "
      (read-char port)
      (read-char port)
      (read-char port)
      (let ((x2 (string->number (read-number-string port))))
        (read-char port) ; read the comma
        (let ((y2 (string->number (read-number-string port))))
          (read-char port) ; read the newline
          (make-segment (make-point x1 y1) (make-point x2 y2)))))))

(define (read-segments port)
  (let ((c (peek-char port)))
    (if (eof-object? c)
        '()
        (let ((seg (read-segment port)))
          (cons seg (read-segments port))))))

(define (day5-part1)
  (let ((segments (call-with-input-file "5" read-segments))
        (grid (make-grid 1000)))
    (draw-segments! grid (append (filter-in-vertical segments)
                                 (filter-in-horizontal segments)))
    (number-of-2+s grid)))
