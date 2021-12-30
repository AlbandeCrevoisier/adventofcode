#|
  Segments in a ten by ten grid are given as follows:
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

; format: "x1,y1 -> x2,y2"
(define (read-segment port)
  (let ((line (read-line port)))
    (make-segment (make-point (digit-value (string-ref line 0))
                              (digit-value (string-ref line 2)))
                  (make-point (digit-value (string-ref line 7))
                              (digit-value (string-ref line 9))))))

(define (read-segments port)
  (let ((c (peek-char port)))
    (if (eof-object? c)
        '()
        (let ((seg (read-segment port)))
          (cons seg (read-segments port))))))

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
