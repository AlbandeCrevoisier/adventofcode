#|
  We are looking for local minima in a height map, only looking at vertical
  & horizontal relations.
  Ex: 1 & 5 in 219
               398
               985
  Risk level: height + 1.
  1/ What is the sum of all local minima risk level?
  Let's learn about bytevector.
  We'll store data in a single vector, so we must track the line length.
|#

(define (read-byteline port)
  (apply bytevector (map digit-value (string->list (read-line port)))))

(define (read-bytelines port)
  (let ((c (peek-char port)))
    (if (eof-object? c)
        '()
        (let ((line (read-byteline port)))
          (cons line (read-bytelines port))))))

(define (display-bytelines hm)
  (for-each (lambda (byteline) (display byteline) (newline)) hm))

(define (make-heightmap bytelines)
  (cons (bytevector-length (car bytelines))
        (apply bytevector-append bytelines)))

(define heightmap-width car)
(define heightmap-values cdr)

(define (heightmap-length hm)
  (bytevector-length (heightmap-values hm)))

(define (heightmap-ref hm index)
  (bytevector-u8-ref (heightmap-values hm) index))

(define (read-heightmap port)
  (make-heightmap (read-bytelines port)))

(define (left-value hm index)
  (if (= (modulo index (heightmap-width hm)) 0) ; first column
      10 ; so it will never be the minimum
      (heightmap-ref hm (- index 1))))

(define (right-value hm index)
  (if (= (modulo index (heightmap-width hm))
         (- (heightmap-width hm) 1)) ; last column
      10 ; so it will never be the minimum
      (heightmap-ref hm (+ index 1))))

(define (above-value hm index)
  (let ((width (heightmap-width hm)))
    (if (< index width)
        10 ; so it will never be the minimum
        (heightmap-ref hm (- index width)))))

(define (below-value hm index)
  (let ((width (heightmap-width hm)))
    (if (>= (+ index width) (bytevector-length (heightmap-values hm)))
        10 ; so it will never be the minimum
        (heightmap-ref hm (+ index width)))))

(define (local-min? hm index)
  (let ((local-height (heightmap-ref hm index)))
    (and (< local-height (left-value hm index))
         (< local-height (right-value hm index))
         (< local-height (above-value hm index))
         (< local-height (below-value hm index)))))

(define (range start end)
  (if (= start end)
      '()
      (cons start (range (+ start 1) end))))

(define (local-minima hm)
  (define (filter-min index)
    (if (local-min? hm index)
        (list (heightmap-ref hm index))
        '()))
  (apply append
         (map filter-min (range 0 (heightmap-length hm)))))

(define (day9-part1)
  (let ((hm (call-with-input-file "9" read-heightmap)))
    (apply + (map (lambda (x) (+ x 1)) (local-minima hm)))))
