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

(define (heightmap-set! hm index val)
  (bytevector-u8-set! (heightmap-values hm) index val))

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

#|
  Basins are areas that flow to a minimum. Only 9's are not part of any basin.
  The size of a basin is the number of locations it contains.
  Each location belongs to a single basin, which belongs to a single minimum.
  2/ Multiply the sizes of the three largest basins.
  Note: The idea is probably to find minima & then find the size of the basin
  surrounding them.
  Assumption: based on the text, I assume that only 9's can be in the boundary
  of a basin. I.e. the following is invalid:
  1 2 1
  3 4 3
  Otherwise, I do not know thowards which basin the 2 & the 4 would belong.
  Note: I do not like it, but I believe the best way is to modify the heightmap
  to set the locations already visited to 10. Wish I could find a functional
  way.
|#

(define (local-minima-indices hm)
  (define (filter-min index)
    (if (local-min? hm index)
        (list index)
        '()))
  (apply append
         (map filter-min (range 0 (heightmap-length hm)))))

(define (basin-size hm index)
  (heightmap-set! hm index 10) ; mark location as visited
  (+ 1
     (if (< (left-value hm index) 9)
         (basin-size hm (- index 1))
         0)
     (if (< (right-value hm index) 9)
         (basin-size hm (+ index 1))
         0)
     (if (< (above-value hm index) 9)
         (basin-size hm (- index (heightmap-width hm)))
         0)
     (if (< (below-value hm index) 9)
         (basin-size hm (+ index (heightmap-width hm)))
         0)))

(define (basin-sizes hm)
  (map (lambda (x) (basin-size hm x)) (local-minima-indices hm)))

(import (srfi 95)) ; sorting & merging algorithms

(define (day9-part2)
  (let ((hm (call-with-input-file "9" read-heightmap)))
    (let ((basin-sizes (sort (basin-sizes hm))))
      (let ((three-biggest-basins
              (list-tail basin-sizes (- (length basin-sizes) 3))))
        (apply * three-biggest-basins)))))
