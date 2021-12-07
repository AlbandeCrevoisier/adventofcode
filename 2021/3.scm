#|
  Input is a binary diagnostic report:
  00100
  11110
  10110
  10111
  Gamma rate: most common bit of each position.
  Epsilon rate: least common bit.
  1/ Get the power consumption by multiplying the gamma & epsilon rates.
  Note: the epsilon rate is the bit-by-bit opposite of the gamma rate.
  Let's just sum the bits of each position & compare it to the number
  of lines / 2.
|#

; input -> list of strings -> list of list of numbers
(define (input->bins port)
  (let ((line (read-line port)))
    (if (eof-object? line)
        '()
        (cons (map digit-value (string->list line)) (input->bins port)))))

(define (position-sum bins)
  (if (null? (car bins))
      '()
      (cons (apply + (map car bins))
            (position-sum (map cdr bins)))))

(define (gamma bins)
  (map
    (lambda (x)
      (if (< x (/ (length bins) 2))
          0
          1))
    (position-sum bins)))

(define (flip-bit bit)
  (if (= bit 0) 1 0))

(define (epsilon gamma)
  (map flip-bit gamma))

; Must work on any number of bits.
(define (bin->number bin)
  (if (null? bin)
      0
      (+ (* (car bin) (expt 2 (- (length bin) 1)))
         (bin->number (cdr bin)))))

; Call on input
(display
  (let ((gamma_rate (gamma (call-with-input-file "3" input->bins))))
    (* (bin->number gamma_rate)
       (bin->number (epsilon gamma_rate)))))
(newline)

#|
  - Oxygen generator rating: same as gamma, but filtering bit after bit - for
                             the second bit, use the most common bit of the
                             remaining numbers.
  - CO2 scrubber rating: same with epsilon.
|#

(define (filter bins criterion position)
  (if (null? bins)
      '()
      (if (= (list-ref (car bins) position) criterion)
          (cons (car bins) (filter (cdr bins) criterion position))
          (filter (cdr bins) criterion position))))

(define (most-common-bit-in-pos bins position)
  (let ((frequency (apply + (map (lambda (x) (list-ref x position)) bins)))
        (total (length bins)))
    (if (< frequency (/ total 2))
        0
        1)))

(define (O2-generator bins)
  (define (rec bins position)
    (if (= (length bins) 1)
        (car bins)
        (rec (filter bins (most-common-bit-in-pos bins position) position)
             (+ 1 position))))
  (rec bins 0))

(define (CO2-scrubber bins)
  (define (rec bins position)
    (if (= (length bins) 1)
        (car bins)
        (rec (filter bins
                     (flip-bit (most-common-bit-in-pos bins position))
                     position)
             (+ 1 position))))
  (rec bins 0))

; Call on input
(let ((bins (call-with-input-file "3" input->bins)))
  (display (* (bin->number (O2-generator bins))
              (bin->number (CO2-scrubber bins)))))
(newline)
