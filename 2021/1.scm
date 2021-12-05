#|
  Example input:
  199
  200
  208
  210
  These represent successive depths.
  1/ How many times does the depth increase?
|#

(define (input->list port)
  (let ((line (read-line port)))
    (if (eof-object? line)
        '()
        (cons (string->number line) (input->list port)))))

(define (number-times-increases depths)
  (if (< (length depths) 2)
      0
      (+ (if (< (car depths) (cadr depths))
             1
             0)
         (number-times-increases (cdr depths)))))

; Call on input
(display (number-times-increases (call-with-input-file "1" input->list)))
(newline)


; 2/ Same question but by using a 3-values sliding window.

(define (depths->windows depths)
  (if (< (length depths) 3)
      '()
      (cons (+ (car depths) (cadr depths) (caddr depths))
            (depths->windows (cdr depths)))))

; Call on input
(display (number-times-increases
           (depths->windows (call-with-input-file "1" input->list))))
(newline)
