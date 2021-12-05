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

;; Call on input
(display (number-times-increases (call-with-input-file "1" input->list)))
