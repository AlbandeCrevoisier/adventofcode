(define-library (utils)
  (export read-input-file extract-numeric difference displayln)
  (import (scheme base)  ;; R7RS
          (chibi)  ;; IO
          (srfi 1)  ;; list: unfold
          (srfi 115))  ;; regexp
  
  (begin
    (define (read-input-file f)
      (call-with-input-file f
        (lambda (f)
          (unfold
           eof-object?
           values
           (lambda (x) (read-line f))
           (read-line f)))))

    (define (extract-numeric str) (map string->number
                                       (regexp-extract '(+ numeric) str)))

    (define (difference a b) (abs (- a b)))

    (define (displayln x) (begin (display x) (display "\n")))
    ))
