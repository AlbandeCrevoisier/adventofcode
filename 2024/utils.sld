(define-library (utils)
  (export read-input-file extract-numeric displayln)
  (import (scheme base)  ;; R7RS
          (chibi)  ;; IO
          (srfi 115))  ;; regexp
  
  (begin
    (define (read-all-lines f)
      (let ((rl (read-line f)))
        (if (eof-object? rl)
            '()
            (cons rl (read-all-lines f)))))
    (define (read-input-file f)
      (call-with-input-file f read-all-lines))

    (define (extract-numeric str) (regexp-extract '(+ numeric) str))

    (define (displayln x) (begin (display x) (display "\n")))
    ))
