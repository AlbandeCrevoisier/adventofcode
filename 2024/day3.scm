;; The task consists in finding valid computer multiplication
;; instructions in corrupted memory.
;; Valid substring: "mul(a,b)" where a & b are 1-3 digits numbers.
;; Ex: xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul \
;;     (32,64]then(mul(11,8)mul(8,5))
;; -> mul(2,4), mul(5,5), mul(11,8) & mul(8,5) are valid.
;; Once these valid instructions are extracted, we are to execute them
;; & sum their results.
;; This seems a basic regexp problem.
(import (scheme base)
        (utils)
        (srfi 115))  ;; regexp

;; (define memory (apply string-append (read-input-file "day3-example")))
(define memory (apply string-append (read-input-file "day3-input")))

(define valid-mul '(: "mul(" (** 1 3 numeric) "," (** 1 3 numeric) ")"))

(define (extract-valid-muls memory) (regexp-extract valid-mul memory))
(define (execute-mul mul) (apply * (extract-numeric mul)))

(displayln (apply + (map execute-mul (extract-valid-muls memory))))
