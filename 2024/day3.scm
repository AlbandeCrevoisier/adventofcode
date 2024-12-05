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

(define memory (apply string-append (read-input-file "day3-example")))
;;(define memory (apply string-append (read-input-file "day3-input")))

(define valid-mul '(: "mul(" (** 1 3 numeric) "," (** 1 3 numeric) ")"))

(define (extract-valid-muls memory) (regexp-extract valid-mul memory))
(define (execute-mul mul) (apply * (extract-numeric mul)))

(define (sum-mul memory)
  (apply + (map execute-mul (extract-valid-muls memory))))

(displayln (sum-mul memory))


;; The second task is to additionaly parse the do() and don't()
;; instructions that respectively enable & disable multiplications.
;; I find this task ambiguous: multiplications are enabled by default,
;; but is that true for each line or the whole input?
;; I shall first assume each line corresponds to a separate memory
;; blocks, & thus that multiplication is enabled at each start start.
;; After this, we only need to extract the memory portions where
;; multiplications are enabled, & to apply our prior algorithm.

;;(define memory-blocks (read-input-file "day3-example"))
(define memory-blocks (read-input-file "day3-input"))

(define (split-do mem-block) (regexp-split "do()" mem-block))
(define (erase-dont mem-block) (regexp-replace '(: "don't()" (* any))
                                               mem-block
                                               ""))
(define (map-erase-dont mem-blocks) (map erase-dont mem-blocks))
(define (map-string-append mem-blocks) (map string-append mem-blocks))

(define mult-enabled-mem
  (apply append
         (map map-string-append
              (map map-erase-dont
                   (map split-do memory-blocks)))))

(displayln (apply + (map sum-mul mult-enabled-mem)))
;; answer is too high
