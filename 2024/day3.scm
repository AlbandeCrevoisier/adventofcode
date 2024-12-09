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

;;(define memory (read-input-file "day3-example"))
(define memory (read-input-file "day3-input"))

(define valid-mul '(: "mul(" (** 1 3 numeric) "," (** 1 3 numeric) ")"))
(define (extract-muls mem-block) (regexp-extract valid-mul mem-block))

(define (execute-mul mul) (apply * (extract-numeric mul)))

;; Map a function f that returns a list to memory & append all their
;; returned lists.
(define (map-to-mem f memory) (apply append (map f memory)))


(define (sum-mul memory)
  (apply + (map execute-mul
                (map-to-mem extract-muls memory))))

(displayln (sum-mul memory))


;; The second task is to additionaly parse the do() and don't()
;; instructions that respectively enable & disable multiplications.
;; I find this task ambiguous: multiplications are enabled by default,
;; but is that true for each line or the whole input?
;; I shall first assume each line corresponds to a separate memory
;; blocks, & thus that multiplication is enabled at each start start.
;; After this, we only need to extract the memory portions where
;; multiplications are enabled, & to apply our prior algorithm.
;; As this results in too high an answer, let's now assume that we are
;; given a single memory block, & so that only the beginning of
;; the first line has multiplication enabled.
;; This second assumption proved to be correct.

(define (all-memory memory) (list (apply string-append memory)))

(define (split-do mem-block) (regexp-split "do()" mem-block))
(define (erase-dont mem-block) (regexp-replace '(: "don't()" (* any))
                                               mem-block
                                               ""))
(define (mul-enabled-mem memory)
  (map erase-dont (map-to-mem split-do memory)))

(displayln (sum-mul (mul-enabled-mem (all-memory memory))))
