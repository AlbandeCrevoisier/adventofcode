#|
  Borked navigation subsystem, where input lines have been mangled.
  Input: lines like "[({(<(())[]>[[{[]{<()<>>".
  Each pair of delimiters form a chunk.
  Lines are either incomplete or corrupted.
  1/ For corrupted lines, find the first corrupted delimiter:
  ): 3 points.
  ]: 57 points.
  }: 1197 points.
  >: 25137 points.
  and sum the associated values.
  Note: the obvious imperative programming answer is to use a stack, but is
  there a way to use the function call stack instead? I can't see how, it's
  too hard for me.
|#

(define (read-input port)
  (let ((line (read-line port)))
    (if (eof-object? line)
        '()
        (cons (string->list line) (read-input port)))))

(define (head stack)
  (car stack))

(define (pop stack)
  (cdr stack))

(define (push stack element)
  (cons element stack))

(define (delimiter-value del)
  (cond ((char=? del #\) ) 3)
        ((char=? del #\] ) 57)
        ((char=? del #\} ) 1197)
        ((char=? del #\> ) 25137)))

(define (delimiter-match? opening closing)
  (or (and (char=? opening #\( ) (char=? closing #\) ))
      (and (char=? opening #\[ ) (char=? closing #\] ))
      (and (char=? opening #\{ ) (char=? closing #\} ))
      (and (char=? opening #\< ) (char=? closing #\> ))))

(define (check-delimiter line stack)
  (if (null? line)
      0 ; line is not corrupted
    (let ((del (car line))
          (opened (if (null? stack)
                      #\0 ; placeholder
                      (head stack))))
      (cond ((or (char=? del #\( )
                 (char=? del #\[ )
                 (char=? del #\{ )
                 (char=? del #\< ))
             (check-delimiter (cdr line) (push stack del)))
            ((delimiter-match? opened del)
             (check-delimiter (cdr line) (pop stack)))
            (else
             (delimiter-value del))))))

(define (check-line line)
  (check-delimiter line '()))

(define (day10-part1)
  (let ((lines (call-with-input-file "10" read-input)))
    (apply + (map check-line lines))))
