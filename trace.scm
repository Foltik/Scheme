;; This is where the magic happens
(define (disp vec)
  (display "Sampling " vec "\n")
  (define vec (vsmap * 100 vec))
  (rect vec (vsmap + 1 vec) '(1 0 0))
  '(0 0))

(define (sample point)
  '())

(define (super-sample-helper fn factor offset coord)
  (cond
   ((= factor 1)
    (fn coord))
   (else
    (apply vavg
      (map
        (lambda (new-coord)
          (super-sample-helper fn (/ factor 4) (vsmap / 2 offset) new-coord))
        (vmaplst + coord (offset-permutations offset)))))))

(define (super-sample fn factor coord)
  (super-sample-helper fn factor (vec 0.5 0.5) coord))
