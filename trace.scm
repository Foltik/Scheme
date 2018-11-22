;; This is where the magic happens
(define (disp vec)
  (display "Sampling " vec "\n")
  (define vec (vsmap * 100 vec))
  (rect vec (vsmap + 1 vec) '(1 0 0))
  '(0 0))

(define max-iterations 200)
(define (trace ray total-distance closest-call) '())

(define (sample point)
  '())

(define (make-offsets pos offset)
  (vmaplst + pos (offset-permutations offset)))

(define (super-sample-helper fn factor pos offset)
  (cond
   ((= factor 1)
    (fn pos))
   (else
    (apply vavg
      (map
        (lambda (newpos)
          (super-sample-helper fn (/ factor 4) newpos (vsmap / 2 offset)))
        (make-offsets pos offset))))))

(define (super-sample fn factor pos)
  (super-sample-helper fn factor pos (vec 0.5 0.5)))

(define (fill fn pos size lod)
  (cond
    ((= lod 1)
     (rect pos (vsmap + size pos) (fn pos)))
    (else
     (let ((halfsize (/ size 2))
           (sublod (- lod 1)))
       (fill fn pos halfsize sublod)
       (fill fn (vec (+ (x pos) halfsize) (y pos)) halfsize sublod)
       (fill fn (vec (x pos) (+ (y pos) halfsize)) halfsize sublod)
       (fill fn (vsmap + halfsize pos) halfsize sublod)))))

(define (fill-canvas fn size lodoffset)
  (fill fn (vecn 2 (- (/ size 2))) size (+ (log2 size) lodoffset)))

