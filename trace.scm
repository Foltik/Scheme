(define (nearest-object ray)
  (apply minfn car
    (append
      (map
        (lambda (sphere)
          (list
           (s.dist sphere ray)
           (s.norm sphere ray)
           (s.mat sphere)))
        scene-spheres)
      (map
        (lambda (plane)
          (list
            (p.dist plane ray)
            (p.norm plane)
            (p.mat plane)))
        scene-planes))))
      
(define max-steps 100)
(define (trace-helper ray steps distance output-color)
  (let ((nearest (nearest-object ray)))
    (cond
      ((or (= steps max-steps) (> (z (r.pos ray)) 100) (> (car nearest) 100))
       (if (eq? output-color #t)
           (color 0 0 0)
           (list #f 1000000000)))
      ((< (car nearest) 0.001)
        (let ((norm (car (cdr nearest)))
              (mat (car (cdr (cdr nearest)))))
          (if (eq? output-color #t)
            (if (> (m.reflect mat) 0)
              (vmap +
                (vsmap * (- 1 (m.reflect mat)) (m.color mat))
                (vsmap * (m.reflect mat)
                  (trace (r.reflect ray norm) #t)))
              (colclip
                (vmap *
                  (m.color mat)
                  (apply vmap +
                    (map
                      (lambda (light) (l.calc-color light (r.pos ray) norm (m.albedo mat)))
                      scene-lights)))))
            (list #t distance))))
      (else
       (trace-helper
         (rmarch ray (car nearest))
         (+ steps 1)
         (+ distance (car nearest))
         output-color)))))
       
(define (trace ray output-color)
  (trace-helper ray 0 0 output-color))

(define (sample point)
  (trace (prime-ray point) #t))

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

(define (fill-canvas fn lodoffset)
  (fill fn (vecn 2 (- half-scene-size)) scene-size (+ 1 (log2 scene-size) lodoffset)))

