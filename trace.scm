(define (nearest-object ray)
  (apply minfn car
    (append
      (map
        (lambda (sphere) (append (s.dist-norm sphere ray) (list (s.mat sphere))))
        scene-spheres))))
      ;; (map
      ;;   (lambda (plane) (append (s.dist-norm plane ray) (p.mat plane)))
      ;;   scene-planes))))
      
(define max-steps 20)
(define (trace-helper ray steps distance)
  (let ((nearest (nearest-object ray)))
    (cond
      ((or (= steps max-steps) (> (z (r.pos ray)) 40) (> (car nearest) 100))
       (color 0 0 0))
      ((< (car nearest) 0.001)
       (define light-dir (vec 0 -1 0))
       (define to-light light-dir)
       (vsmap * (vdot (car (cdr nearest)) to-light) (m.diff (car (cdr (cdr nearest))))))
      (else
       (trace-helper
         (rmarch ray (car nearest))
         (+ steps 1)
         (+ distance (car nearest)))))))
       
(define (trace ray)
  (trace-helper ray 0 0))

(define (sample point)
  (trace (prime-ray point)))

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

