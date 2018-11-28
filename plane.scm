(define (plane pos normal material)
  (list pos normal material))

;; Getters
(define (p.pos plane)
  (car plane))
(define (p.norm plane)
  (car (cdr plane)))
(define (p.mat plane)
  (car (cdr (cdr plane))))

(define (p.dist plane ray)
  (let ((denom (vdot (vmap - (p.norm plane)) (r.dir ray))))
    (if (> denom 0.000001)
        (let ((dist (/ (vdot (vmap - (p.pos plane) (r.pos ray)) (vmap - (p.norm plane))) denom)))
          (if (>= dist 0)
              dist
              1000000))
        1000000)))
