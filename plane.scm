(define (plane pos normal material)
  (list pos normal material))

;; Getters
(define (p.pos plane)
  (car plane))
(define (p.norm plane)
  (car (cdr plane)))
(define (p.mat plane)
  (car (cdr (cdr plane))))


(define (p.dist-norm plane ray)
  (list
    (cond
      ((> (x (p.norm plane) 0))
       (- (x (r.pos ray)) (x (p.pos plane))))
      ((> (y (p.norm plane) 0))
       (- (y (r.pos ray)) (y (p.pos plane))))
      ((> (z (p.norm plane) 0))
       (- (z (r.pos ray)) (z (p.pos plane)))))
    (p.norm plane)))
