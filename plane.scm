(define (plane pos normal color)
  (list pos normal color))

;; Getters
(define (p.pos plane)
  (car plane))
(define (p.norm plane)
  (car (cdr plane)))
(define (p.col plane)
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
