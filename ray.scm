(define (ray position direction)
  (list position direction))

;; Getters
(define (r.pos ray)
  (car ray))
(define (r.dir ray)
  (car (cdr ray)))

;; Util
(define (rmarch r t)
  (ray
    (vmap + (r.pos r) (vsmap * t (r.dir r)))
    (r.dir r)))

(define (r.reflect r normal)
  (ray (r.pos r) (vmap - (r.dir r) (vsmap * (* 2 (vdot (r.dir r) normal)) normal))))
