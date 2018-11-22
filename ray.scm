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
