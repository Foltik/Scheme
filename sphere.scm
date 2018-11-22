(define (sphere pos radius color)
  (list pos radius color))

;; Getters
(define (s.pos sphere)
  (car sphere))
(define (s.rad sphere)
  (car (cdr sphere)))
(define (s.col sphere)
  (car (cdr (cdr sphere))))

(define (s.dist-norm sphere ray)
  (let ((vec-between (vmap - (r.pos ray) (s.pos sphere))))
    (list
      (- (vmag vec-between) (s.rad sphere))
      (vnorm vec-between))))
