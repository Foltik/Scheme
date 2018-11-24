(define (sphere pos radius material)
  (list pos radius material))

;; Getters
(define (s.pos sphere)
  (car sphere))
(define (s.rad sphere)
  (car (cdr sphere)))
(define (s.mat sphere)
  (car (cdr (cdr sphere))))

(define (s.dist-norm sphere ray)
  (let ((vec-between (vmap - (r.pos ray) (s.pos sphere))))
    (list
      (- (vmag vec-between) (s.rad sphere))
      (vnorm vec-between))))
