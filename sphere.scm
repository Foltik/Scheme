(define (sphere pos radius material)
  (list pos radius material))

;; Getters
(define (s.pos sphere)
  (car sphere))
(define (s.rad sphere)
  (car (cdr sphere)))
(define (s.mat sphere)
  (car (cdr (cdr sphere))))

(define (s.dist sphere ray)
  (-
    (vmag (vmap - (r.pos ray) (s.pos sphere)))
    (s.rad sphere)))

(define (s.norm sphere ray)
  (vnorm
    (vmap - (r.pos ray) (s.pos sphere))))
