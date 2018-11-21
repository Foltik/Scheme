(define (sphere pos radius color)
  (list pos radius color))

;; Getters
(define (s.pos sphere)
  (car sphere))
(define (s.rad sphere)
  (car (cdr sphere)))
(define (s.col sphere)
  (car (cdr (cdr sphere))))

;; Check if a ray intersects the sphere
(define (s.intersect? ray sphere)
  (define hyp (v- (s-pos sphere) (r-pos ray)))
  (define adj (v. hyp (r-dir ray)))
  (define d (- (v. hyp hyp) (* adj adj)))
  (< d (* (s-rad sphere) (s-rad sphere))))
