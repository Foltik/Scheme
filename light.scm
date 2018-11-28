(define (point-light pos color intensity)
  (list pos color intensity #t))

(define (directional-light dir color intensity)
  (list dir color intensity #f))

;; Getters
(define (l.pos light)
  (car light))

(define (l.dir light)
  (car light))

(define (l.color light)
  (car (cdr light)))

(define (l.intensity light)
  (car (cdr (cdr light))))

(define (l.point? light)
  (eq? (car (cdr (cdr (cdr light)))) #t))

;; Util
(define (l.dist-to light pos)
  (if (l.point? light)
      (vmag (vmap - (l.pos light) pos))
      1000000000))

(define (l.dir-to light pos)
  (if (l.point? light)
      (vnorm (vmap - (l.pos light) pos))
      (vmap - (l.dir light))))

(define (l.intensity-at light pos)
  (if (l.point? light)
      (/ (l.intensity light) (* 4 pi (square (vmap - (l.pos light) pos))))
      (l.intensity light)))

(define (l.calc-color light pos normal albedo)
  (let ((shadow-trace (trace (ray pos (l.dir-to light pos)) #f)))
    (if (or (eq? (car shadow-trace) #t) (< (car (cdr shadow-trace)) (l.dist-to light pos)))
        (color 0 0 0)
        (vsmap *
          (*
            (l.intensity light)
            (max 0 (vdot normal (l.dir-to light pos)))
            (/ albedo pi))
          (l.color light)))))
