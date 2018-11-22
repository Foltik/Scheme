;; Min
(define (min . args)
  (fold (lambda (a b) (if (< a b) a b)) (car args) (cdr args)))

;; Max
(define (max . args)
  (fold (lambda (a b) (if (> a b) a b)) (car args) (cdr args)))

;; Square a number
(define (square n)
  (* n n))

;; Rotate a 2d vector around an angle theta
(define (v2rot coord theta)
  (let ((c (cos theta))
        (s (sin theta)))
    (vec
      (- (* (x coord) c) (* (y coord) s))
      (+ (* (y coord) c) (* (x coord) s)))))

;; All permutations of (+/-x, +/-y)
;; (offset-permutations '(0.5 0.5)) -> ((0.5 0.5) (-0.5 0.5) (0.5 -0.5) (-0.5 -0.5))
(define (offset-permutations vec2)
  (let ((x (x vec2))
        (y (y vec2))
        (-x (- (x vec2)))
        (-y (- (y vec2))))
    (list
      (vec x y)
      (vec -x y)
      (vec x -y)
      (vec -x -y))))
