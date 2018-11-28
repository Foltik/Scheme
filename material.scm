(define (material color albedo reflectivity)
  (list color albedo reflectivity))

;; Getters
(define (m.color material)
  (car material))

(define (m.albedo material)
  (car (cdr material)))

(define (m.reflect material)
  (car (cdr (cdr material))))

