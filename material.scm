(define (material diffuse specular)
  (list diffuse specular))

;; Getters
(define (m.diff material)
  (car material))

(define (m.spec material)
  (car (cdr material)))


