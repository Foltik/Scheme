(define (vec . args)
  (apply list args))

(define (vecn n k)
  (repeat n k))

;; Getters
(define (x vec)
  (car vec))
(define (y vec)
  (car (cdr vec)))
(define (z vec)
  (car (cdr (cdr vec))))
(define (w vec)
  (car (cdr (cdr (cdr vec)))))
(define (i vec)
  (x vec))
(define (j vec)
  (y vec))
(define (k vec)
  (z vec))
(define (l vec)
  (w vec))

;; Map the function fn over the vectors
(define (vmap fn . vecs)
  (apply map fn vecs))

;; Map a function with a scalar over the vector
(define (vsmap fn k vec)
  (map (lambda (n) (fn n k)) vec))

;; Map the function fn with vector vec over the vectors
(define (vmaplst fn vec vecs)
  (map (lambda (v) (vmap fn vec v)) vecs))
  
;; Take the average of all the vectors
(define (vavg . vecs)
  (vsmap / (length vecs)
    (apply vmap + vecs)))

;; Magnitude of a vector
(define (vmag vec)
  (sqrt (apply + (map square vec))))

;; Normalized vector
(define (vnorm vec)
  (vsmap / (vmag vec) vec))

;; Dot product of two vectors
(define (vdot a b)
  (apply + (vmap * a b)))

;; Cross product of two vectors
(define (vcross a b)
  (vec
    (-
      (* (y a) (z b))
      (* (z a) (y b)))
    (-
      (* (z a) (x b))
      (* (x a) (z b)))
    (-
      (* (x a) (y b))
      (* (y a) (x b)))))
