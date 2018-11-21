(define (ray position direction)
  (list position direction))

;; Getters
(define (r.pos ray)
  (car ray))
(define (r.dir ray)
  (car (cdr ray)))
