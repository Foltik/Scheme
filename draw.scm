;; Rename builtins so we can override them
(define turtle-color color)
(define turtle-setpos setpos)


(define (color r g b)
  (list r g b))


;; Getters
(define (r col)
  (x col))
(define (g col)
  (y col))
(define (b col)
  (z col))


;; Util
(define (setcolor col)
  (turtle-color (colstr col)))

(define (setpos pos)
  (turtle-setpos (x pos) (y pos)))

(define (colstr col)
  (rgb (r col) (g col) (b col)))

(define (colnorm col)
  (vsmap / 255 col))


;; Drawing Util
(define (pixel pos col)
  (setcolor col)
  (penup)
  (setpos pos)
  (pendown)
  (forward 1))

(define (rect p1 p2 col)
  (setcolor col)
  (penup)
  (setpos p1)
  (pendown)
  (begin_fill)
  (setpos (vec (x p2) (y p1)))
  (setpos p2)
  (setpos (vec (x p1) (y p2)))
  (end_fill))
