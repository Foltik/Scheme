;;; Scheme Recursive Art Contest Entry
;;;
;;; Please do not include your name or personal info in this file.
;;;
;;; Title: <Your title here>
;;;
;;; Description:
;;;   <It's your masterpiece.
;;;    Use these three lines to describe
;;;    its inner meaning.>

;;;; UTIL ;;;;
;; Rename builtins so we can override them 
(define single-map map)
(define single-apply apply)
(define single-display display)

;; Redefine apply to take multiple parameters to append in additon to a list
;; (apply + 5 '(1 1 1)) -> (+ 5 1 1 1)
(define (apply fn . args)
  (single-apply
    fn
    (single-apply
      append
      (single-map (lambda (e) (if (list? e) e (list e))) args))))

;; Redefine map to work across multiple lists
;; (map + '(1 2 3) '(1 2 3)) -> ((+ 1 1) (+ 2 2) (+ 3 3))
(define (map fn . lst)
  (if (null? (car lst))
      '()
      (cons (apply fn (single-map car lst))
            (apply map fn (single-map cdr lst)))))

;; Fold Left
(define (foldl fn acc list)
  (if (null? list)
      acc
      (foldl fn (fn acc (car list)) (cdr list))))

;; List of value k repeated n times
;; (repeat 3 0) -> (0 0 0)
(define (repeat-helper n k lst)
  (if (= 0 n)
      lst
      (repeat-helper (- n 1) k (cons k lst))))
(define (repeat n k)
  (repeat-helper n k '()))

;; Display all arguments in a row
(define (display . args)
  (map single-display args))
;;;; END UTIL ;;;;




;;;; MATH ;;;;
(define pi 3.141592653589793)

;; Min
(define (min . args)
  (fold (lambda (a b) (if (< a b) a b)) (car args) (cdr args)))

;; Min with custom function to determine comparison property
(define (minfn fn . args)
  (fold (lambda (a b) (if (< (fn a) (fn b)) a b)) (car args) (cdr args)))

;; Max
(define (max . args)
  (fold (lambda (a b) (if (> a b) a b)) (car args) (cdr args)))

;; Square a number
(define (square n)
  (* n n))

;; Clip a number between bounds
(define (clip lower upper n)
  (max (min n upper) lower))

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
;;;; END MATH ;;;;




;;;; DRAW ;;;;
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

(define (colclip col)
  (vmap (lambda (color) (clip 0 1 color)) col))


;; Drawing Util
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
;;;; END DRAW ;;;;




;;;; VECTORS ;;;;
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
;;;; END VECTORS




;;;; RAYS ;;;;
(define (ray position direction)
  (list position direction))

;; Getters
(define (r.pos ray)
  (car ray))
(define (r.dir ray)
  (car (cdr ray)))

;; Util
(define (rmarch r t)
  (ray
    (vmap + (r.pos r) (vsmap * t (r.dir r)))
    (r.dir r)))

(define (r.reflect r normal)
  (ray (r.pos r) (vmap - (r.dir r) (vsmap * (* 2 (vdot (r.dir r) normal)) normal))))
;;;; END RAYS




;;;; SPHERES ;;;;
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
;;;; END SPHERES




;;;; PLANES ;;;;
(define (plane pos normal material)
  (list pos normal material))

;; Getters
(define (p.pos plane)
  (car plane))
(define (p.norm plane)
  (car (cdr plane)))
(define (p.mat plane)
  (car (cdr (cdr plane))))

(define (p.dist plane ray)
  (let ((denom (vdot (vmap - (p.norm plane)) (r.dir ray))))
    (if (> denom 0.000001)
        (let ((dist (/ (vdot (vmap - (p.pos plane) (r.pos ray)) (vmap - (p.norm plane))) denom)))
          (if (>= dist 0)
              dist
              1000000))
        1000000)))
;;;; END PLANES ;;;;



;;;; LIGHTS ;;;;
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
;;;; END LIGHTS ;;;;




;;;; MATERIALS ;;;;
(define (material color albedo reflectivity)
  (list color albedo reflectivity))

;; Getters
(define (m.color material)
  (car material))

(define (m.albedo material)
  (car (cdr material)))

(define (m.reflect material)
  (car (cdr (cdr material))))
;;;; END MATERIALS;;;;




;;;; SCENE ;;;;
;; Scene is a square with side length scene-size
(define scene-size 1024)
(define half-scene-size (/ scene-size 2))

(define scene-spheres
  (list
    (sphere (vec 0 5 8) 2 (material (color 0.99 0.71 0.08) 0.18 0.2))
    (sphere (vec 7.36 8.126 8) .5 (material (color 0 0.2 0.38) 0.18 0.6))
    (sphere (vec -7.36 1.874 2.32) .5 (material (color 0 0.2 0.38) 0.18 0.6))
    (sphere (vec 5.21 7.21 2.32) .5 (material (color 0 0.2 0.38) 0.18 0.6))
    (sphere (vec -5.21 2.8 2.32) .5 (material (color 0 0.2 0.38) 0.18 0.6))
    (sphere (vec 5.21 7.21 13.28) .5 (material (color 0 0.2 0.38) 0.18 0.6))
    (sphere (vec -5.21 2.8 13.68) .5 (material (color 0 0.2 0.38) 0.18 0.6))
    (sphere (vec 0 5 16) .5 (material (color 0 0.2 0.38) 0.18 0.6))
    (sphere (vec -0 5 0) 1.5 (material (color 0.2 0.2 1.0) 0.18 0))
    (sphere (vec -18 18 17) 1 (material (color 1 1 1) 0.18 .75))
    (sphere (vec 3 3 0) 1 (material (color 1 1 1) 0.18 .75))))
 
(define scene-planes
  (list
    (plane (vec 0 0 100) (vec 0 0 -1) (material (color 0.13 0.13 0.13) 0.18 0.32))
    (plane (vec 0 -2 0) (vec 0 1 0) (material (color 0.13 0.13 0.13) 0.18 0.45))))
 
 
(define scene-lights
  (list
    (point-light (vec 0 0 3) (color 0.8 0.6 0.3) 10000)
    (point-light (vec -20 2 18) (color 0.8 0.45 0.3) 10000)
    (point-light (vec 18 2 18) (color 0 0.7 0.7) 10000)
    (point-light (vec 0 5 5) (color 0.8 0.6 0.3) 100)
    (point-light (vec -3 5 8) (color 0.8 0.6 0.3) 100)
    (point-light (vec 3 5 8) (color 0.8 0.6 0.3) 100)
    (point-light (vec 0 8 5) (color 0.8 0.6 0.3) 100)
    (directional-light (vec -1 -1 0.4) (color 1 1 1) 20)))
;;;; END SCENE ;;;;




;;;; CAMERA ;;;;
;; Properties
(define camera-pos (vec 0 6 -3))
(define camera-lookat (vec 0 5 8))
(define camera-fov 45)
(define camera-focal 1)

;; Direction vectors
(define camera-forward
  (vnorm (vmap - camera-lookat camera-pos)))
(define camera-right
  (vnorm (vcross (vec 0 1 0) camera-forward)))
(define camera-up
  (vnorm (vcross camera-forward camera-right)))

(define (prime-ray pos)
  (ray
    camera-pos
    (vnorm
      (vmap +
        (vsmap * (/ (x pos) half-scene-size) camera-right)
        (vsmap * (/ (y pos) half-scene-size) camera-up)
        (vsmap * camera-focal camera-forward)))))
;;;; END CAMERA ;;;;




;;;; TRACE
(define (nearest-object ray)
  (apply minfn car
    (append
      (map
        (lambda (sphere)
          (list
           (s.dist sphere ray)
           (s.norm sphere ray)
           (s.mat sphere)))
        scene-spheres)
      (map
        (lambda (plane)
          (list
            (p.dist plane ray)
            (p.norm plane)
            (p.mat plane)))
        scene-planes))))
      
(define max-steps 100)
(define (trace-helper ray steps distance output-color)
  (let ((nearest (nearest-object ray)))
    (cond
      ((or (= steps max-steps) (> (z (r.pos ray)) 100) (> (car nearest) 100))
       (if (eq? output-color #t)
           (color 0 0 0)
           (list #f 1000000000)))
      ((< (car nearest) 0.001)
        (let ((norm (car (cdr nearest)))
              (mat (car (cdr (cdr nearest)))))
          (if (eq? output-color #t)
            (if (> (m.reflect mat) 0)
              (vmap +
                (vsmap * (- 1 (m.reflect mat)) (m.color mat))
                (vsmap * (m.reflect mat)
                  (trace (r.reflect ray norm) #t)))
              (colclip
                (vmap *
                  (m.color mat)
                  (apply vmap +
                    (map
                      (lambda (light) (l.calc-color light (r.pos ray) norm (m.albedo mat)))
                      scene-lights)))))
            (list #t distance))))
      (else
       (trace-helper
         (rmarch ray (car nearest))
         (+ steps 1)
         (+ distance (car nearest))
         output-color)))))
       
(define (trace ray output-color)
  (trace-helper ray 0 0 output-color))

(define (sample point)
  (trace (prime-ray point) #t))

(define (make-offsets pos offset)
  (vmaplst + pos (offset-permutations offset)))

(define (super-sample-helper fn factor pos offset)
  (cond
   ((= factor 1)
    (fn pos))
   (else
    (apply vavg
      (map
        (lambda (newpos)
          (super-sample-helper fn (/ factor 4) newpos (vsmap / 2 offset)))
        (make-offsets pos offset))))))

(define (super-sample fn factor pos)
  (super-sample-helper fn factor pos (vec 0.5 0.5)))

(define (fill fn pos size lod)
  (cond
    ((= lod 1)
     (rect pos (vsmap + size pos) (fn pos)))
    (else
     (let ((halfsize (/ size 2))
           (sublod (- lod 1)))
       (fill fn pos halfsize sublod)
       (fill fn (vec (+ (x pos) halfsize) (y pos)) halfsize sublod)
       (fill fn (vec (x pos) (+ (y pos) halfsize)) halfsize sublod)
       (fill fn (vsmap + halfsize pos) halfsize sublod)))))

(define (fill-canvas fn lodoffset)
  (fill fn (vecn 2 (- half-scene-size)) scene-size (+ 1 (log2 scene-size) lodoffset)))
;;;; END TRACE




(define (print-sample point)
  (display "Sampling " point "\n")
  (super-sample sample 4 point))

(define (draw)
  (speed 0)
  (hideturtle)
  (fill-canvas print-sample 0)
  (exitonclick))

; Please leave this last line alone.  You may add additional procedures above
; this line.
(draw)
