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

;; Rename builtins so we can override them 
(define setcolor color)
(define single-map map)
(define single-apply apply)

;; General Util
;; Flatten a list by one level
;; (flat-once (1 2 ((3 4))) -> (1 2 (3 4))
(define (flat-once lst)
  (single-apply append
         (single-map (lambda (e) (if (list? e) e (list e)))
              lst)))

;; Redefine apply to take multiple parameters and a list
;; (apply + 5 '(1 1 1)) -> (+ 5 1 1 1)
(define (apply fn . args)
  (single-apply fn (flat-once args)))

;; Redefine map to map across multiple lists
;; (apply + '(1 2 3) '(1 2 3)) -> ((+ 1 1) (+ 2 2) (+ 3 3))
(define (map fn . lst)
  (if (null? (car lst))
      '()
      (cons (apply fn (single-map car lst))
            (apply map fn (single-map cdr lst)))))

(define (foldl fn id list)
  (if (null? list)
      id
      (foldl fn (fn id (car list)) (cdr list))))

;; Zip lists
;; (zip '(1 2) '(a b) '(y z)) -> ((1 a y) (2 b z))
(define (zip . lsts)
  (apply map list lsts))
      

;; Math Util
(define (square n)
  (* n n))


;; Vector Util
(define (vec . args)
  (apply list args))
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
;; Util
(define (vec-mag vec)
  (sqrt (apply + (map square vec))))
(define (vec-map fn vecs)
  (apply map fn vecs))
  
;; Scalar Operations
(define (vs+ vec k)
  (map (lambda (n) (+ n k)) vec))
(define (vs- vec k)
  (map (lambda (n) (- n k)) vec))
(define (vs* vec k)
  (map (lambda (n) (* n k)) vec))
(define (vs/ vec k)
  (map (lambda (n) (/ n k)) vec))
;; Vector-Vector Operations
(define (v+ . vecs)
  (vec-map + vecs))
(define (v- . vecs)
  (apply map - vecs))
(define (v* . vecs)
  (apply map * vecs))
(define (v/ . vecs)
  (vec-map - vecs))
(define (v. a b)
  (apply + (v* a b)))
(define (vx a b)
  (list
    (-
      (* (y a) (z b))
      (* (z a) (y b)))
    (-
      (* (z a) (x b))
      (* (x a) (z b)))
    (-
      (* (x a) (y b))
      (* (y a) (x b)))))



;; Color Util
(define (color r g b)
  (list r g b))
(define (col r g b)
  (color r g b))
(define (c r g b)
  (color r g b))
;; Getters
(define (r col)
  (x col))
(define (g col)
  (y col))
(define (b col)
  (z col))
;; Util
(define (col->str col)
  (rgb (r col) (g col) (b col)))
(define (col-norm col)
  (vs/ col 255))



;; Sphere Util
(define (sphere pos radius color)
  (list pos radius color))
(define (sph pos radius color)
  (sphere pos radius color))
(define (s pos radius color)
  (sphere pos radius color))
;; Getters
(define (s-pos sphere)
  (car sphere))
(define (s-rad sphere)
  (car (cdr sphere)))
(define (s-col sphere)
  (car (cdr (cdr sphere))))
;; Util
(define (s-intersect? sphere ray)
  (define hyp (v- (s-pos sphere) (r-pos ray)))
  (define adj (v. hyp (r-dir ray)))
  (define d (- (v. hyp hyp) (* adj adj)))
  (< d (* (s-rad sphere) (s-rad sphere))))



;; Ray Util
(define (ray position direction)
  (list position direction))
;; Getters
(define (r-pos ray)
  (car ray))
(define (r-dir ray)
  (car (cdr ray)))

           
           
      


;; Scene Util
; Camera is aligned along the -z axis.
; +x is right.
; +y is up.
; Sensor is a 2x2 unit square centered on the z-axis.
;;(define scene_width 800)
;;(define scene_height 600)
;;(define scene_width (screen_width))
;;(define scene_height (screen_height))
(define scene_width 200)
(define scene_height 150)

(define aspect_ratio (/ scene_width scene_height))
(define (aspect_adjust coord)
  (* coord aspect_ratio))

(define fov 90)
(define (fov_adjust coord)
  (* coord (tan (/ (radians fov) 2))))
  
;; (define (sensor_x coord)
;;   (fov_adjust (aspect_adjust (- (* 2 (/ (+ 0.5 coord) scene_width)) 1))))
;; (define (sensor_y coord)
;;   (fov_adjust (- 1 (* 2 (/ (+ 0.5 coord) scene_height)))))

;; (define (range first last)
;;   (if (> first last)
;;       '()
;;       (cons first (range (+ first 1) last))))

;; (define (loop i j i_max j_max fn)
;;   (cond
;;     ((and (< i i_max) (< j j_max))
;;      (begin
;;        (fn i j)
;;        (loop i (+ j 1) i_max j_max fn)))
;;     ((= j j_max)
;;      (loop (+ i 1) 0 i_max j_max fn))
;;     ((= i i_max)
;;      '())))
      
;; Drawing Util
(define (pixel x y col)
  (setcolor (col->str col))
  (penup)
  (setpos x y)
  (pendown)
  (forward 1))

(define (rect x1 y1 x2 y2 col)
  (setcolor (col->str col))
  (penup)
  (setpos x1 y1)
  (pendown)
  (begin_fill)
  (setpos x2 y1)
  (setpos x2 y2)
  (setpos x1 y2)
  (end_fill))

(define (sample x y)
  '())

(define (scale_offset n)
  (* n (/ (sqrt 5) 2)))

(define (rotate_offset_x x y)
  (define theta (atan 0.5))
  (- (* x (cos theta)) (* y (sin theta))))

(define (rotate_offset_y x y)
  (define theta (atan 0.5))
  (+ (* y (cos theta)) (* x (sin theta))))

(define (disp x y)
  (display (list x y))
  (define x (* x 100))
  (define y (* y 100))
  ;;(define x (rotate_offset_x x y))
  ;;(define y (rotate_offset_y x y))
  (rect x y (+ x 1) (+ y 1) '(1 0 0))
  (vec 0 0 0))


(define (subdivide fn offset x y)
  (vs/
    (v+
      (v+
        (fn (+ x offset) (+ y offset))
        (fn (- x offset) (+ y offset)))
      (v+
        (fn (+ x offset) (- y offset))
        (fn (- x offset) (- y offset))))
    4))

(define (exponential_subdivide factor offset x y)
  (if (= factor 2)
      ;; disp -> sample
      (subdivide disp offset x y)
      (subdivide
        (lambda (x y)
          (exponential_subdivide (/ factor 2) (/ offset 2) x y))
        offset x y)))

(define (super_sample factor x y)
  (exponential_subdivide factor 0.5 x y))


(define (draw)
  ;;(speed 0)
  ;;(hideturtle)
  ;;(super_sample 2 0 0)

  
  (exitonclick))

; Please leave this last line alone.  You may add additional procedures above
; this line.
(draw)
