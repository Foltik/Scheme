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

(load "util.scm")
(load "math.scm")
(load "draw.scm")

(load "vec.scm")
(load "ray.scm")
(load "sphere.scm")
(load "plane.scm")
(load "light.scm")
(load "material.scm")

(load "scene.scm")
(load "camera.scm")
(load "trace.scm")

(define (print-sample point)
  (display "Sampling " point "\n")
  (sample point))
  ;;(super-sample sample 4 point))

(define (draw)
  (speed 0)
  (hideturtle)
  (fill-canvas print-sample -3)
  (exitonclick))

; Please leave this last line alone.  You may add additional procedures above
; this line.
(draw)
