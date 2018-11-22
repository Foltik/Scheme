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

(define (test pos)
  (display "Sampling " pos "\n")
  '(1 0 0))

(define (draw)
  (speed 0)
  (hideturtle)
  (fill-canvas test 64 -3)
  (exitonclick))

; Please leave this last line alone.  You may add additional procedures above
; this line.
(draw)
  ;;(super-sample disp 16 (vec 0 0))
