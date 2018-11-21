; Camera is aligned along the -z axis.
; +x is right.
; +y is up.
; Sensor is a 2x2 unit square centered on the z-axis.
(define scene-width 1024)
(define scene-height 1024)

(define fov 90)
(define (fov_adjust coord)
  (* coord (tan (/ (radians fov) 2))))
