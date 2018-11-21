; Camera is aligned along the -z axis.
; +x is right.
; +y is up.
; Sensor is a 2x2 unit square centered on the z-axis.
(define scene_width 200)
(define scene_height 150)

(define aspect_ratio (/ scene_width scene_height))
(define (aspect_adjust coord)
  (* coord aspect_ratio))

(define fov 90)
(define (fov_adjust coord)
  (* coord (tan (/ (radians fov) 2))))
