;; Camera is aligned along the -z axis.
;; +x is right.
;; +y is up.
;; Sensor is a 2x2 unit square centered on the z-axis.

;; Scene is a square with side length scene-size
(define scene-size 1024)

(define fov 90)
(define (fov_adjust coord)
  (* coord (tan (/ (radians fov) 2))))

(define spheres
  (list
    (sphere (vec 0 0 -5) 1 (vec 0.4 1.0 0.4))))

