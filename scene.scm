;; Scene is a square with side length scene-size
(define scene-size 256)
(define half-scene-size (/ scene-size 2))

(define fov 90)
(define (fov_adjust coord)
  (* coord (tan (/ (radians fov) 2))))

(define scene-spheres
  (list
    (sphere (vec 0 2 4) 2 (material (color 0.2 1 0.2) (color 0 0 0)))))

(define scene-planes
  (list
    (plane (vec 0 0 0) (vec 0 1 0) (color 1 0 0))))
