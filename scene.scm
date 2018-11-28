;; Scene is a square with side length scene-size
(define scene-size 256)
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
