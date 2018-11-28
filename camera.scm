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
