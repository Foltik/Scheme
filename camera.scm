;; Properties
(define camera-pos (vec 0 2 -3))
(define camera-lookat (vec 0 1 0))
(define camera-fov 45)
(define camera-focal 1)

;; Direction vectors
(define camera-forward
  (vnorm (vmap - camera-pos camera-lookat)))
(define camera-right
  (vnorm (vcross (vec 0 1 0) camera-forward)))
(define camera-up
  (vnorm (vcross camera-forward camera-right)))
