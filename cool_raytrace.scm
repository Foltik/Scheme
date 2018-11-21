;;; Scheme Recursive Art Contest Entry
;;;
;;; Please do not include your name or personal info in this file.
;;;
;;; Title: A Morning Filled with 2^32 Suns
;;;
;;; Description:
;;;    Breaking! Moore’s Law dies:
;;;    The hardware just can't keep up,
;;;    With the latest "styles".

; Ver. 1000

; [This source file does not require any modifications to scheme_primitives.py]

; *** READ THIS: ***
; Included are several lines commented out with quadruple semicolons. (;;;;)
; If you want to render the image in <30 minutes instead of >10 hrs, just
; uncomment those lines and run in DrRacket. It will output a .ppm
; (portable pixmap format) which can be opened in a variety of image viewing
; programs, such as Adobe Photoshop.
;     Have fun!

;;;;#lang scheme

; Width and height of image will be twice this value. Default is 512.
; Undefined behavior if this value is not a power of two.
; REMEMBER TO CHANGE levelOfDetail below! Otherwise it will not output full res!
(define halfImgSize 512)

; This MUST be a list of integers in the range:
; [0, (log base 2 of halfImgSize) + 1]
;(define levelOfDetail (list 0   1   2   3   4   5   6   7   8   9   10))
(define levelOfDetail (list 5))
; ------------------------- 1   2   4   8  16  32  64 128 256 512 1024

; ***************
; * VECTOR MATH *
; ***************

(define (getX pos) (car pos))
(define (getY pos) (car (cdr pos)))
(define (getZ pos) (car (cdr (cdr pos))))
(define (newVec x y z) (list x y z))

; Magnitude of vector
(define (vecMagn pos)
  (sqrt (+ (square (getX pos)) (square (getY pos)) (square (getZ pos))))
  )

; Add A and B
(define (vec+ A B)
  (newVec (+ (getX A) (getX B)) (+ (getY A) (getY B)) (+ (getZ A) (getZ B)))
  )

; Subtract B from A
(define (vec- A B)
  (newVec (- (getX A) (getX B)) (- (getY A) (getY B)) (- (getZ A) (getZ B)))
  )

; Multiply vector by a scalar
(define (vec* pos M)
  (newVec (* (getX pos) M) (* (getY pos) M) (* (getZ pos) M)))

; Cross product
(define (vecCross A B)
  (newVec
   (- (* (getY A) (getZ B)) (* (getZ A) (getY B)))
   (- (* (getZ A) (getX B)) (* (getX A) (getZ B)))
   (- (* (getX A) (getY B)) (* (getY A) (getX B)))
   )
  )

; Dot product (assumes A and B are normalized)
(define (vecDot A B)
  (define mulProd (vecMul A B))
  (+ (getX mulProd) (getY mulProd) (getZ mulProd)))

; Multiplies the respective components together
(define (vecMul A B)
  (newVec (* (getX A) (getX B)) (* (getY A) (getY B)) (* (getZ A) (getZ B))))

; Normalizes (sets magnitude to 1)
(define (vecNormal A)
  (vec* A (/ 1 (vecMagn A)))
  )

; **********
; * CAMERA *
; **********

; You can adjust these settings to change the perspective.
; Exploring at 5.556×10^-4 fps might be a little frustrating, though

; (Models a perfect pinhole camera)

; Where the camera is located
(define cameraPos (newVec 0 3 -5)) 
; Point that the camera is looking at
(define cameraLookat (newVec 0 2 0)) 
; Focal length of camera (Camera is 2 units wide and 2 units tall)
; (focal length of 1 is 45 degree FOV)
(define cameraFocalLen 1)


; *******************
; * COMPLEX NUMBERS *
; *******************

; Complex numbers
(define real car)
(define img cdr)
(define cplx cons)
(define (cplx* A B) 
  (let ((a (real A)) (b (img A)) (c (real B)) (d (img B)))
    ; A = a + bi
    ; B = c + di
    ; A * B = (ac - bd) + (ad + bc)i
    (cplx (- (* a c) (* b d)) (+ (* a d) (* b c)))
    )
  )
(define (cplx+ A B)
  (cplx (+ (real A) (real B)) (+ (img A) (img B)))
  )
(define (square n) (* n n))
(define (cplxAbs A)
  (sqrt (+ (square (real A)) (square (img A))))
  )

(define (juliaNext C)
  ; Magic numbers 0.285 and 0.01 taken from Wikipedia's examples of
  ; quadratic Julia sets. Local maxima for prettiness.
  (cplx+ (cplx* C C) (cplx 0.285 0.01))
  )

; Use tail recursion to find
; - The number of iterations if the absolute value of STARTCPLX exceeds 
;   MAXABSVAL under the Julia set
; - False if the value does not escape in MAXITER iterations
(define (juliaIterations startCplx maxIter maxAbsVal n)
  (cond
   ((> n maxIter) #f)
   ((> (cplxAbs startCplx) maxAbsVal) n)
   (else (juliaIterations (juliaNext startCplx) maxIter maxAbsVal (+ n 1)))
   )
 )

; Returns a float between 0 (escapes quickly) to 1 (never escapes)
(define (juliaNormalized startCplx)
  (define numIter (juliaIterations startCplx 100 5 0))
  (if numIter
      (/ numIter 100)
      1.0
      )
  )

; *********************************
; * MISC / MATH HELPER PROCEDURES *
; *********************************

; Floating-point analogue to the modulo operator for signed integers
(define (fmod a b)
  (- a (* (floor (/ a b)) b))
  )

; Triangle wave: (Odd function, kinda like an dangerous sine wave of period 4)
;  x    | F(x)                                                                  
; -1    | -1   | \                /\           1|   /\                /         
; -0.5  | -0.5 |  \              /  \           |  /  \              /          
;  0    |  0   |   \            /    \          | /    \            /           
;  0.5  |  0.5 |    \          /      \         |/      \          /            
;  1    |  1.0 | ----+--------+--------+--------+--------+--------+------       
;  1.5  |  0.5 |    -6\      -4       -2\      /|        2\      /4             
;  2    |  0   |       \    /            \    / |          \    /               
;  2.5  | -0.5 |        \  /              \  /  |           \  /                
;  3    | -1   |         \/                \/ -1|            \/                 
(define (flipflop x)
  (if
   (< (fmod (+ x 1) 4) 2)
   (- (fmod (+ x 1) 2) 1)
   (- 1 (fmod (- x 1) 2))
   )
  )
  
; Returns the pair with the smallest (<) car
(define (minCar A B) (if (< (car A) (car B)) A B))

; Returns the pair with the smallest (<) car in the list of pairs, LST
(define (multiMinCar lst)
  (if
   (null? (cdr lst))
   (car lst)
   (multiMinCar (cons (minCar (car lst) (car (cdr lst))) (cdr (cdr lst))))
   )
  )

; Works like the APPEND in python
; (as opposed to EXTEND, which is how APPEND works in Scheme!)
(define (pyAppend A B)
  (append A (cons B '()))
  )

; Why are these not built-in?
(define (max A B)
  (if (> A B) A B))
(define (min A B)
  (if (< A B) A B))

; Utility: Perform procedure f on all elements in list lst
(define (multiCall f lst)
  (cond
   ((not (null? lst))
    (f (car lst))
    (multiCall f (cdr lst))
    )
   )
  )

; ***********************
; * MATERIAL PROCEDURES *
; ***********************

; Materials are vectors containing three values:
; getX: diffuse
; getY: specular (0 0 0 = no specular color)
; getZ: emissive

; The following auxiliary procedures allow for malformed vectors to have
; default values. (black for specular and emissive)
; Note: emissive is currently unused

(define (getDiffuse material)
  (car material))
(define (getSpecular material)
  (if
   (null? (cdr material))
   (newVec 0 0 0)
   (getY material)))
(define (getEmissive material)
  (if
   (null? (cdr material))
   (newVec 0 0 0)
   (getSpecular (cdr material))))

; Checkerboard material func
(define (mfCheckerboard pos)
  (list
   (if
    (= (modulo (+ (floor (getX pos)) 
                  (floor (getY pos)) 
                  (floor (getZ pos))) 2) 1)
    (newVec 0.7 0.7 0.7)
    (newVec 0.5 0.5 0.5)
    )
   (newVec 0.2 0.2 0.2)
   )
  )

; *******************************
; * DISTANCE / OBJECT FUNCTIONS *
; *******************************

; Plane (+Y is perpendicular to surface)
(define (dfPlane pos yLoc)
  (list
   (- (getY pos) yLoc)
   (newVec 0 1 0)
   )
 )

; Sphere
(define (dfSphere pos center radius)
  (list
   (- (vecMagn (vec- pos center)) radius)
   (vecNormal (vec- pos center))
   )
  )

; Returns a (distance . material) pair, where distance is a float and material
; is a 3D vector containing (diffuse specular emission)
(define (globalDistFunc pos)
  (multiMinCar (list
                
                ; STUFF IN THIS LIST WILL BE RENDERED!
                ; (try add a second pearl, for instance)
                
                ; Nice polished floor
                (pyAppend 
                 (dfPlane pos 0) ; Distance
                 (mfCheckerboard pos) ; Material
                 )

                ; Pearl
                (pyAppend
                 (dfSphere pos (newVec 0 2 0) 2) ; Distance
                 (list (newVec 0.4 0.4 0.4) (newVec 0.6 0.6 0.6)) ; Material
                 )
   ))
  )

; ***************************
; * RASTERIZING / RENDERING *
; ***************************

; Returns the color of the skybox in that direction (ignores all solid objects 
; by definition)
(define (calcSkyboxColor direction)
  (define powerA 
    (juliaNormalized 
      (cplx (flipflop (* (getY direction) 3)) 
            (flipflop (* (getX direction) 3))
        )
      )
    )
  (define powerB 
    (juliaNormalized 
      (cplx (flipflop (* (getX direction) 2)) 
            (flipflop (* (getY direction) 2))
        )
      )
    )
  (vec+ 
    ; This is where the two "galaxy" colors come from
    (newVec (expt powerA 16) (expt powerA 2) powerA)
    (newVec powerB (expt powerB 2) (expt powerB 16))
    )
  )
  
(define (directionalLight position normal lightDir)
  (define penumbra 
    ; Returns false if something was hit (i.e. full occlusion)
    (raymarch (vec+ position (vec* lightDir 0.05)) lightDir 50 #f 0 1)
    )
  (if
   penumbra
   (* (max 0 (vecDot lightDir normal)) penumbra)
   0
   )
  )

; To be multiplied with a surface's material's diffuse
(define (calcDiffuseStrength position normal)
  (vec+
   (vec* (newVec 1.0 0.7 0.13) ; "yellow" light
     (directionalLight position normal (vecNormal (newVec 1 1 -1))))
   (vec* (newVec 0.15 0.4 1.0) ; "blue" light
     (directionalLight position normal (vecNormal (newVec -1 1 -1))))
   )
  )

; To be multiplied with a surface's material's specular
(define (calcSpecularStrength position normal direction maxSteps)
  (define reflected
    (vec- direction (vec* (vec* normal (vecDot direction normal)) 2)))
  (raymarch (vec+ position (vec* reflected 0.05)) reflected maxSteps #t 0 1)
  )

; Calculate the final color of a surface given
; its position in the world and the surface data
(define (calcSurfaceColor position surface direction maxSteps)
  (define surfaceMat (getZ surface))
  (vec+ (vec+ ; Final color is the sum of

         ; Diffuse color
         (vecMul
           (getDiffuse surfaceMat)
           (calcDiffuseStrength position (getY surface))
           )

         ; Reflective (specular) color
         (if (<= (vecMagn (getSpecular surfaceMat)) 0.001)
             (newVec 0 0 0) ; No calculation needed if no reflection
             (vecMul
               (getSpecular surfaceMat) 
               (calcSpecularStrength position (getY surface) direction maxSteps)
               )
           )

       )

       (getEmissive surfaceMat) ; Emissive color

     )
  )


; Returns incoming radiance traveling toward STARTPOS at angle -DIRECTION
; Is tail recursive, but makes frequent calls to other non-tail recursive
; functions such as calcDiffuseStrength
; If OUTPUTTYPE, then color is returned. Otherwise return whether or not the ray
; escaped to the void (true value returned used for penumbras)
; TOTALDISTANCE should be initialized as 0
; CLOSESTCALL should be initialized as 1
(define (raymarch startPos direction maxSteps outputType totalDistance closestCall)
  (define surface (globalDistFunc startPos))
  (cond
   
   ; If it reached the max iteration count or is beyond the max render distance
   ; or is more than 100 units away from the nearest object
   ((or (= maxSteps 0) (> (getZ startPos) 40) (> (car surface) 100)) 
    ; Ray marching is escaping into space
    
    (if outputType (calcSkyboxColor direction) closestCall)
    )
    ; 74 c 597 c 325 c
   
   ; 0.001 is march epsilon. lower values increase precision
   ((> (car surface) 0.001) 
    ; Have not hit anything yet,
    ; tail recurse until something is hit or until the ray escapes into the void
    
    (raymarch
     (vec+ startPos (vec* direction (car surface)))
     direction
     (- maxSteps 1)
     outputType
     (+ totalDistance (car surface))

     ; Penumbra factor = 4 (bigger values -> sharper shadows)
     (if (zero? totalDistance)
       1 
       (min closestCall (/ (* 4 (car surface)) totalDistance)))
     )
    )
   
   
   (else

    ; Hit found, calculate final color.
    ; startPos is the position
    ; (getX surface) is the distance
    ; (getY surface) is the normal (pre-normalized)
    ; (getZ surface) is material data
    
    (if outputType 
      (calcSurfaceColor startPos surface direction (- maxSteps 1)) 
      #f)

    )
   )
  )

; Converts my 3D vector into a turtle-compliant RGB triplet (using tonemapping)
(define (toRGB pos)
  (rgb (getX pos) (getY pos) (getZ pos))
  )

(define (tonemap pos)
  (define r (getX pos))
  (define g (getY pos))
  (define b (getZ pos))
  ; Reinhard tonemapping
  ; (newVec (/ r (+ r 1)) (/ g (+ g 1)) (/ b (+ b 1)))
  (newVec (min r 1) (min g 1) (min b 1))
  )

; Calculate camera matrix (orthonormal)
(define cameraForward (vecNormal (vec- cameraLookat cameraPos)))
(define cameraRight (vecNormal (vecCross (newVec 0 1 0) cameraForward)))
(define cameraUp (vecNormal (vecCross cameraForward cameraRight)))

; Returns the final color used for the pixel at x, y
(define (calcColor x y)
  ; Camera is looking down the positive z axis (right-handedness not important
  ; because no "real" matrices are used here)
  (tonemap (raymarch 
   ; Camera position
   cameraPos 
   ; Ray direction
   (vecNormal 
     (vec+ 
       (vec+ 
         (vec* cameraRight (/ x halfImgSize)) 
         (vec* cameraUp (/ y halfImgSize))
         ) 
       (vec* cameraForward cameraFocalLen))
     )
   200 ; Maximum number of iterations
   #t ; Output color (as opposed to mere occlusion data)
   0 ; Initial value for total distance traveled
   1 ; Initial value for closest call
   ))
  )

; Same as calcColor, but performs x2 super sampling.
; This makes the calculation take x4 as long, but the end result is totally
; worth it.
(define (calcColorSS x y)
  (vec* 
    (vec+ 
      (vec+ 
        (calcColor x y)
        (calcColor (+ x 0.5) y)
        )
      (vec+ 
        (calcColor x (+ y 0.5))
        (calcColor (+ x 0.5) (+ y 0.5))
        )
      )
    0.25)
  )

; Draws a rectangle on the screen
(define (rect x1 y1 x2 y2 c)
  (let ((x2 (- x2 1)) (y2 (- y2 1)))
    (color c)
    (setposition x1 y1)
    (pendown)
    (begin_fill)
    (setposition x2 y1)
    (setposition x2 y2)
    (setposition x1 y2)
    (end_fill)
    (penup)
   )
  )
  
; Dummy procedures for .ppm output
;;;;(define (speed x) '())
;;;;(define (setposition x y) '())
;;;;(define (color x) '())
;;;;(define (penup) '())
;;;;(define (pendown) '())
;;;;(define (begin_fill) '())
;;;;(define (end_fill) '())
;;;;(define (rgb x y z) '())
;;;;(define (exitonclick) '())

; Where the magic happens
(define (draw)
  (speed 0)
  (define (spaceFill x y size level)
    (if
     (= level 0)
     (rect x y (+ x size) (+ y size) (toRGB (calcColorSS x y)))
     (cond
      ((>= size 1)
       (if (> level 1) (spaceFill x y (/ size 2) (- level 1)) '())
       (spaceFill (+ x (/ size 2)) y (/ size 2) (- level 1))
       (spaceFill x (+ y (/ size 2)) (/ size 2) (- level 1))
       (spaceFill (+ x (/ size 2)) (+ y (/ size 2)) (/ size 2) (- level 1))
       )
      )
     )
    )
  
;;;;  (call-with-output-file "output.ppm" #:exists 'replace
;;;;    (lambda (ofile)
;;;;      (write-string "P3" ofile) (newline ofile)
;;;;      (write (* halfImgSize 2) ofile) (write-string " " ofile) 
;;;;      (write (* halfImgSize 2) ofile) (newline ofile)
;;;;      (write-string "255" ofile) (newline ofile)
;;;;      (define (integerPlease x)
;;;;        (exact-floor x)
;;;;        )
;;;;      (define (outputLoop x y)
;;;;        (define rgb (calcColorSS (- x halfImgSize) (- halfImgSize y)))
;;;;        (write (integerPlease(* 255 (getX rgb))) ofile)
;;;;        (write-string " " ofile)
;;;;        (write (integerPlease(* 255 (getY rgb))) ofile)
;;;;        (write-string " " ofile)
;;;;        (write (integerPlease(* 255 (getZ rgb))) ofile)
;;;;        (write-string " " ofile)
;;;;
;;;;        (cond
;;;;          ((and 
;;;;            (= x (- (* halfImgSize 2) 1)) 
;;;;            (= y (- (* halfImgSize 2) 1))) '())
;;;;          ((= x (- (* halfImgSize 2) 1))
;;;;           (newline ofile)
;;;;           (if (= (modulo y 5) 0)(displayln y) '())
;;;;           (outputLoop 0 (+ y 1))
;;;;           )
;;;;          (else
;;;;           (outputLoop (+ x 1) y)
;;;;           )
;;;;          )
;;;;        
;;;;        )
;;;;      (outputLoop 0 0)
;;;;      ))

;;;;
    ; IMPORTANT IMPORTANT IMPORTANT IMPORTANT
    ; "!! Comment out the next two lines if you want to output .ppm instead !!"
    (multiCall (lambda (level) (spaceFill (- 0 halfImgSize) (- 0 halfImgSize)
     (* halfImgSize 2) level)) levelOfDetail)
;;;;
  
  (setposition 0 (- 0 halfImgSize))
  (hideturtle)
  (exitonclick))

; Please leave this last line alone.  You may add additional procedures above
; this line.
(draw)
