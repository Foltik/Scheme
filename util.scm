;; Rename builtins so we can override them 
(define single-map map)
(define single-apply apply)
(define single-display display)

;; Redefine apply to take multiple parameters to append in additon to a list
;; (apply + 5 '(1 1 1)) -> (+ 5 1 1 1)
(define (apply fn . args)
  (single-apply
    fn
    (single-apply
      append
      (single-map (lambda (e) (if (list? e) e (list e))) args))))

;; Redefine map to work across multiple lists
;; (map + '(1 2 3) '(1 2 3)) -> ((+ 1 1) (+ 2 2) (+ 3 3))
(define (map fn . lst)
  (if (null? (car lst))
      '()
      (cons (apply fn (single-map car lst))
            (apply map fn (single-map cdr lst)))))

;; Fold right
(define (foldr fn end lst)
  (if (null? lst)
      end
      (fn (car lst) (foldr fn end (cdr lst)))))
(define reduce foldr)

;; Fold Left
(define (foldl fn acc list)
  (if (null? list)
      acc
      (foldl fn (fn acc (car list)) (cdr list))))
(define fold foldl)


;; Zip elements of lists
;; Also unzips lists if called again
;; (zip '(1 2) '(a b) '(y z)) -> ((1 a y) (2 b z))
(define (zip . lsts)
  (apply map list lsts))


;; List of value k repeated n times
;; (repeat 3 0) -> (0 0 0)
(define (repeat-helper n k lst)
  (if (= 0 n)
      lst
      (repeat-helper (- n 1) k (cons k lst))))
(define (repeat n k)
  (repeat-helper n k '()))

;; Display all arguments in a row
(define (display . args)
  (map single-display args))
