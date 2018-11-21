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

;; Fold Left
(define (foldl fn id list)
  (if (null? list)
      id
      (foldl fn (fn id (car list)) (cdr list))))

;; Zip elements of lists
;; Also unzips lists if called again
;; (zip '(1 2) '(a b) '(y z)) -> ((1 a y) (2 b z))
(define (zip . lsts)
  (apply map list lsts))

;; Display all arguments in a row
(define (display . args)
  (map single-display args))
