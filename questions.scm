(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
  (map (lambda (lst) (cons first lst)) rests)
)

(define (zip pairs)
 (define (zip-helper pairs lst1 lst2)
 (if (null? pairs) (append (list lst1) (list lst2))
   (zip-helper (cdr pairs) (append lst1 (list (caar pairs))) (append lst2 (list (car(cdar pairs)))))
 )
)
 (zip-helper pairs nil nil)
)

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate lst)
  (define (get-lst lst current index)
    (if (null? lst) current
        (get-lst (cdr lst) (append current (list(list index (car lst)))) (+ 1 index))
      )
  )
  (get-lst lst () 0)
)
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  (define (list-change-helper remaining denoms current-lst)
  (cond
    ((or (null? denoms) ( < remaining 0)) nil)
    ((= remaining 0) (list current-lst))
    (else
     (append
      (list-change-helper (- remaining (car denoms)) denoms (append current-lst (list (car denoms))))
      (list-change-helper remaining (cdr denoms) current-lst)
     )
    )
  )
)
(list-change-helper total denoms nil)
)

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 19
          expr
         ; END PROBLEM 19
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
          (append (list form params) (map let-to-lambda body))
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
          (append (list (list 'lambda (car (zip values)) (let-to-lambda (car body)))) (map let-to-lambda (car (cdr (zip values)))))
           ; END PROBLEM 19
           ))
        (else
         ; BEGIN PROBLEM 19
         (map let-to-lambda expr)
         ; END PROBLEM 19
         )))
