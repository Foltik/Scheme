
(define (flat lst)
  (cond
    ((null? lst) '())
    ((pair? (car lst))
     (append (flat (car lst))
             (flat (cdr lst))))
    (else (cons (car lst) (flat (cdr lst))))))

(define (flat-once lst)
  (apply append
         (map (lambda (e) (if (list? e) e (list e)))
              lst)))

(define single-apply apply)
(define (multi-apply fn . args)
  (single-apply fn (flat-once args)))

(define single-map map)
(define (single-map fn lst)
  (if (null? lst)
      '()
      (cons (fn (car lst))
            (single-map fn (cdr lst)))))

(define (multi-map fn . lst)
  (if (null? (car lst))
      '()
      (cons (multi-apply fn (single-map car lst))
            (multi-apply multi-map fn (single-map cdr lst)))))
