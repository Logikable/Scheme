(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.
(define (map proc items)
  (cond ((null? items) ())
        (else (cons (proc (car items)) (map proc (cdr items))))
  )
)

(define (cons-all first rests)
  (if (null?  rests) nil
    (cons (append (cons first nil) (car rests)) (cons-all first (cdr rests)))
  ; (map (lambda (x) (append first x)) rests)
))

(define (zip pairs)
  (list (map car pairs) (map cadr pairs))
  ; (list one two)
)

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
  (define (helper s index)
    (if (null? s) s (cons (cons index(cons (car s) nil)) (helper (cdr s) (+ index 1))))
  )
  (helper s 0)
)
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  (cond 
    ((= total 0) nil)
    ((null? denoms) nil)
    ((= (car denoms) total) (cons (cons (car denoms) nil) (list-change total (cdr denoms))))
    ((< total (car denoms)) (list-change total (cdr denoms)))
    (else (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms)) (list-change total (cdr denoms))))
   )
)
  ; END PROBLEM 18

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
           ; (print "hit1")
           (cons form(cons params (let-to-lambda body)))
           ; (list (car expr)(let-to-lambda(cdr expr)))
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           ; (print "hit2")
           (append (list (let-to-lambda (list 'lambda (car (zip values)) (car body)))) (let-to-lambda (cadr (zip values))))
           ; (list 'lambda body)
           ; (print body)
           ; (print values)
           ; (print (car (zip values)))
           ; (print (cadr (zip values)))
           ; (map print (cadr (zip values)))

           ; END PROBLEM 19
           ))
        (else
         ; BEGIN PROBLEM 19
         ; (print "hit3")
         (map let-to-lambda expr)
         ; END PROBLEM 19
         )))
