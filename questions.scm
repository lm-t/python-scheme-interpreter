(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cddr x) (cdr (cdr x)))
(define (cadar x) (car (cdr (car x))))

; Some utility functions that you may find useful to implement.
(define (map proc items)
  (if (null? items) nil
    (cons (proc (car items)) (map proc (cdr items)))
    )
  )

(define (cons-all first rests)
  (if (null? rests) nil
    (cons (cons first (car rests)) (cons-all first (cdr rests)))
    )
  )
(define (zip pairs)
(cond ((null? pairs) (list nil nil))
      ((null? (car pairs)) nil)
      (else (cons (map (lambda (s) (car s)) pairs) (zip (map (lambda (s) (cdr s)) pairs))))
  )
)
;; Problem 18
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN Question 18
  (define (enumerate-iter s count)
    (if (null? s) nil
      (cons (list count (car s)) (enumerate-iter (cdr s) (+ count 1)))
      )
    )
    (enumerate-iter s 0)
  )
  ; END Question 18

;; Problem 19
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN Question 19
  (cond ((< total 0) nil)
        ((= total 0) (list nil))
        ((null? denoms) nil)
        (else (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms))
                    (list-change total (cdr denoms))
              )
          )
    )
  )
  ;END Question 19
(define (list-count total denoms)
  (cond ((< total 0) 0)
        ((= total 0) 1)
        ((null? denoms) 0)
        (else (+ (list-count (- total (car denoms)) denoms) (list-count total (cdr denoms))))
      )
)
;; Problem 20
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (analyze expr)
  (cond ((atom? expr)
         ; BEGIN Question 20
         expr
         ; END Question 20
         )
        ((quoted? expr)
         ; BEGIN Question 20
         expr
         ; END Question 20
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN Question 20
           (cons form (cons params (map analyze body)))
           ; END Question 20
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN Question 20
           (cons (list 'lambda (car (zip values)) (analyze (car body))) (map analyze (cadr (zip values))))
           ; END Question 20
           ))
        (else
         ; BEGIN Question 20
         (map analyze expr)
         ; END Question 20
         )))

;; Problem 21 (optional)
;; Draw the hax image using turtle graphics.
(define (hax d k)
  ; BEGIN Question 21
  'REPLACE-THIS-LINE
  )
  ; END Question 21
