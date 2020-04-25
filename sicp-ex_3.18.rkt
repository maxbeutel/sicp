#lang sicp

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (contains-cycle? x)
  (let ((encountered '()))
    (define (helper x)
      (cond
        ((memq x encountered) #t)
        ((not (pair? x)) #f)
        (else (begin
                (set! encountered (cons x encountered))
                (or (helper (car x)) (helper (cdr x)))
                ))))
    (helper x)))

;;;;;;;;

(contains-cycle? '(1 2 3 4))

(define y (list 'a 'b 'c 'v))

(define z (make-cycle y))

(contains-cycle? z)

(define x1 '(b))
(define x2 '(a))
(define x (cons x1 x2))
(set-cdr! x2 x)

(contains-cycle? x)

(define u '(1 2 3 4 5 6 7 8))
(define v '(1 2 3 4 5 6 7 8))
(set-cdr! (cdddr (cddddr v)) (cdddr v))

(contains-cycle? v)
