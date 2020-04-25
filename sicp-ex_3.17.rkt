#lang sicp

(define (count-pairs x)
  (let ((encountered '()))
    (define (helper x)
      (cond ((or (not (pair? x)) (memq x encountered)) 0)
            (else (begin
                    (set! encountered (cons x encountered))
                    (+ (helper (car x))
                       (helper (cdr x))
                       1)))))
    (helper x)))

;;
(define u '(a b c))

(count-pairs u)

;;
(define v1 '(a))
(define v2 '(b))
(define v (cons v1 v2))
(set-cdr! v1 v2)

(count-pairs v)

;;
(define w1 '(a))
(define w2  (cons w1 w1))
(define w (cons w2 w2))

(count-pairs w)

;;
(define x1 '(b))
(define x2 '(a))
(define x (cons x1 x2))
(set-cdr! x2 x)

(count-pairs x)
