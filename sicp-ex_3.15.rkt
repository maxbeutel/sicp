#lang sicp

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

;;;;;;;

(define x (list 'a 'b))

(define z1 (cons x x))

(define z2 (cons (list 'a 'b) (list 'a 'b)))

(set-to-wow! z1)

(set-to-wow! z2)

;;;;;;;

#|

Diagram for z1:

        x -> |a|*| -> |b|/|
       ^ ^
       | |
z1 -> |*|*|

Diagram for z2:

      |a|*| -> |b|/|
       ^
       |
z2 -> |*|*| -> |a|*| -> |b|/|

|#
