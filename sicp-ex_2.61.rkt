#lang racket

(require rackunit)

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((> (car set) x) (cons x set))
        ((< (car set) x) (cons (car set) (adjoin-set x (cdr set))))
        (else set)))

;;;;;;;
(check-equal? (adjoin-set 4 (list 2 3)) (list 2 3 4))

(check-equal? (adjoin-set 1 (list 2 3)) (list 1 2 3))

(check-equal? (adjoin-set 2 (list 1 2 3)) (list 1 2 3))


