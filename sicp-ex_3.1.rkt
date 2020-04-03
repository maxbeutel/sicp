#lang racket

(define (make-accumulator initial)
  (lambda (amount)
    (set! initial (+ initial amount))
    initial))

;;;;;;;

(define A (make-accumulator 5))

(A 10)

(A 10)
