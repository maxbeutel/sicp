#lang racket

(require rackunit)

;; O(n)
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;; O(n)
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

;; O(n^2)
(define (union-set set1 set2)
  (if (null? set1)
      set2
      (adjoin-set (car set1) (union-set (cdr set1) set2))))

;; O(n^2)
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))


;;;;;

(check-equal? (intersection-set (list 1 2 3)
                                (list 3 4 5)) (list 3))

(check-equal? (union-set '()
                         '()) '())

(check-equal? (union-set '()
                         (list 1 2 3)) (list 1 2 3))

(check-equal? (union-set (list 1 2 3)
                         '()) (list 1 2 3))

(check-equal? (union-set (list 1 2 3)
                         (list 3 4 5)) (list 1 2 3 4 5))

(check-equal? (adjoin-set 1 (list 2 3)) (list 1 2 3))

(check-equal? (adjoin-set 2 (list 2 3)) (list 2 3))

(check-equal? (element-of-set? -1 (list 3 4 1 2)) false)

(check-equal? (element-of-set? 1 (list 3 4 1 2)) true)
