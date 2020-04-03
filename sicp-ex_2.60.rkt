#lang racket

;; O(n)
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;; O(1)
(define (adjoin-set x set)
  (cons x set))

;; O(n^2)
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))        
        (else
         (intersection-set (cdr set1) set2))))

;; O(n)
(define (union-set set1 set2)
  (append set1 set2)) 
        
;;;;;;;;;;;;;;;;;;;

(intersection-set (list 1 2 7 8 9) (list 1 2 3 4 5))

(union-set (list 1 2) (list 1 3 4 5))
