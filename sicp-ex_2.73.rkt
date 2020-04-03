#lang racket

;; Global lookup table for operations.
;; We can assume the language provides these primitives.

(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) #f)) ;; or maybe '() instead of #f

;;;;;;;;;;;;;;;;;;;;;;

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp)
           (if (same-variable? exp var)
               1
               0))
         (else ((get 'deriv (operator exp))
                (operands exp)
                var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;;;;;;;;;;;;;;;;;;;;;;


;; a) We look up the operations based on the operation 'deriv and the given operator
;; We can't use this approach for boolean functions number? or variable? because it's not just a simple lookup
;; in a table, we need to test the input variable to see if it's a number or a variable.

;; b) Sum/Product


(define (install)
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))

  (define (addend s) (car s))
  (define (augend s) (cadr s))

  (define (sum exp var)
    (make-sum (deriv (addend exp) var) (deriv (augend exp) var)))

  (put 'deriv '+ sum)

  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))

  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))

  (define (product exp var)
    (make-sum
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp))))

  (put 'deriv '* product))

(install)

(deriv '(+ x 3) 'x)
(deriv '(+ x x) 'x)

(deriv '(* x y) 'x)
