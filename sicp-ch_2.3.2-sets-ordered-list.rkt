#lang racket

(require rackunit)

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= (car set) x) true)
        ((> (car set) x) false)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((> (car set) x) (cons x set))
        ((< (car set) x) (cons (car set) (adjoin-set x (cdr set))))
        (else set)))
        
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (x2 (car set2)))
                (cond ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                      ((< x2 x1) (cons x2 (union-set set1 (cdr set2))))
                      (else (cons x1 (union-set (cdr set1) (cdr set2)))))))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((< x1 x2) (intersection-set (cdr set1)
                                           set2))
              ((< x2 x1) (intersection-set set1
                                           (cdr set2)))
              (else (cons x1 (intersection-set (cdr set1)
                                               (cdr set2))))))))

;;;;;;;

(check-equal? (intersection-set (list 1 2 3)
                                (list 3 4 5)) (list 3))

(check-equal? (union-set '()
                         (list 1 2 3)) (list 1 2 3))

(check-equal? (union-set (list 1 2 3)
                         '()) (list 1 2 3))

(check-equal? (union-set (list 1 2 3 4)
                         (list 4 5 6 7)) (list 1 2 3 4 5 6 7))

(check-equal? (union-set (list 4 5 6 7)
                         (list 1 2 3 4)) (list 1 2 3 4 5 6 7))

(check-equal? (union-set (list 4 5)
                         (list 1 2 3 4)) (list 1 2 3 4 5))

(check-equal? (union-set (list 4 5 6 7)
                         (list 1 2)) (list 1 2 4 5 6 7))


(check-equal? (union-set (list 1 2 3 4 5)
                         (list 0 1 2 3 4 5)) (list 0 1 2 3 4 5))


(check-equal? (adjoin-set 4 (list 2 3)) (list 2 3 4))

(check-equal? (adjoin-set 1 (list 2 3)) (list 1 2 3))

(check-equal? (adjoin-set 2 (list 1 2 3)) (list 1 2 3))



(check-equal? (element-of-set? -1 (list 1 2 3 4)) false)

(check-equal? (element-of-set? 1 (list 1 2 3 4)) true)

(check-equal? (element-of-set? 1 '()) false)
