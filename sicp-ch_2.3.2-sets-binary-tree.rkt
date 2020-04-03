#lang racket

(require rackunit)

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (make-tree-leaf entry)
  (list entry '() '()))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set)) (element-of-set? x (left-branch set)))
        ((> x (entry set)) (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set)) (make-tree (entry set)
                                      (adjoin-set x (left-branch set))
                                      (right-branch set)))
        ((> x (entry set)) (make-tree (entry set)                                      
                                      (left-branch set)
                                      (adjoin-set x (right-branch set))))))

;;;;;;;;;;;;;;;

(define T (make-tree 7  (make-tree-leaf 3) (make-tree-leaf 9)))

(check-equal? (adjoin-set 1 '()) (list 1 '() '()))

(check-equal? (adjoin-set 1 T) (make-tree 7 (make-tree 3 (make-tree-leaf 1) '()) (make-tree-leaf 9)))

(check-equal? (adjoin-set 10 T) (make-tree 7 (make-tree-leaf 3) (make-tree 9 '() (make-tree-leaf 10))))

(check-equal? (element-of-set? -1 T) false)

(check-equal? (element-of-set? 9 T) true)

