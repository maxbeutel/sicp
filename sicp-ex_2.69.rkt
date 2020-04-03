#lang racket

(require rackunit)

;; Leaves
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

;; Tree

(define (make-code-tree left right)
  (list left right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set
         (make-leaf (car pair)
                    (cadr pair))
         (make-leaf-set (cdr pairs))))))

;;;;;;;;;;;;

#|

Note that:
    (make-leaf-set (list '(X 10) '(A 8) '(Y 100) '(B 3)))
returns an ordered list, with the lowest first
   '((leaf B 3) (leaf A 8) (leaf X 10) (leaf Y 100))

|#

(define pairs (list '(A 8)
                    '(B 3)
                    '(C 1)
                    '(D 1)
                    '(E 1)
                    '(F 1)
                    '(G 1)
                    '(H 1)))

(define (successive-merge pairs)
  (if (= (length pairs) 1)
      (car pairs)
      (successive-merge (adjoin-set (make-code-tree (car pairs) (cadr pairs))
                                    (cddr pairs)))))

(define (generate-huffman-tree pairs)
  (successive-merge
   (make-leaf-set pairs)))

;; (make-leaf-set pairs)

(generate-huffman-tree pairs)
