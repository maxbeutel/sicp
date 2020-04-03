#lang racket

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree
        elements (length elements))))

;; Returns a pair
;; First element: constructed tree
;; Second element: list of elements not included in the tree
;;
;;
;; Splits the input list into three parts:
;; left side of tree
;; median element
;; right side of tree
;;
;; we create a tree from the left side, and from the right side
;; and create a node with the median element as the value
;;
;; Tree diagram for (list 1 3 5 7 9 11) as input
;;
;;             5
;;         /      \
;;         1       9
;;          \    /   \
;;           3  7    11
;;
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size
             (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree
                elts left-size)))
          (let ((left-tree
                 (car left-result))
                (non-left-elts
                 (cdr left-result))
                (right-size
                 (- n (+ left-size 1))))
            (let ((this-entry
                   (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree
                     (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

;;;;;;;;;;;

(list->tree (list 1 3 5 7 9 11))
