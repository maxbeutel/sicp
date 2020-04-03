#lang racket

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define T1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define T2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define T3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

;; Create a list of the left branch of the current node,
;; then append a list whose head is value of current node,
;; followed by the right branch of current node.
;; This produces a list of all values in the tree, in ascending order
;; We use append here, the list to which we append halves in size for each
;; level of the tree. Overall we need to append n times, then total complexity
;; is O(n*log n)
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

;; Idea: avoid append
;; Create a ordered list of the right branch
;; then prepend all nodes in the left branch to it by using (cons <current element> <right elements>)
;; Will produce a list of tree elements in-order, and avoid overhead of (append)
;; Complexity: O(n), as we can assume cons is O(1)
(define (tree->list-2 tree)
  (define (add-list T L)
    (if (null? T)
        L
        (add-list
         (left-branch T)
         (cons (entry T)
              (add-list (right-branch T) L)))))
  (add-list tree '()))

;;;;;;;;;;;;;;;;;

;; Both functions traverse tree in-order
;; (tree->list-1 T1)
;; (tree->list-1 T2)
;; (tree->list-1 T3)

;;;(tree->list-2 T2)
(tree->list-2 T1)
(tree->list-2 T2)
(tree->list-2 T3)
