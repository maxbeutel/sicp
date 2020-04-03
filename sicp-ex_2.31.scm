(define (square n)
  (* n n))

(define (tree-map f T)
  (map (lambda (sub-T)
         (if (list? sub-T)
             (tree-map f sub-T)
             (f sub-T)))
       T))

(define (square-tree T)
  (tree-map square T))

;;;;;;;;;;;;;;

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

;; Expected result:
;; (1 (4 (9 16) 25) (36 49))
