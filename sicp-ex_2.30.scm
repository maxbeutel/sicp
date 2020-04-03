(define (square n)
  (* n n))

(define (square-tree T)
  (cond
   ((null? T) T)
   ((not (pair? T)) (square T))
   (else (cons
          (square-tree (car T))
          (square-tree (cdr T))))))

(define (square-tree-map T)
  (map (lambda (sub-T)
         (if (list? sub-T)
             (square-tree-map sub-T)
             (square sub-T)))
       T))

;;;;;;;;;;;;;;;;;

;; (square-tree
;;  (list 1
;;        (list 2 (list 3 4) 5)
;;        (list 6 7)))

(square-tree-map
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

;; Expected result:
;; (1 (4 (9 16) 25) (36 49))
