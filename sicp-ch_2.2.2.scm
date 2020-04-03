(define (scale-tree tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree factor))
        (else (cons
               (scale-tree (car tree) factor)
               (scale-tree (cdr tree) factor)
               ))))

(scale-tree (list 1 2 3) 10)

;;;;;;;;;;;

(define (scale-tree-map tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree-map sub-tree factor)
             (* sub-tree factor)))
       tree))

(scale-tree-map (list 1 2 3) 10)
