(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
       '()
       (cons (accumulate op init (map car seqs))
             (accumulate-n op init (map cdr seqs)))))

;;;;;;;;;;;;;;

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (accumulate + 0 (map * row v))) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols-of-n (transpose n)))
    (map (lambda (row-of-m)
           (matrix-*-vector cols-of-n row-of-m) ;;;; @TODO ????
           ) m)))

;;;;;;;;;;;;;;

;; (define m
;;   (list (list 1 2 3 4)
;;         (list 4 5 6 6)
;;         (list 6 7 8 9)))

;; (define v (list 1 2 3 4))

(define m (list (list 1 2)
                (list 3 4)))

(define n (list (list 5 6)
                (list 7 8)))

(define v (list 5 6))

;;;;;;;;;;;;;;

(dot-product v v)

(matrix-*-vector m v)

(transpose m)

(matrix-*-matrix m n)
