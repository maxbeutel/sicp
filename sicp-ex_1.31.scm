(define (cube n)
  (* n n n))

(define (inc n)
  (+ n 1))

(define (identity n)
  n)

;;;;;;;;;;

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1 (* a (+ a 2.0))) (pi-sum (+ a 4.0) b))))

;;;;;;;;;;

(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter cur a b)
    (if (> a b)
        cur
        (iter (* cur (term a)) (next a) b)))
  (iter 1 a b))

(define (integer-product a b)
  (product identity a inc b))

(define (integer-product-iter a b)
  (product-iter identity a inc b))

(define (cube-product a b)
  (product cube a inc b))

(define (cube-product-iter a b)
  (product-iter cube a inc b))

(define (pi-product a b)
  (define (term n)
    (if (= n 1)
        (/ 2.0 3.0)
        (* (/ (* 2 n) (- (* 2 n) 1))
           (/ (* 2 n) (+ (* 2 n) 1)))))
  (product term a inc b))

(define (pi-product-iter a b)
  (define (term n)
    (if (= n 1)
        (/ 2.0 3.0)
        (* (/ (* 2 n) (- (* 2 n) 1))
           (/ (* 2 n) (+ (* 2 n) 1)))))
  (product-iter term a inc b))

;;;;;;;;;;

(integer-product 1 5)

(cube-product 1 5)

(* 4 (pi-product 1 1000))

;;;;;;;;;;

(integer-product-iter 1 5)

(cube-product-iter 1 5)

(* 4 (pi-product-iter 1 1000))

;;(* 8 (pi-sum 1 1000))
