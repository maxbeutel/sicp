(define (f n)
  (if (< n 3) n (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(define (g n)
  (g-iter 2 1 0 n))

(define (g-iter a b c n)
    (if (= n 0)
      c
      (g-iter (+ a (* 2 b) (* 3 c)) a b (- n 1))))

(f 0)
(f 1)
(f 2)
(f 3)
(f 4)
(f 5)

(g 0)
(g 1)
(g 2)
(g 3)
(g 4)
(g 5)
