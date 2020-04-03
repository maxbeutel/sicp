(define (prime? n)
  (= (smallest-divisor n) n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n d)
  (if (> (square d) n)
      n
      (if (divides? d n)
          d
          (find-divisor n (+ d 1)))))

(define (square n)
  (* n n))

(define (divides? a b)
  (= (remainder b a) 0))

(divides? 5 25)
(divides? 4 25)

(find-divisor 40 2)
(find-divisor 17 2)
(find-divisor 561 2)

(prime? 40)
(prime? 17)
(prime? 561)
(prime? 7919)
