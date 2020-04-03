(define (pascal n k)
  (if (or (= n k) (= k 0))
      1
      (+ (pascal (- n 1) k) (pascal (- n 1) (- k 1))))
  )

(pascal 5 0)

(pascal 5 1)

(pascal 5 2)

(pascal 5 3)

(pascal 5 4)

(pascal 5 5)
