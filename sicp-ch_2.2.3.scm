(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(filter odd? (list 1 2 3 4 5))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5))

(accumulate * 1 (list 1 2 3 4 5))

(accumulate cons '() (list 1 2 3 4 5))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 2 7)

(define (enumerate-tree T)
  (cond ((null? T) '())
        ((not (pair? T)) (list T))
        (else (append (enumerate-tree (car T))
                      (enumerate-tree (cdr T))))))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(define (fib n)
  (fib-iter 1 0 n))

;;;;;;;

(define (sum-odd-squares T)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree T)))))

(sum-odd-squares (list 1 (list 2 (list 3 4)) 5))

(define (even-fibs n)
  (accumulate cons
              '()
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))

(even-fibs 20)
