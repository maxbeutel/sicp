(define (cube x)
  (* x x x))

(define (average a b)
  (/ (+ a b) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;;;;;;;;;;

;; Approximate derivative of g
(define (deriv g)
  (define h 0.00001)
  (lambda (x)
    (/ (- (g (+ x h)) (g x))
       h)))

(define (newton-transform g)
  (lambda (x) (- x
                 (/ (g x) ((deriv g) x)))))


(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt-newton x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))


(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt-fixed x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

;;;;;;;;;;

(sqrt-newton 2)

(sqrt-fixed 2)
