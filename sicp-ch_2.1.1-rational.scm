(define (gcd a b)
  (if
   (= b 0)
   a
   (gcd b (remainder a b))))

;;;;;;;;;;

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;;;;;;;;;;

(define (make-rat n d)
  (let ((g (abs (gcd n d))))
    (cond ((= d 0) (error "Division by 0"))
          ((and (> n 0) (> d 0)) (cons (/ n g) (/ d g)))
          ((and (< n 0) (< d 0)) (cons (abs (/ n g)) (abs (/ d g))))
          ((and (< n 0) (> d 0)) (cons (/ n g) (/ d g)))
          ((and (> n 0) (< d 0)) (cons (* -1 (/ n g)) (abs (/ d g))))
          (else (cons 0 0)))))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;;;;;;;;;;

;; (print-rat (make-rat 1 3))

;; (print-rat (make-rat -1 -3))

;; (print-rat (make-rat -1 3))

;; (print-rat (make-rat 1 -3))

;; (print-rat (make-rat 0 3))

(define one-half (make-rat 1 2))

(print-rat one-half)

(define one-third (make-rat 1 3))

(print-rat one-third)

(print-rat (add-rat one-half one-third))

(print-rat (mul-rat one-half one-third))

(print-rat (add-rat one-third one-third))
