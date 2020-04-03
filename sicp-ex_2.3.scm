(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;;;;;;;;;;;

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))


(define (make-segment x1 y1 x2 y2)
  (cons
   (make-point x1 y1)
   (make-point x2 y2)))

(define (start-segment l)
  (car l))

(define (end-segment l)
  (cdr l))

(define (print-segment l)
  (newline)
  (display "from")
  (print-point (start-segment l))
  (display " to ")
  (print-point (end-segment l)))

;;;;;;;;;;;

(print-point (make-point 1 2))

(print-segment (make-segment 0 0 5 5))
