#lang racket

(define (make-monitored f)
  (let ((count 0))
    (lambda (arg)
      (if (eq? arg 'how-many-calls?)
          count
          (begin
            (set! count (+ count 1))
            (f arg))))))

;;;;;;;;;;;;;

(define s (make-monitored sqrt))

(s 100)

(s 'how-many-calls?)

(s 100)

(s 'how-many-calls?)
