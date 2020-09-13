#lang sicp

;;;;;;;;;;;;;;;;

(define (print-list L)
  (cond ((null? L)
         (display "\n------\n"))
        (else
         (begin
           (display (car L))
           (display " ")
           (print-list (cdr L))))))

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (insert item)
      (let ((new-pair (cons item '())))
        (cond ((empty?)
               (begin
                 (set! front-ptr new-pair)
                 (set! rear-ptr new-pair)))
              (else
               (begin
                 (set-cdr! rear-ptr new-pair)
                 (set! rear-ptr new-pair))))))
    (define (empty?)
      (null? front-ptr))
    (define (delete)
      (set! front-ptr (cdr front-ptr)))
    (define (print)
      (print-list front-ptr))
    (define (dispatch m)
      (cond ((eq? m 'insert) insert)
            ((eq? m 'delete) delete)
            ((eq? m 'print) print)
            ((eq? m 'empty?) empty?)            
            (else (error "Unknown request" m))))
    dispatch))

(define q (make-queue))
((q 'empty?))
((q 'print))

((q 'insert) 'a)
((q 'print))

((q 'insert) 'b)
((q 'print))

((q 'delete))
((q 'print))

((q 'insert) 'c)
((q 'print))

((q 'insert) 'd)
((q 'print))

((q 'delete))
((q 'print))
