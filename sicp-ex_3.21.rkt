#lang sicp

(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))

(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "Queue is empty")
      (car (front-ptr queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue) (error "Queue is empty"))
        (else (set-front-ptr! queue (cdr (front-ptr queue))) queue)))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

;;;;;;;;;;;;;;;;

(define (print-list L)
  (cond ((null? L)
         (display "------\n"))
        (else
         (begin
           (display (car L))
           (display "\n")
           (print-list (cdr L))))))

(define (print-queue queue)
  (display "> Queue contents:\n")
  (cond ((empty-queue? queue) (display "[]\n"))
        (else (print-list (front-ptr queue)))))

(define q1 (make-queue))
(print-queue q1)

(insert-queue! q1 'a)
(print-queue q1)

(insert-queue! q1 'b)
(print-queue q1)

(delete-queue! q1)
(print-queue q1)

(delete-queue! q1)
(print-queue q1)