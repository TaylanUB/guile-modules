(define-module (taylan godwars puzzle-utils)
  #:export (state-min
            state-max
            state-length
            
            make-queue
            queue-empty?
            queue-push!
            queue-pop!

            make-sequence
            sequence-append
            sequence-append!

            make-set
            set-empty?
            set-add!
            set-element?

            make-state
            state-equal?

            make-operation
            apply-operation))

(use-modules (ice-9 q)
             (srfi srfi-1))

(define state-min (make-parameter 0))
(define state-max (make-parameter 0))
(define state-length (make-parameter 0))

(define (make-queue . elements)
  (let ((q (make-q)))
    (for-each (lambda (e) (enq! q e)) elements)
    q))
(define queue-empty? q-empty?)
(define (queue-push! queue . elements)
  (unless (null? elements)
    (enq! queue (car elements))
    (apply queue-push! queue (cdr elements))))
(define queue-pop! q-pop!)

(define (make-set . elements)
  (if (null? elements)
      (cons '() #f)
      (apply list elements)))
(define (set-empty? set)
  (not (cdr set)))
(define (set-add! set . elements)
  (unless (null? elements)
    (if (set-empty? set)
        (begin
          (set-car! set (car elements))
          (set-cdr! set (cdr elements)))
        (begin
          (set-cdr! set (cons (car set) (cdr set)))
          (set-car! set (car elements))
          (apply set-add! set (cdr elements))))))
(define (set-element? set element)
  (and
   (not (set-empty? set))
   (if (equal? element (car set))
       #t
       (let ((rest (cdr set)))
         (and (not (null? rest))
              (set-element? rest element))))))

(define (make-sequence . elements)
  (if (null? elements)
      (cons '() #f)
      (apply list elements)))
(define (sequence-empty? sequence)
  (not (cdr sequence)))
(define (sequence-append sequence . elements)
  (if (sequence-empty? sequence)
      (apply make-sequence elements)
      (append sequence elements)))
(define (sequence-append! sequence . elements)
  (unless (null? elements)
    (if (sequence-empty? sequence)
        (begin
          (set-car! sequence (car elements))
          (set-cdr! sequence (cdr elements)))
        (append! sequence elements))))

(define make-state list)
(define (state-equal? state1 state2 . states)
  (apply every = state1 state2 states))

(define make-operation list)
(define (apply-operation state operation)
  (let ((result (map + state operation)))
    (for-each
     (lambda (element)
       (if (not (<= (state-min) element (state-max)))
           (throw 'jam)))
     result)
    result))
