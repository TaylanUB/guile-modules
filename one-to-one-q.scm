;;; one-to-one-q.scm --- LIMITED lock-free thread-safe queue.

;; Copyright (C) 2013  Taylan Ulrich B.

;; Author: Taylan Ulrich B. <taylanbayirli@gmail.com>
;; Keywords: thread safe queue

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a queue that is thread-safe *IF* there is only one enqueuing and one
;; dequeuing thread.

;; Basic idea: the queue is essentially a (boxed) linked list, but we have a
;; special mutable null object that can be "denulled" into a normal node with a
;; fresh null object at its tail, so no matter how many objects the dequeue
;; procedure removes from the front (by setting the box's pointer to subsequent
;; nodes), the enqueuing thread can always reach the null object at the end and
;; mutate it into a node.  (It only needs to be assured that this mutation
;; happens atomically from the perspective of the dequeuing thread; this is
;; achieved via a conceptual is-null tag which is removed only after setting up
;; the contents of the node.)

;; The list of a queue can be extracted via `q-list', and nullity should be
;; checked with `qnull?' and not `null?'.


;;; Code:

(define-module (taylan one-to-one-q)
  #:export (make-q q-empty? q-enq! q-deq! q-list qnull?))

(define (make-qnull)
  (cons #f #f))

(define (qnull? q)
  (not (cdr q)))

(define (qnull-denull qnull value)
  (set-car! qnull value)
  (set-cdr! qnull (make-qnull)))

(define (make-q)
  (make-variable (make-qnull)))

(define (q-empty? q)
  (qnull? (variable-ref q)))

(define (q-list q)
  (variable-ref q))

(define (q-enq! q value)
  (let seek-qnull ((obj (variable-ref q)))
    (if (qnull? obj)
        (qnull-denull obj value)
        (seek-qnull (cdr obj)))))

(define (q-deq! q)
  (let ((content (variable-ref q)))
    (if (qnull? content)
        (error "Cannot dequeue empty queue.")
        (let ((value (car content))
              (rest (cdr content)))
          (variable-set! q rest)
          value))))

;;; one-to-one-q.scm ends here
