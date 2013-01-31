;;; Prologue
;;
;; min ∈ ℤ
;; max ∈ ℤ
;; length ∈ ℕ
;;
;; States = { n | min ≤ n ≤ max }^length
;;
;; Operations = { f : S → States, f : s ↦ s + t | S ⊂ States, t ∈ ℤ^length }
;;
;;
;; Goal: Implement an algorithm taking min, max, length, a starting state, a
;; goal state, and a subset of Operations as input, and yields the set of tuples
;; of operations whose composition map the starting state to the goal state.
;;
;;
;; The domain of an operation is only a subset of States because the application
;; might result in a tuple whose elements break the min/max limits. We call this
;; situation a "jam".
;;
;;
;; The solution is implemented with a breadth-first graph-search. Thanks to
;; `thestinger' on Freenode (`strcat' on OFTC).
;;
;; We save the accumulating operation-sequence for each path alongside the
;; states it pushes to the queue. The `state-progress' record type is used for
;; this, whose name is so due to the lack of a better one.
;;
;; The length value is not really used by the program. It would only really be
;; useful for assertions.

(define-module (taylan godwars-puzzle-internal-solver)
  #:export (find-solutions))

(use-modules (taylan godwars-puzzle-utils)
             (srfi srfi-9))

(state-min 1)
(state-max 9)
(state-length 5)

(define-record-type :state-progress
  (make-state-progress state operation-sequence)
  state-progress?
  (state state-progress-state)
  (operation-sequence state-progress-operation-sequence))

(define (find-solutions start-state goal-state operations)
  (let ((state-progress-queue (make-queue (make-state-progress start-state (make-sequence))))
        (visited-states (make-set))
        (solutions (make-sequence)))
    (let loop ()
      (if (queue-empty? state-progress-queue)
          solutions
          (let* ((state-progress (queue-pop! state-progress-queue))
                 (state (state-progress-state state-progress))
                 (operation-sequence (state-progress-operation-sequence state-progress)))
            (unless (set-element? visited-states state)
              (set-add! visited-states state)
              (for-each
               (lambda (operation)
                 (catch 'jam
                   (lambda ()
                     (let ((next-state (apply-operation state operation))
                           (operation-sequence (sequence-append operation-sequence
                                                                operation))) 
                       (unless (set-element? visited-states next-state)
                         (if (state-equal? goal-state next-state)
                             (sequence-append! solutions operation-sequence)
                             (queue-push! state-progress-queue
                                          (make-state-progress next-state
                                                               operation-sequence))))))
                   (lambda (exception-key)
                     #t)))
               operations))
            (loop))))))
