;;; Get visible-state.
;;; Assume a real-state not in tried-assumptions[visible-state].
;;; Add real-state to tried-assumptions[visible-state].
;;; Get solutions for real-state.
;;; If solutions is empty, try other real-state.
;;; Follow solution steps, check for wrong assumption at each step, try other
;;; real-state if assumption was wrong.

(define-module (taylan godwars-puzzle-solver)
  #:export (find-solutions-for-visible-state))

(use-modules (taylan godwars-puzzle-internal-solver))

(define low-values '(-3 -2 -1))
(define high-values '(1 2 3))

(define )

(define (make-assumption visible-state excluded-states)
  (map (lambda (elt)
         (case elt
           ('very-low -4)
           ('low ())))))
