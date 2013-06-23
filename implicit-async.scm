(define-module (taylan implicit-async)
  :use-module (ice-9 q)                 ;Queue
  :use-module (ice-9 threads)
  :export (run-parallel define-blocker flush-parallels))

(define q (make-q))
(define pending 0)

(define mutex (make-mutex))
(define condvar (make-condition-variable))

(define prompt-tag (make-prompt-tag))

(define-syntax run-parallel
  (syntax-rules ()
    ((run-parallel body body* ...)
     (call-with-prompt
      prompt-tag
      (lambda () body body* ...)
      (lambda (k blocker args)
        (set! pending (+ 1 pending))
        (begin-thread
         (call-with-values (lambda () (apply blocker args))
           (lambda values
             (with-mutex
              mutex
              (enq! q (lambda () (apply k values)))
              (signal-condition-variable condvar)))))
        *unspecified*)))))

(define-syntax define-blocker
  (syntax-rules ()
    ((define-blocker blocker)
     (let ((original blocker))
       (set! blocker (lambda args (abort-to-prompt prompt-tag original args)))))))

(define (flush-parallels)
  (while (> pending 0)
    (with-mutex
     mutex
     (while (q-empty? q)
       (wait-condition-variable condvar mutex))
     ((deq! q)))
    (set! pending (+ -1 pending)))
  *unspecified*)
