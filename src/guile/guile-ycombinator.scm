;;; guile-ycombinator.scm --- Guile-specific Y combinator extensions

(use-modules (ice-9 format)
             (ice-9 time)
             (srfi srfi-1))

(load "../portable/y-combinator.scm")
(load "../portable/fibonacci.scm")
(load "../portable/list-ops.scm")

;; Guile-specific: timed evaluation
(define-macro (time-it expr)
  `(let ((start (get-internal-real-time)))
     (let ((result ,expr))
       (let ((elapsed (/ (- (get-internal-real-time) start) 
                        internal-time-units-per-second)))
         (format #t "Time: ~,3f seconds~%" elapsed)
         result))))

;; Performance test
(define guile-performance-test
  (lambda ()
    (displayln "\nGuile Performance Test:")
    (display "  fib(30) = ")
    (time-it (displayln (fib 30)))
    (let ((memo-fib (make-memoized-fib)))
      (display "  memo-fib(30) = ")
      (time-it (displayln (memo-fib 30))))))
