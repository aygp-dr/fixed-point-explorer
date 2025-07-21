;;; chez-ycombinator.ss --- Chez Scheme specific extensions

(load "../portable/y-combinator.scm")
(load "../portable/fibonacci.scm")
(load "../portable/list-ops.scm")

;; Chez-specific: Define format if not available
(define format
  (lambda (port fmt . args)
    (if port
        (apply printf fmt args)
        (apply sprintf fmt args))))

;; Chez-specific optimized Y combinator using case-lambda
(define Y-chez
  (lambda (f)
    (let ((g (case-lambda
               (() (error 'Y-chez "no arguments"))
               ((x) ((f (lambda (a) ((g g) a))) x))
               ((x y) ((f (lambda (a b) ((g g) a b))) x y))
               ((x y . z) (apply (f (lambda args (apply (g g) args))) x y z)))))
      (g g))))

;; Test Chez-specific version
(define test-chez-y
  (lambda ()
    (displayln "\nChez-specific Y combinator test:")
    (let ((fib-chez (Y-chez CONTINUE-fib)))
      (displayln (format #f "  fib-chez(10) = ~a" (fib-chez 10))))))
