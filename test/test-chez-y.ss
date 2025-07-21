;;; test-chez-y.ss --- Test Y combinator implementations in Chez Scheme

;; Load from correct paths
(load "../src/portable/y-combinator.scm")

(displayln "=== Chez Scheme Y Combinator Test ===")
(displayln "Chez Scheme has better optimization capabilities than Guile")

;; Test our portable Y-explicit
(define CONTINUE-fact
  (lambda (f)
    (lambda (n)
      (if (= n 0) 1 (* n (f (- n 1)))))))

(define fact (Y-explicit CONTINUE-fact))

(displayln "\nTesting Y-explicit in Chez:")
(display "  fact(5) = ") (displayln (fact 5))
(display "  fact(10) = ") (displayln (fact 10))

;; Chez-optimized Y using case-lambda
(define Y-chez
  (lambda (f)
    (let ((g (case-lambda
               (() (error 'Y-chez "no arguments"))
               ((x) ((f (lambda (a) ((g g) a))) x))
               ((x y) ((f (lambda (a b) ((g g) a b))) x y))
               ((x y . z) (apply (f (lambda args (apply (g g) args))) x y z)))))
      (g g))))

(displayln "\nChez-specific Y combinator advantages:")
(displayln "  - Uses case-lambda for efficient dispatch")
(displayln "  - Avoids apply overhead for common arities")
(displayln "  - Still handles variable arguments via rest args")

(define fact-chez (Y-chez CONTINUE-fact))
(display "\nTesting Y-chez: fact(10) = ")
(displayln (fact-chez 10))

;; Simple timing test
(displayln "\nTesting with larger values:")
(display "  fact(20) = ")
(displayln (fact 20))
(display "  fact-chez(20) = ")
(displayln (fact-chez 20))

(displayln "\nConclusion: Both avoid infinite recursion through delayed evaluation")