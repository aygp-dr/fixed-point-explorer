;;; y-combinator-test.scm --- Test different Y combinator implementations

(load "../src/portable/y-combinator.scm")

(displayln "=== Y Combinator Implementation Test ===")

;; Test single-argument function (factorial)
(define CONTINUE-fact
  (lambda (f)
    (lambda (n)
      (if (= n 0) 1 (* n (f (- n 1)))))))

;; Classic Y would cause infinite loop due to eager evaluation!
;; (define fact-classic (Y-classic CONTINUE-fact))

;; Working version with delayed evaluation
(define fact-explicit (Y-explicit CONTINUE-fact))

(displayln "\nTesting factorial with Y-explicit:")
(display "  fact(5) = ")
(displayln (fact-explicit 5))  ;; Should be 120

;; The issue with classic Y in eager languages
(displayln "\nExplaining the classic Y combinator issue:")
(displayln "  Y-classic = (lambda (f) ((lambda (x) (f (x x))) (lambda (x) (f (x x)))))")
(displayln "  This causes infinite recursion in eager evaluation!")
(displayln "  We need to delay the self-application with a lambda.")

;; Show the actual implementation
(displayln "\nOur Y-explicit implementation:")
(displayln "  Uses (lambda args (apply (x x) args)) to delay evaluation")
(displayln "  This prevents infinite recursion and handles multiple arguments")

;; Test with multi-argument function
(define CONTINUE-add
  (lambda (f)
    (lambda (a b)
      (if (= a 0) b (f (- a 1) (+ b 1))))))

(define add-y (Y-explicit CONTINUE-add))
(display "\nTesting multi-arg with Y-explicit: add(3, 4) = ")
(displayln (add-y 3 4))  ;; Should be 7