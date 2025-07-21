;;; fibonacci.scm --- Fibonacci using Y combinator

(load "y-combinator.scm")

;; CONTINUE function for Fibonacci
(define CONTINUE-fib
  (lambda (f)
    (lambda (n)
      (if (<= n 1)
          n
          (+ (f (- n 1)) (f (- n 2)))))))

;; Create fibonacci function
(define fib (Y-explicit CONTINUE-fib))

;; Memoized version for performance (Guile-specific)
(define make-memoized-fib
  (lambda ()
    ;; Simple alist-based memoization for portability
    (let ((cache '()))
      (define CONTINUE-memo-fib
        (lambda (f)
          (lambda (n)
            (let ((cached (assoc n cache)))
              (if cached
                  (cdr cached)
                  (let ((result (if (<= n 1)
                                    n
                                    (+ (f (- n 1)) (f (- n 2))))))
                    (set! cache (cons (cons n result) cache))
                    result))))))
      (Y-explicit CONTINUE-memo-fib))))

;; Test function
(define test-fibonacci
  (lambda ()
    (displayln "Testing Fibonacci:")
    (display "  fib(0) = ") (display (fib 0)) (displayln " (expected: 0)")
    (display "  fib(1) = ") (display (fib 1)) (displayln " (expected: 1)")
    (display "  fib(5) = ") (display (fib 5)) (displayln " (expected: 5)")
    (display "  fib(10) = ") (display (fib 10)) (displayln " (expected: 55)")
    (display "  Implementation: ") (displayln implementation-name)))
