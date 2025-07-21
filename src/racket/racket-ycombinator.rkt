#lang racket
;;; racket-ycombinator.rkt --- Racket-specific Y combinator

(require racket/trace)

;; Load portable implementations
(load "../portable/y-combinator.scm")
(load "../portable/fibonacci.scm")
(load "../portable/list-ops.scm")

;; Racket-specific: Typed Y combinator
(require typed/racket)

(: Y-typed (All (a b) ((a -> b) -> (a -> b)) -> (a -> b)))
(define (Y-typed f)
  ((λ ([x : (Rec X (X -> (a -> b)))])
     (f (λ (y) ((x x) y))))
   (λ ([x : (Rec X (X -> (a -> b)))])
     (f (λ (y) ((x x) y))))))

;; Contracts for safety
(define/contract (safe-fib n)
  (-> exact-nonnegative-integer? exact-nonnegative-integer?)
  (fib n))

;; Traced version for debugging
(define traced-fib
  (let ()
    (trace CONTINUE-fib)
    (Y-explicit CONTINUE-fib)))

(define (test-racket-features)
  (displayln "\nRacket-specific features:")
  (displayln "  Testing contracts...")
  (displayln (format "  safe-fib(10) = ~a" (safe-fib 10)))
  (displayln "  Testing traced version (first 5 only)...")
  (traced-fib 5))
