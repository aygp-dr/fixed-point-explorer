;;; test-all.scm --- Run all tests

(load "../test-framework.scm")
(load "../../src/portable/y-combinator.scm")
(load "../../src/portable/fibonacci.scm")
(load "../../src/portable/list-ops.scm")

(displayln "=== Fixed Point Explorer Test Suite ===")
(display "Implementation: ")
(displayln implementation-name)

(reset-test-stats!)

;; Test Y combinator basics
(displayln "\n1. Y Combinator Basic Tests")
(assert-equal 0 (fib 0) "fib(0) = 0")
(assert-equal 1 (fib 1) "fib(1) = 1")
(assert-equal 5 (fib 5) "fib(5) = 5")
(assert-equal 55 (fib 10) "fib(10) = 55")
(assert-equal 6765 (fib 20) "fib(20) = 6765")

;; Test list operations
(displayln "\n2. List Operation Tests")
(assert-equal '(1 2 3 4 5 6) 
              (append-y '(1 2 3) '(4 5 6))
              "append-y '(1 2 3) '(4 5 6)")
(assert-equal 5 
              (length-y '(a b c d e))
              "length-y '(a b c d e)")
(assert-equal '(2 4 6 8)
              (map-y (lambda (x) (* x 2)) '(1 2 3 4))
              "map-y double '(1 2 3 4)")
(assert-equal '(2 4 6)
              (filter-y even? '(1 2 3 4 5 6))
              "filter-y even? '(1 2 3 4 5 6)")
(assert-equal 15
              (foldr-y + 0 '(1 2 3 4 5))
              "foldr-y + 0 '(1 2 3 4 5)")

;; Test edge cases
(displayln "\n3. Edge Case Tests")
(assert-equal '() 
              (append-y '() '())
              "append-y empty lists")
(assert-equal 0
              (length-y '())
              "length-y empty list")
(assert-equal '()
              (map-y (lambda (x) x) '())
              "map-y identity empty")
(assert-equal '()
              (filter-y (lambda (x) #t) '())
              "filter-y always-true empty")

(if (test-summary)
    (begin
      (displayln "\n✓ All tests passed!")
      (exit 0))
    (begin
      (displayln "\n✗ Some tests failed!")
      (exit 1)))
