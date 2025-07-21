#!/usr/bin/env guile
!#
;;; example-srfi64-test.scm - Example of SRFI-64 testing in Guile
;;;
;;; This file demonstrates various SRFI-64 testing features including:
;;; - Basic assertions
;;; - Test groups
;;; - Error testing
;;; - Approximate comparisons
;;; - Custom test runners

(use-modules (srfi srfi-64))

;;; Example module to test
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(define (divide a b)
  (if (zero? b)
      (error "Division by zero")
      (/ a b)))

(define (approximate-pi)
  3.14159265)

;;; Begin test suite
(test-begin "example-test-suite")

;;; Basic equality tests
(test-equal "factorial of 0" 1 (factorial 0))
(test-equal "factorial of 5" 120 (factorial 5))
(test-equal "string comparison" "hello" (string-append "hel" "lo"))

;;; Boolean assertions
(test-assert "positive number" (> 5 0))
(test-assert "list not empty" (not (null? '(1 2 3))))

;;; Test groups for organization
(test-group "arithmetic operations"
  (test-equal "addition" 10 (+ 5 5))
  (test-equal "subtraction" 5 (- 10 5))
  (test-equal "multiplication" 20 (* 4 5))
  (test-equal "division" 4 (/ 20 5)))

;;; Error testing
(test-error "division by zero throws error" 
            (divide 10 0))

;;; Approximate equality (useful for floating point)
(test-approximate "pi approximation" 
                  3.14159 
                  (approximate-pi) 
                  0.00001)

;;; Nested test groups
(test-group "list operations"
  (test-group "basic list functions"
    (test-equal "length of list" 3 (length '(a b c)))
    (test-equal "car of list" 'first (car '(first second third)))
    (test-equal "cdr of list" '(2 3) (cdr '(1 2 3))))
  
  (test-group "list predicates"
    (test-assert "null? on empty list" (null? '()))
    (test-assert "pair? on cons" (pair? (cons 1 2)))
    (test-assert "list? on proper list" (list? '(1 2 3)))))

;;; Conditional test execution
(test-skip (if (not (defined? 'some-optional-feature)) 1 0))
(test-assert "optional feature test" 
             (and (defined? 'some-optional-feature)
                  (some-optional-feature)))

;;; Custom test with detailed info
(test-equal "factorial with info"
            (begin 
              (display "Computing factorial of 10...\n")
              3628800)
            (factorial 10))

;;; End test suite
(test-end "example-test-suite")

;;; Run tests and exit with appropriate code
(exit (if (zero? (test-runner-fail-count (test-runner-current))) 0 1))