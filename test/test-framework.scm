;;; test-framework.scm --- Simple test framework

(define *test-count* 0)
(define *pass-count* 0)
(define *fail-count* 0)

(define (reset-test-stats!)
  (set! *test-count* 0)
  (set! *pass-count* 0)
  (set! *fail-count* 0))

(define (assert-equal expected actual desc)
  (set! *test-count* (+ *test-count* 1))
  (if (equal? expected actual)
      (begin
        (set! *pass-count* (+ *pass-count* 1))
        (display "  ✓ ")
        (displayln desc))
      (begin
        (set! *fail-count* (+ *fail-count* 1))
        (display "  ✗ ")
        (displayln desc)
        (display "    Expected: ")
        (displayln expected)
        (display "    Actual:   ")
        (displayln actual))))

(define (test-summary)
  (newline)
  (display "Test Summary: ")
  (display *test-count*)
  (display " tests, ")
  (display *pass-count*)
  (display " passed, ")
  (display *fail-count*)
  (displayln " failed")
  (= *fail-count* 0))
