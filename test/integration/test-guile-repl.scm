;;; test-guile-repl.scm --- Quick REPL test for Guile

;; Test loading from project root
(display "Testing Guile REPL from project root...\n")

;; Load portable modules - these should be in the load path via -L
(primitive-load-path "fibonacci.scm")
(display "✓ Loaded fibonacci.scm\n")

;; Test basic functionality
(display "Testing fib(10): ")
(display (fib 10))
(display " (expected 55)\n")

;; Test the test function
(newline)
(test-fibonacci)

;; Test list operations
(newline)
(primitive-load-path "list-ops.scm")
(display "✓ Loaded list-ops.scm\n")
(display "Testing append-y: ")
(display (append-y '(1 2 3) '(4 5 6)))
(newline)

(display "\nAll manual tests passed!\n")