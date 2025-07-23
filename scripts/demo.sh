#!/bin/sh
# demo.sh - Run Y Combinator demonstrations

# Get script directory and project root
SCRIPT_DIR=$(dirname "$0")
PROJECT_ROOT=$(cd "$SCRIPT_DIR/.." && pwd)

# Run demo from project root
cd "$PROJECT_ROOT"

exec guile3 -L src/portable -c '
(display "\n=== Fixed Point Explorer - Y Combinator Demo ===\n\n")

;; Load and demonstrate Fibonacci
(primitive-load-path "fibonacci.scm")
(display "Fibonacci using Y combinator:\n")
(display "  fib(0) = ") (display (fib 0)) (newline)
(display "  fib(1) = ") (display (fib 1)) (newline)  
(display "  fib(5) = ") (display (fib 5)) (newline)
(display "  fib(10) = ") (display (fib 10)) (newline)
(display "  fib(20) = ") (display (fib 20)) (newline)

(newline)

;; Load and demonstrate list operations
(primitive-load-path "list-ops.scm")
(display "List operations using Y combinator:\n")
(display "  append-y '"'"'(1 2 3) '"'"'(4 5 6) = ")
(display (append-y '"'"'(1 2 3) '"'"'(4 5 6))) (newline)

(display "  length-y '"'"'(a b c d e) = ")
(display (length-y '"'"'(a b c d e))) (newline)

(display "  map-y (λx.x*2) '"'"'(1 2 3 4) = ")
(display (map-y (lambda (x) (* x 2)) '"'"'(1 2 3 4))) (newline)

(display "  filter-y even? '"'"'(1 2 3 4 5 6) = ")
(display (filter-y even? '"'"'(1 2 3 4 5 6))) (newline)

(display "\n✓ All demonstrations complete!\n")
(display "\nFor interactive exploration: gmake repl-guile\n")
'