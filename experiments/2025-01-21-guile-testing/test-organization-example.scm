#!/usr/bin/env guile
!#
;;; test-organization-example.scm - Example of organizing tests in a Guile project
;;;
;;; This demonstrates best practices for organizing test files and test suites
;;; in a real project structure.

(use-modules (srfi srfi-64)
             (srfi srfi-1)   ; List utilities
             (ice-9 ftw))    ; File tree walk

;;; Test runner configuration
(define (setup-test-runner)
  "Configure test runner with custom settings"
  (let ((runner (test-runner-simple)))
    ;; Customize the runner if needed
    (test-runner-current runner)))

;;; Test helper functions
(define (with-test-directory thunk)
  "Run tests in a temporary directory"
  (let ((original-dir (getcwd))
        (test-dir (mkdtemp "/tmp/test-XXXXXX")))
    (dynamic-wind
      (lambda () (chdir test-dir))
      thunk
      (lambda () 
        (chdir original-dir)
        (system* "rm" "-rf" test-dir)))))

(define (load-test-files pattern)
  "Load all test files matching pattern"
  (let ((test-files (find-files "tests" pattern)))
    (for-each load test-files)))

(define (find-files dir pattern)
  "Find all files in dir matching pattern"
  (let ((files '()))
    (ftw dir
         (lambda (filename statinfo flag)
           (when (and (eq? flag 'regular)
                      (string-match pattern filename))
             (set! files (cons filename files)))
           #t))
    (reverse files)))

;;; Main test suite structure
(define (run-all-tests)
  "Run all test suites in the project"
  (setup-test-runner)
  
  ;; Start main test suite
  (test-begin "project-test-suite")
  
  ;; Run unit tests
  (test-group "unit-tests"
    (run-unit-tests))
  
  ;; Run integration tests
  (test-group "integration-tests"
    (run-integration-tests))
  
  ;; Run performance tests (optional)
  (when (getenv "RUN_PERF_TESTS")
    (test-group "performance-tests"
      (run-performance-tests)))
  
  (test-end "project-test-suite")
  
  ;; Return exit code based on test results
  (exit (if (zero? (test-runner-fail-count (test-runner-current))) 0 1)))

;;; Unit test organization
(define (run-unit-tests)
  "Run all unit tests"
  
  ;; Test data structures
  (test-group "data-structures"
    (test-group "list-operations"
      (test-equal "empty list" '() (make-list 0))
      (test-equal "list of 3" '(a a a) (make-list 3 'a)))
    
    (test-group "hash-tables"
      (let ((ht (make-hash-table)))
        (hash-set! ht 'key 'value)
        (test-equal "hash retrieval" 'value (hash-ref ht 'key))
        (test-assert "key exists" (hash-ref ht 'key #f)))))
  
  ;; Test core algorithms
  (test-group "algorithms"
    (test-group "sorting"
      (test-equal "sort empty" '() (sort '() <))
      (test-equal "sort numbers" '(1 2 3 4 5) (sort '(3 1 4 1 5) <)))
    
    (test-group "searching"
      (test-assert "member found" (member 3 '(1 2 3 4 5)))
      (test-assert "member not found" (not (member 6 '(1 2 3 4 5)))))))

;;; Integration test organization
(define (run-integration-tests)
  "Run integration tests"
  
  (test-group "file-operations"
    (with-test-directory
     (lambda ()
       (call-with-output-file "test.txt"
         (lambda (port)
           (display "test content" port)))
       
       (test-assert "file exists" (file-exists? "test.txt"))
       
       (test-equal "file content"
                   "test content"
                   (call-with-input-file "test.txt"
                     (lambda (port)
                       (get-string-all port)))))))
  
  (test-group "process-interaction"
    (test-equal "echo command" 
                "hello\n"
                (let ((output (open-input-pipe "echo hello")))
                  (let ((result (get-string-all output)))
                    (close-pipe output)
                    result)))))

;;; Performance test organization
(define (run-performance-tests)
  "Run performance tests"
  
  (define (measure-time thunk)
    "Measure execution time of thunk in milliseconds"
    (let ((start (get-internal-real-time)))
      (thunk)
      (/ (- (get-internal-real-time) start)
         (/ internal-time-units-per-second 1000))))
  
  (test-group "performance"
    (test-assert "list creation performance"
                 (< (measure-time (lambda () (make-list 10000 'x)))
                    100)) ; Should take less than 100ms
    
    (test-assert "hash table performance"
                 (< (measure-time 
                     (lambda ()
                       (let ((ht (make-hash-table)))
                         (do ((i 0 (+ i 1)))
                             ((= i 1000))
                           (hash-set! ht i (* i i))))))
                    50)))) ; Should take less than 50ms

;;; Test fixtures and utilities
(define-syntax with-fixture
  (syntax-rules ()
    ((with-fixture setup teardown body ...)
     (dynamic-wind
       setup
       (lambda () body ...)
       teardown))))

;; Example fixture usage
(define (test-with-fixtures)
  (test-group "fixture-example"
    (with-fixture
     ;; Setup
     (lambda ()
       (define temp-file "temp-test.txt")
       (call-with-output-file temp-file
         (lambda (port)
           (display "temporary data" port))))
     
     ;; Teardown
     (lambda ()
       (when (file-exists? "temp-test.txt")
         (delete-file "temp-test.txt")))
     
     ;; Test body
     (test-assert "temp file exists during test"
                  (file-exists? "temp-test.txt")))))

;;; Mock objects for testing
(define (make-mock-function)
  "Create a mock function that records calls"
  (let ((calls '()))
    (values
     ;; The mock function
     (lambda args
       (set! calls (cons args calls))
       'mock-return-value)
     ;; Accessor for calls
     (lambda () (reverse calls))
     ;; Reset function
     (lambda () (set! calls '())))))

;; Example mock usage
(define (test-with-mocks)
  (test-group "mock-example"
    (let-values (((mock-fn get-calls reset-calls) (make-mock-function)))
      (mock-fn 'arg1 'arg2)
      (mock-fn 'arg3)
      
      (test-equal "mock was called twice"
                  2
                  (length (get-calls)))
      
      (test-equal "first call args"
                  '(arg1 arg2)
                  (car (get-calls))))))

;;; Run tests if this file is executed directly
(when (equal? (car (command-line)) "test-organization-example.scm")
  (run-all-tests))