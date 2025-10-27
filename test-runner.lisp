;;; CNS Test Runner
;;; Simple testing framework for CNS interpreter

(load "cns.lisp")

;;; ============================================================================
;;; Test Framework
;;; ============================================================================

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)
(defvar *test-results* '())

(defun reset-test-stats ()
  "Reset test statistics."
  (setf *tests-passed* 0)
  (setf *tests-failed* 0)
  (setf *test-results* '()))

(defun test-cns (name cns-code expected-result)
  "Test a CNS program and check if result matches expected."
  (format t "~%Testing: ~A~%" name)
  (handler-case
      (let* ((ast (parse-cns cns-code))
             (result (interpret-cns ast :verbose nil)))
        (if (equal result expected-result)
            (progn
              (format t "  ✓ PASSED (result: ~A)~%" result)
              (incf *tests-passed*)
              (push (list :pass name) *test-results*))
            (progn
              (format t "  ✗ FAILED~%")
              (format t "    Expected: ~A~%" expected-result)
              (format t "    Got:      ~A~%" result)
              (incf *tests-failed*)
              (push (list :fail name expected-result result) *test-results*))))
    (error (e)
      (format t "  ✗ ERROR: ~A~%" e)
      (incf *tests-failed*)
      (push (list :error name e) *test-results*))))

(defun test-cns-file (name filepath expected-result)
  "Test a CNS file and check if result matches expected."
  (format t "~%Testing file: ~A (~A)~%" name filepath)
  (handler-case
      (with-open-file (stream filepath)
        (let ((code (make-string (file-length stream))))
          (read-sequence code stream)
          (test-cns name code expected-result)))
    (error (e)
      (format t "  ✗ ERROR loading file: ~A~%" e)
      (incf *tests-failed*)
      (push (list :error name e) *test-results*))))

(defun print-test-summary ()
  "Print test results summary."
  (format t "~%~%===========================================~%")
  (format t "TEST SUMMARY~%")
  (format t "===========================================~%")
  (format t "Passed: ~A~%" *tests-passed*)
  (format t "Failed: ~A~%" *tests-failed*)
  (format t "Total:  ~A~%" (+ *tests-passed* *tests-failed*))
  (format t "===========================================~%")
  (if (zerop *tests-failed*)
      (format t "✓ ALL TESTS PASSED!~%")
      (format t "✗ SOME TESTS FAILED~%"))
  (format t "===========================================~%~%"))

;;; ============================================================================
;;; Test Suite
;;; ============================================================================

(defun run-all-tests ()
  "Run all CNS tests."
  (reset-test-stats)
  (format t "~%~%")
  (format t "===========================================~%")
  (format t "CNS INTERPRETER TEST SUITE~%")
  (format t "===========================================~%")
  
  ;; Test 1: Simple factorial
  (test-cns "Factorial of 5"
            "Story: Compute factorial of a positive integer

Given:
  n: Integer = 5
  result: Integer = 1

Step 1 → Multiply result by n
  Because: n contributes to the product
  Then: n becomes n - 1

Step 2 → If n > 1
  Because: we need to include all integers down to 1
  Then: repeat from Step 1
  Otherwise: go to End

End: Return result"
            120)
  
  ;; Test 2: Factorial of 3
  (test-cns "Factorial of 3"
            "Story: Compute factorial of 3

Given:
  n: Integer = 3
  result: Integer = 1

Step 1 → Multiply result by n
  Because: n contributes to the product
  Then: n becomes n - 1

Step 2 → If n > 1
  Because: we need to include all integers down to 1
  Then: repeat from Step 1
  Otherwise: go to End

End: Return result"
            6)
  
  ;; Test 3: Factorial of 1 (edge case)
  (test-cns "Factorial of 1"
            "Story: Compute factorial of 1

Given:
  n: Integer = 1
  result: Integer = 1

Step 1 → Multiply result by n
  Because: n contributes to the product
  Then: n becomes n - 1

Step 2 → If n > 1
  Because: we need to include all integers down to 1
  Then: repeat from Step 1
  Otherwise: go to End

End: Return result"
            1)
  
  ;; Test 4: Counter test
  (test-cns "Counter to 5"
            "Story: Count to 5

Given:
  counter: Integer = 0
  target: Integer = 5

Step 1 → If counter < target
  Because: we haven't reached target
  Then: counter becomes counter + 1
  Otherwise: go to End

Step 2 → If counter < target
  Because: check if we continue
  Then: repeat from Step 1
  Otherwise: go to End

End: Return counter"
            5)
  
  ;; Test files if they exist
  (when (probe-file "examples/factorial.cns")
    (test-cns-file "Factorial file" "examples/factorial.cns" 120))
  
  (when (probe-file "examples/simple-counter.cns")
    (test-cns-file "Counter file" "examples/simple-counter.cns" 5))
  
  ;; Print summary
  (print-test-summary)
  
  ;; Return success/failure
  (zerop *tests-failed*))

;;; Run tests when loaded
(format t "~%Run tests with: (run-all-tests)~%~%")
