;;; CNS Validator
;;; Comprehensive validation system for Causal Narrative Script code
;;; Checks syntax, semantics, required elements, and common errors
;;; before execution to provide clear error messages and catch issues early.

(require 'sb-bsd-sockets)

;;; Load the main CNS interpreter (for parse-cns function)
(load (merge-pathnames "cns.lisp" *load-truename*))

;;; ============================================================================
;;; Validation Error Types
;;; ============================================================================

(defstruct validation-error
  type      ; :syntax, :semantic, :missing-element, :logic
  severity  ; :error, :warning
  message   ; Human-readable error message
  line      ; Line number (if applicable)
  context)  ; Additional context

(defun make-error (type severity message &key line context)
  "Helper to create validation error."
  (make-validation-error :type type 
                          :severity severity 
                          :message message 
                          :line line 
                          :context context))

;;; ============================================================================
;;; Structure Validators
;;; ============================================================================

(defun validate-has-story (code)
  "Check if CNS code has a Story: declaration."
  (if (search "Story:" code)
      nil
      (list (make-error :missing-element :error
                       "Missing required 'Story:' declaration at the beginning"))))

(defun validate-has-given (code)
  "Check if CNS code has a Given: section."
  (if (search "Given:" code)
      nil
      (list (make-error :missing-element :warning
                       "Missing 'Given:' section - no variables declared"))))

(defun validate-has-steps (code)
  "Check if CNS code has at least one Step."
  (if (search "Step" code)
      nil
      (list (make-error :missing-element :error
                       "Missing 'Step' declarations - program has no logic"))))

(defun validate-has-end (code)
  "Check if CNS code has an End: section."
  (if (search "End:" code)
      nil
      (list (make-error :missing-element :error
                       "Missing required 'End:' section"))))

(defun validate-has-because-clauses (code)
  "Check if CNS code has Because: clauses for causality."
  (let ((step-count (count #\→ code))
        (because-count (count-matches "Because:" code)))
    (if (< because-count step-count)
        (list (make-error :missing-element :warning
                         (format nil "Only ~D 'Because:' clauses for ~D steps - causality not fully explained"
                                because-count step-count)))
        nil)))

(defun count-matches (substring string)
  "Count non-overlapping occurrences of substring in string."
  (let ((count 0)
        (pos 0))
    (loop while (setf pos (search substring string :start2 pos))
          do (incf count)
             (incf pos (length substring)))
    count))

;;; ============================================================================
;;; Syntax Validators
;;; ============================================================================

(defun validate-step-arrows (code)
  "Check that all Steps have arrows (→)."
  (let ((errors '())
        (lines (split-string code #\Newline))
        (line-num 0))
    (dolist (line lines)
      (incf line-num)
      (let ((trimmed (trim line)))
        ;; Only check lines that START with "Step" (step declarations)
        (when (and (starts-with trimmed "Step")
                   (not (position #\→ line)))
          (push (make-error :syntax :error
                           "Step declaration missing arrow (→)"
                           :line line-num
                           :context trimmed)
                errors))))
    (nreverse errors)))

(defun validate-end-format (code)
  "Check that End: follows single-line format."
  (let ((errors '())
        (lines (split-string code #\Newline))
        (line-num 0)
        (found-end nil)
        (end-line-num 0))
    (dolist (line lines)
      (incf line-num)
      (when (starts-with (trim line) "End:")
        (setf found-end t)
        (setf end-line-num line-num)
        ;; Check if End: has content on the same line
        (let ((trimmed (trim line)))
          (when (= (length trimmed) 4)  ; Just "End:"
            (push (make-error :syntax :warning
                             "End: appears to be on separate line - should be 'End: Return <value>'"
                             :line line-num
                             :context trimmed)
                  errors)))))
    (nreverse errors)))

(defun validate-indentation (code)
  "Check for proper indentation of sub-clauses."
  (let ((errors '())
        (lines (split-string code #\Newline))
        (line-num 0)
        (in-section nil))
    (dolist (line lines)
      (incf line-num)
      (let ((trimmed (trim line)))
        (cond
          ;; Section headers should not be indented
          ((or (starts-with trimmed "Story:")
               (starts-with trimmed "Given:")
               (starts-with trimmed "Step")
               (starts-with trimmed "End:"))
           (when (starts-with line "  ")
             (push (make-error :syntax :warning
                              "Section header should not be indented"
                              :line line-num
                              :context trimmed)
                   errors))
           (setf in-section t))
          
          ;; Sub-clauses should be indented
          ((and in-section
                (or (starts-with trimmed "Because:")
                    (starts-with trimmed "Then:")
                    (starts-with trimmed "Otherwise:")
                    (starts-with trimmed "Effect:")
                    (starts-with trimmed "If")))
           (when (not (starts-with line "  "))
             (push (make-error :syntax :warning
                              "Sub-clause should be indented with 2 spaces"
                              :line line-num
                              :context trimmed)
                   errors))))))
    (nreverse errors)))

;;; ============================================================================
;;; Semantic Validators
;;; ============================================================================

(defun validate-variable-declarations (ast)
  "Check that all variables used are declared in Given."
  (let ((declared-vars '())
        (errors '()))
    ;; Extract declared variables
    (dolist (node ast)
      (when (and (listp node) (eql (car node) 'given))
        (dolist (var (cdr node))
          (when (and (listp var) (eql (car var) 'var))
            (push (cadr var) declared-vars)))))
    
    ;; TODO: Check that all referenced variables are declared
    ;; This would require parsing expressions in steps
    
    errors))

(defun validate-step-sequence (ast)
  "Check that steps are numbered sequentially."
  (let ((errors '())
        (expected-step 1))
    (dolist (node ast)
      (when (and (listp node) (eql (caddr node) 'step))
        (let ((step-num (cadr node)))
          (when (not (eql step-num expected-step))
            (push (make-error :logic :warning
                             (format nil "Step ~D found, expected Step ~D" 
                                    step-num expected-step))
                  errors))
          (setf expected-step (1+ step-num)))))
    (nreverse errors)))

(defun validate-control-flow (ast)
  "Check for valid control flow (repeat from, go to, etc.)."
  (let ((errors '())
        (step-numbers '()))
    ;; Collect all step numbers
    (dolist (node ast)
      (when (and (listp node) (eql (caddr node) 'step))
        (push (cadr node) step-numbers)))
    
    ;; Check that all referenced steps exist
    (dolist (node ast)
      (when (and (listp node) (eql (caddr node) 'step))
        (dolist (clause (car node))
          (when (listp clause)
            (let ((text (cadr clause)))
              (when (stringp text)
                ;; Check for "repeat from Step N"
                (when (search "repeat from Step" text)
                  (let* ((step-str (subseq text (+ (search "Step" text) 5)))
                         (step-num (parse-integer step-str :junk-allowed t)))
                    (when (and step-num (not (member step-num step-numbers)))
                      (push (make-error :logic :error
                                       (format nil "References non-existent Step ~D" step-num)
                                       :context text)
                            errors))))
                ;; Check for "go to Step N"
                (when (search "go to Step" text)
                  (let* ((step-str (subseq text (+ (search "Step" text) 5)))
                         (step-num (parse-integer step-str :junk-allowed t)))
                    (when (and step-num (not (member step-num step-numbers)))
                      (push (make-error :logic :error
                                       (format nil "References non-existent Step ~D" step-num)
                                       :context text)
                            errors))))))))))
    (nreverse errors)))

(defun validate-effects (ast)
  "Check that effects are properly declared and valid."
  (let ((errors '()))
    (dolist (node ast)
      (when (and (listp node) (eql (caddr node) 'step))
        (dolist (clause (car node))
          (when (and (listp clause) (eql (car clause) 'effect))
            (let ((effect-str (cadr clause)))
              ;; Check for common effect patterns
              (cond
                ;; Network effects should specify socket
                ((or (search "Accept connection" effect-str)
                     (search "Send" effect-str)
                     (search "Create socket" effect-str))
                 ;; Valid network effect
                 nil)
                ;; File effects should specify filename
                ((or (search "Write to file" effect-str)
                     (search "Read from file" effect-str))
                 ;; Valid file effect
                 nil)
                ;; Print effects
                ((search "Print" effect-str)
                 ;; Valid print effect
                 nil)
                ;; Unknown effect - warning
                (t
                 (push (make-error :semantic :warning
                                  (format nil "Unrecognized effect pattern: ~A" effect-str)
                                  :context effect-str)
                       errors))))))))
    (nreverse errors)))

;;; ============================================================================
;;; Main Validation Function
;;; ============================================================================

(defun validate-cns (code)
  "Perform comprehensive validation of CNS code.
   Automatically detects and expands CNSC (compact) format.
   Returns (values is-valid error-list warning-list).
   is-valid is T if no errors (warnings are OK).
   error-list contains all errors.
   warning-list contains all warnings."
  (let ((all-errors '())
        (errors '())
        (warnings '()))
    
    ;; Auto-expand CNSC to CNS if detected
    (let ((expanded-code (if (is-cnsc-code code)
                             (expand-cnsc-to-cns code)
                             code)))
    
    ;; Structure validation (on expanded code)
    (setf all-errors (append all-errors
                             (validate-has-story expanded-code)
                             (validate-has-given expanded-code)
                             (validate-has-steps expanded-code)
                             (validate-has-end expanded-code)
                             (validate-has-because-clauses expanded-code)))
    
    ;; Syntax validation (on expanded code)
    (setf all-errors (append all-errors
                             (validate-step-arrows expanded-code)
                             (validate-end-format expanded-code)
                             (validate-indentation expanded-code)))
    
    ;; Try to parse - if it fails, return early
    (handler-case
        (let ((ast (parse-cns expanded-code)))
          ;; Semantic validation (on AST)
          (setf all-errors (append all-errors
                                   (validate-variable-declarations ast)
                                   (validate-step-sequence ast)
                                   (validate-control-flow ast)
                                   (validate-effects ast))))
      (error (e)
        (push (make-error :syntax :error
                         (format nil "Parse failed: ~A" e))
              all-errors)))
    
    ;; Separate errors from warnings
    (dolist (err all-errors)
      (if (eql (validation-error-severity err) :error)
          (push err errors)
          (push err warnings)))
    
    (values (null errors)  ; is-valid
            (nreverse errors)
            (nreverse warnings)))))

;;; ============================================================================
;;; Pretty Printing
;;; ============================================================================

(defun print-validation-error (err &optional (stream t))
  "Print a validation error in a readable format."
  (format stream "~A: ~A~%"
          (case (validation-error-severity err)
            (:error "ERROR")
            (:warning "WARNING"))
          (validation-error-message err))
  (when (validation-error-line err)
    (format stream "  Line ~D~%" (validation-error-line err)))
  (when (validation-error-context err)
    (format stream "  Context: ~A~%" (validation-error-context err))))

(defun print-validation-results (is-valid errors warnings &optional (stream t))
  "Print validation results in a user-friendly format."
  (format stream "~%=== CNS Validation Results ===~%~%")
  
  (if (null errors)
      (format stream "✓ No errors found~%")
      (progn
        (format stream "✗ ~D error(s) found:~%~%" (length errors))
        (dolist (err errors)
          (print-validation-error err stream)
          (format stream "~%"))))
  
  (if (null warnings)
      (format stream "✓ No warnings~%")
      (progn
        (format stream "⚠ ~D warning(s):~%~%" (length warnings))
        (dolist (warn warnings)
          (print-validation-error warn stream)
          (format stream "~%"))))
  
  (format stream "~%Overall: ~A~%"
          (if is-valid
              "VALID (ready for execution)"
              "INVALID (fix errors before execution)")))

;;; ============================================================================
;;; Standalone Validator Script
;;; ============================================================================

(defun validate-file (filepath)
  "Validate a CNS file and print results."
  (handler-case
      (with-open-file (stream filepath :direction :input)
        (let ((code (make-string (file-length stream))))
          (read-sequence code stream)
          (multiple-value-bind (is-valid errors warnings)
              (validate-cns code)
            (print-validation-results is-valid errors warnings)
            (if is-valid 0 1))))  ; Return exit code
    (error (e)
      (format *error-output* "Failed to validate file: ~A~%" e)
      1)))

;;; Export main functions
(export '(validate-cns 
          validate-file
          print-validation-results
          validation-error
          validation-error-type
          validation-error-severity
          validation-error-message
          validation-error-line
          validation-error-context))
