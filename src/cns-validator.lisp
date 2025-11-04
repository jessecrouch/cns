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
  context   ; Additional context
  fix       ; Suggested fix (if applicable)
  example)  ; Example of correct usage (if applicable)

(defun make-error (type severity message &key line context fix example)
  "Helper to create validation error."
  (make-validation-error :type type 
                          :severity severity 
                          :message message 
                          :line line 
                          :context context
                          :fix fix
                          :example example))

;;; ============================================================================
;;; Structure Validators
;;; ============================================================================

(defun validate-has-story (code)
  "Check if CNS code has a Story: declaration."
  (if (search "Story:" code)
      nil
      (list (make-error :missing-element :error
                       "Missing required 'Story:' declaration at the beginning"
                       :fix "Add 'Story:' line at the beginning of your CNS file"
                       :example "Story: Calculate factorial of a number"))))

(defun validate-has-given (code)
  "Check if CNS code has a Given: section."
  (if (search "Given:" code)
      nil
      (list (make-error :missing-element :warning
                       "Missing 'Given:' section - no variables declared"
                       :fix "Add 'Given:' section after Story: if you need variables"
                       :example "Given:
  n: Integer = 5
  result: Integer = 1"))))

(defun validate-has-steps (code)
  "Check if CNS code has at least one Step."
  (if (search "Step" code)
      nil
      (list (make-error :missing-element :error
                       "Missing 'Step' declarations - program has no logic"
                       :fix "Add at least one Step with logic"
                       :example "Step 1 → Perform action
  Because: We need to process the data
  Then: result becomes n * 2"))))

(defun validate-has-end (code)
  "Check if CNS code has an End: section."
  (if (search "End:" code)
      nil
      (list (make-error :missing-element :error
                       "Missing required 'End:' section"
                       :fix "Add 'End:' line at the end of your CNS file"
                       :example "End: Return result"))))

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
                           :context trimmed
                           :fix "Add → arrow between step number and description"
                           :example "Step 1 → Perform action")
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

(defun split-string-by-any (str delimiters)
  "Split string by any of the given delimiters."
  (let ((result '())
        (current ""))
    (loop for char across str do
      (if (member char delimiters)
          (when (> (length current) 0)
            (push current result)
            (setf current ""))
          (setf current (concatenate 'string current (string char)))))
    (when (> (length current) 0)
      (push current result))
    (nreverse result)))

(defun extract-variables-from-expr (expr)
  "Extract variable names from an expression string.
   Simple heuristic: looks for word tokens that aren't keywords/operators."
  (when (stringp expr)
    (let ((trimmed (trim expr))
          (vars '())
          (in-string nil)
          (cleaned-expr ""))
      
      ;; Skip Because: clauses entirely - they are documentation, not code
      (when (starts-with (string-upcase trimmed) "BECAUSE:")
        (return-from extract-variables-from-expr '()))
      
      ;; First, remove all string literals from the expression
      (loop for i from 0 below (length trimmed)
            for char = (char trimmed i)
            do (cond
                 ((and (char= char #\") (not in-string))
                  (setf in-string t))
                 ((and (char= char #\") in-string)
                  (setf in-string nil))
                 ((not in-string)
                  (setf cleaned-expr (concatenate 'string cleaned-expr (string char))))))
      
      ;; Skip if entire expression was a string literal
      (when (and (> (length cleaned-expr) 0)
                 (> (length trimmed) 0)
                 (not (char= (char trimmed 0) #\")))
        ;; Skip control flow statements and built-in functions - they have their own validators
        (when (not (or (search "go to" cleaned-expr :test #'char-equal)
                       (search "repeat from" cleaned-expr :test #'char-equal)
                       (search "return" cleaned-expr :test #'char-equal)
                       (search "READ FROM FILE" (string-upcase cleaned-expr))
                       (search "PARSE JSON" (string-upcase cleaned-expr))
                       (search "PARSE HTTP" (string-upcase cleaned-expr))
                       (search "CSV READ" (string-upcase cleaned-expr))
                       (search "CSV WRITE" (string-upcase cleaned-expr))
                       (search "LENGTH_OF" (string-upcase cleaned-expr))
                       (search "LENGTH OF" (string-upcase cleaned-expr))
                       (search "FORMAT TIME" (string-upcase cleaned-expr))
                       (search "TRIM" (string-upcase cleaned-expr))
                       (search "UPPERCASE" (string-upcase cleaned-expr))
                       (search "LOWERCASE" (string-upcase cleaned-expr))
                       (search "REPLACE" (string-upcase cleaned-expr))
                       (search "SPLIT" (string-upcase cleaned-expr))
                       (search "NOW()" (string-upcase cleaned-expr))
                       (search "TIMESTAMP()" (string-upcase cleaned-expr))
                       (search "EXTRACT" (string-upcase cleaned-expr))
                       (search "MATCHES" (string-upcase cleaned-expr))
                       ;; v2.0.0 Process Management compound statements
                       (search "SHELL" (string-upcase cleaned-expr))
                       (search "BACKGROUND" (string-upcase cleaned-expr))
                       (search "KILL" (string-upcase cleaned-expr))
                       (search "WAIT FOR" (string-upcase cleaned-expr))
                       (search "STATUS OF" (string-upcase cleaned-expr))
                       (search "PARSE_INT" (string-upcase cleaned-expr))
                       (search "PARSE_FLOAT" (string-upcase cleaned-expr))
                       (search "PARSE_BOOL" (string-upcase cleaned-expr))
                       ;; v2.0.0 Database compound statements
                       (search "DATABASE" (string-upcase cleaned-expr))
                       ;; CLI argument functions
                       (search "ARGS[" (string-upcase cleaned-expr))
                       (search "ARG(" (string-upcase cleaned-expr))
                       (search "HAS_FLAG(" (string-upcase cleaned-expr))
                       ;; File operations
                       (search "FILE EXISTS" (string-upcase cleaned-expr))
                       ;; List operations
                       (search "SLICE" (string-upcase cleaned-expr))
                       (search "REVERSE" (string-upcase cleaned-expr))
                       (search "UNIQUE" (string-upcase cleaned-expr))
                       (search "SORT" (string-upcase cleaned-expr))
                       (search "FIRST FROM" (string-upcase cleaned-expr))
                       (search "KEYS OF" (string-upcase cleaned-expr))
                       (search "VALUES OF" (string-upcase cleaned-expr))
                       (search "MERGE" (string-upcase cleaned-expr))
                       ;; String operations
                       (search "PAD" (string-upcase cleaned-expr))
                       (search "STRIP" (string-upcase cleaned-expr))
                       (search "URL_ENCODE" (string-upcase cleaned-expr))
                       (search "URL_DECODE" (string-upcase cleaned-expr))
                       (search "STARTS WITH" (string-upcase cleaned-expr))))
          ;; Split by common operators and keywords
          (let ((tokens (split-string-by-any cleaned-expr '(#\Space #\+ #\- #\* #\/ #\% #\= #\< #\> #\( #\) #\[ #\] #\, #\"))))
            (dolist (token tokens)
              (let ((tok (trim token)))
                (when (and (> (length tok) 0)
                           ;; Not a number
                           (not (digit-char-p (char tok 0)))
                           (not (string-equal tok "-"))  ; Negative sign alone
                           ;; Not an operator or keyword
                           (not (member (string-upcase tok) '("BECOMES" "TO" "FROM" "AND" "OR" "IF" "THEN" "OTHERWISE" 
                                                                "STEP" "END" "SET" "PRINT" "TRUE" "FALSE" "NIL" "T"
                                                                "REPEAT" "GO" "RETURN" "EACH" "FOR" "AT" "BY" "WITH"
                                                                "SPLIT" "JOIN" "NOW" "ENV" "CONTAINS" "IN" "THE" "A" "AN"
                                                                "SQRT" "POW" "ABS" "ROUND" "FLOOR" "CEIL" "MIN" "MAX" "RANDOM" "OF"
                                                                "PARSE" "JSON" "GET" "LENGTH" "CSV" "READ" "WRITE" "HEADERS"
                                                                "ADD" "LIST" "INTO" "SPACES" "LENGTH_OF"
                                                                "FORMAT" "TIME" "TRIM" "UPPERCASE" "LOWERCASE" "REPLACE"
                                                                "DAYS" "HOURS" "MINUTES" "SECONDS" "TIMESTAMP"
                                                                "EXTRACT" "MATCHES" "GROUP"
                                                                ;; v2.0.0 Process Management Keywords
                                                                "SHELL" "BACKGROUND" "KILL" "WAIT" "STATUS" "SIGKILL" "SIGTERM" 
                                                                "SIGINT" "SIGHUP" "TIMEOUT"
                                                                ;; v2.0.0 Type Conversion Functions
                                                                "PARSE_INT" "PARSE_FLOAT" "PARSE_BOOL"
                                                                ;; v2.0.0 Database Keywords
                                                                "DATABASE" "QUERY" "EXECUTE" "CONNECT" "AS" "ON" "CLOSE"
                                                                ;; Additional CLI keywords
                                                                "ARGS" "ARG" "HAS_FLAG"
                                                                ;; File operations
                                                                "FILE" "EXISTS"
                                                                ;; List operations
                                                                "SLICE" "REVERSE" "UNIQUE" "SORT" "FIRST" "KEYS" "VALUES" "MERGE"
                                                                ;; String operations
                                                                "PAD" "STRIP" "LEFT" "RIGHT" "URL_ENCODE" "URL_DECODE" "STARTS" "ENDS")
                                         :test #'string-equal))
                           ;; Not empty
                           (> (length tok) 0))
                  (push tok vars)))))))
      (remove-duplicates vars :test #'string-equal))))

(defun validate-variable-declarations (ast)
  "Check that all variables used are declared in Given or assigned before use."
  (let ((declared-vars '())
        (assigned-vars '())
        (errors '())
        (step-num 0)
        ;; Built-in CNS variables that don't need declaration
        (builtin-vars '("REQUEST_METHOD" "REQUEST_PATH" "REQUEST_BODY" 
                        "HTTP_STATUS" "HTTP_HEADERS" "QUERY_STRING"
                        "CLIENT_IP" "SERVER_PORT" "TIMESTAMP"
                        "ENV" "ARGS" "ARGC" "REQUEST_DATA")))
    ;; Extract declared variables from Given
    (dolist (node ast)
      (when (and (listp node) (eql (car node) 'given))
        (dolist (var (cdr node))
          (when (and (listp var) (eql (car var) 'var))
            (push (cadr var) declared-vars)))))
    
    ;; Check variables in steps
    (dolist (node ast)
      (when (and (listp node) (eql (car node) 'step))
        (setf step-num (cadr node))
        ;; SKIP action field - it's just a step description, not executable code
        ;; The action field contains the step title like "Create server socket"
        ;; which should not be validated for variable usage
        
        ;; Check Then clauses and track assignments
        (dolist (clause (cddr node))
          ;; Skip Because: clauses - they are documentation, not code
          ;; Only process Then clauses
          (when (and (listp clause) (eql (car clause) 'then))
            (let ((then-str (cadr clause)))
              (when then-str
                ;; Check for assignment: "var becomes expr"
                (if (search "becomes" then-str :test #'char-equal)
                    (let ((becomes-pos (search "becomes" then-str :test #'char-equal)))
                      (when becomes-pos
                        (let ((var-name (trim (subseq then-str 0 becomes-pos)))
                              (expr (trim (subseq then-str (+ becomes-pos 7)))))
                          ;; Track this assignment
                          (push var-name assigned-vars)
                          ;; Check variables in the expression
                          (let ((vars (extract-variables-from-expr expr)))
                            (dolist (var vars)
                              (unless (or (member var declared-vars :test #'string-equal)
                                         (member var assigned-vars :test #'string-equal)
                                         (member var builtin-vars :test #'string-equal))
                                (push (make-error :semantic :error
                                                 (format nil "Variable '~A' used before declaration in Step ~D" var step-num)
                                                 :context then-str
                                                 :fix (format nil "Add to Given section: ~A: Type = initial_value" var)
                                                 :example (format nil "Given:~%  ~A: Integer = 0" var))
                                      errors)))))))
                    ;; Not an assignment, check all variables
                    (let ((vars (extract-variables-from-expr then-str)))
                       (dolist (var vars)
                        (unless (or (member var declared-vars :test #'string-equal)
                                   (member var assigned-vars :test #'string-equal)
                                   (member var builtin-vars :test #'string-equal))
                          (push (make-error :semantic :error
                                           (format nil "Variable '~A' used before declaration in Step ~D" var step-num)
                                           :context then-str
                                           :fix (format nil "Add to Given section: ~A: Type = initial_value" var)
                                           :example (format nil "Given:~%  ~A: Integer = 0" var))
                                errors)))))))))))
    
    (nreverse errors)))

(defun validate-step-sequence (ast)
  "Check that steps are numbered sequentially."
  (let ((errors '())
        (expected-step 1))
    (dolist (node ast)
      (when (and (listp node) (eql (car node) 'step))
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
      (when (and (listp node) (eql (car node) 'step))
        (push (cadr node) step-numbers)))
    
    ;; Check that all referenced steps exist
    (dolist (node ast)
      (when (and (listp node) (eql (car node) 'step))
        (dolist (clause (cddr node))
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
                                       :context text
                                       :fix (format nil "Create Step ~D or reference an existing step" step-num)
                                       :example "If: condition
  Then: repeat from Step 1")
                            errors))))
                ;; Check for "go to Step N"
                (when (search "go to Step" text)
                  (let* ((step-str (subseq text (+ (search "Step" text) 5)))
                         (step-num (parse-integer step-str :junk-allowed t)))
                    (when (and step-num (not (member step-num step-numbers)))
                      (push (make-error :logic :error
                                       (format nil "References non-existent Step ~D" step-num)
                                       :context text
                                       :fix (format nil "Create Step ~D or reference an existing step" step-num)
                                       :example "If: condition
  Then: go to End")
                            errors))))))))))
    (nreverse errors)))

(defparameter *valid-effect-patterns*
  '(;; I/O Effects
    "^PRINT\\s+"
    "^PRINT$"
    "^LOG\\s+"
    
    ;; HTTP Effects
    "^HTTP\\s+GET\\s+FROM\\s+"
    "^HTTP\\s+POST\\s+TO\\s+"
    "^HTTP\\s+PUT\\s+TO\\s+"
    "^HTTP\\s+DELETE\\s+TO\\s+"
    
    ;; Network/Socket Effects
    "^CREATE\\s+SOCKET\\s+"
    "^ACCEPT\\s+CONNECTION\\s+"
    "^NETWORK\\s+READ"
    "^SEND\\s+.+\\s+TO\\s+CLIENT"
    "^SEND\\s+.+\\s+TO\\s+"
    "^CLOSE\\s+CONNECTION"
    "^CLOSE\\s+SOCKET"
    
    ;; File Effects
    "^WRITE\\s+.+\\s+TO\\s+FILE"
    "^WRITE\\s+.+\\s+TO\\s+"
    "^APPEND\\s+.+\\s+TO\\s+FILE"
    "^APPEND\\s+.+\\s+TO\\s+"
    "^READ\\s+FROM\\s+FILE"
    
    ;; Database Effects
    "^DATABASE\\s+CONNECT\\s+"
    "^DATABASE\\s+EXECUTE\\s+"
    "^DATABASE\\s+QUERY\\s+"
    "^DB\\s+CONNECT\\s+"
    "^DB\\s+EXECUTE\\s+"
    "^DB\\s+QUERY\\s+"
    
    ;; CSV Effects
    "^CSV\\s+WRITE\\s+"
    "^CSV\\s+READ\\s+"
    "^WRITE\\s+CSV\\s+"
    "^READ\\s+CSV\\s+"
    
    ;; Shell Effects
    "^SHELL\\s+"
    "^SHELL$"
    
    ;; Git Effects
    "^GIT\\s+STATUS\\s+"
    "^GIT\\s+DIFF\\s+"
    "^GIT\\s+CHECKOUT\\s+"
    "^GIT\\s+ADD\\s+"
    "^GIT\\s+COMMIT\\s+"
    "^GIT\\s+CLONE\\s+"
    "^GIT\\s+BRANCH\\s+"
    "^GIT\\s+LOG\\s+"
    "^GIT\\s+MERGE\\s+"
    
    ;; Search Effects
    "^FIND\\s+"
    "^GREP\\s+"
    
    ;; List Effects
    "^ADD\\s+.+\\s+TO\\s+LIST\\s+"
    "^REMOVE\\s+.+\\s+FROM\\s+LIST\\s+"
    
    ;; General patterns that might appear in effects
    "^.+\\s+INTO\\s+.+$")
  "Valid effect patterns based on the CNS interpreter implementation.
   Patterns are regular expressions tested against uppercase effect strings.")

(defun validate-effects (ast)
  "Check that effects are properly declared and valid."
  (let ((errors '()))
    (dolist (node ast)
      (when (and (listp node) (eql (car node) 'step))
        (dolist (clause (cddr node))
          (when (and (listp clause) (eql (car clause) 'effect))
            (let* ((effect-str (cadr clause))
                   (effect-upper (string-upcase effect-str))
                   (matched nil))
              ;; Try to match against known effect patterns using regex if available
              (dolist (pattern *valid-effect-patterns*)
                (when (if *regex-enabled*
                          ;; Use regex matching if cl-ppcre is available
                          (handler-case
                              (funcall (symbol-function (intern "SCAN" :cl-ppcre))
                                      pattern effect-upper)
                            (error () nil))
                          ;; Fall back to simple string search
                          (search (substitute #\Space #\\ pattern) effect-upper))
                  (setf matched t)
                  (return)))
              ;; If no pattern matched, give a warning (not an error)
              (unless matched
                (push (make-error :semantic :warning
                                 (format nil "Unrecognized effect pattern: ~A~%~
                                             This may be valid but doesn't match known patterns." 
                                         effect-str)
                                 :context effect-str)
                      errors)))))))
    (nreverse errors)))

;;; ============================================================================
;;; Main Validation Function
;;; ============================================================================

(defun validate-cns (code)
  "Perform comprehensive validation of CNS code.
   Returns (values is-valid error-list warning-list).
   is-valid is T if no errors (warnings are OK).
   error-list contains all errors.
   warning-list contains all warnings."
   (let ((all-errors '())
         (errors '())
         (warnings '()))
     
     ;; Use code directly (no CNSC expansion in validator)
     (let ((expanded-code code))
     
     ;; Structure validation
     (setf all-errors (append all-errors
                              (validate-has-story expanded-code)
                              (validate-has-given expanded-code)
                              (validate-has-steps expanded-code)
                              (validate-has-end expanded-code)
                              (validate-has-because-clauses expanded-code)))
     
     ;; Syntax validation
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
  "Print a validation error in a readable format with suggestions and examples."
  (format stream "~A: ~A~%"
          (case (validation-error-severity err)
            (:error "ERROR")
            (:warning "WARNING"))
          (validation-error-message err))
  (when (validation-error-line err)
    (format stream "  Line ~D~%" (validation-error-line err)))
  (when (validation-error-context err)
    (format stream "  Context: ~A~%" (validation-error-context err)))
  (when (validation-error-fix err)
    (format stream "  Fix: ~A~%" (validation-error-fix err)))
  (when (validation-error-example err)
    (format stream "  Example:~%~{    ~A~%~}" 
            (split-string (validation-error-example err) #\Newline))))

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
