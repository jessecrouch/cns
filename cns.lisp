;;; CNS Interpreter in Common Lisp
;;; A minimal implementation of Causal Narrative Script (CNS)
;;; CNS is a programming language optimized for LLM comprehension
;;; with explicit causality, narrative flow, and self-documenting structures.

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(defun split-string (str delimiter)
  "Split string by delimiter into list."
  (let ((result '())
        (start 0)
        (end (position delimiter str)))
    (loop while end do
          (push (subseq str start end) result)
          (setf start (1+ end))
          (setf end (position delimiter str :start start))
          finally (push (subseq str start) result))
    (nreverse result)))

(defun trim (str)
  "Trim whitespace from string."
  (string-trim '(#\Space #\Tab #\Newline #\Return) str))

(defun starts-with (str prefix)
  "Check if string starts with prefix."
  (and (>= (length str) (length prefix))
       (string= (subseq str 0 (length prefix)) prefix)))

(defun emptyp (str)
  "Check if string is empty after trim."
  (zerop (length (trim str))))

;;; ============================================================================
;;; Parser: Convert CNS string to S-expression AST
;;; ============================================================================

(defun parse-cns (code)
  "Parse CNS code string into S-exp AST.
   Returns nested list structure representing the program."
  (let ((lines (remove-if #'emptyp (split-string code #\Newline)))
        (ast '())
        (current-section nil)
        (step-id nil)
        (current-step nil))
    (dolist (line lines)
      (let ((trimmed (trim line))
            (indented (starts-with line "  ")))
        (cond
         ;; Story header
         ((starts-with trimmed "Story:")
          (push `(story ,(trim (subseq trimmed 6))) ast)
          (setf current-section :story))
         
         ;; Given section (variable declarations)
         ((starts-with trimmed "Given:")
          (setf current-section :given)
          (push '(given) ast))
         
         ;; Variable declaration in Given section
         ((and (eql current-section :given) indented)
          (let* ((parts (split-string trimmed #\:))
                 (name (trim (car parts)))
                 (rest (cdr parts))
                 (type-val (if rest (split-string (trim (car rest)) #\=) nil))
                 (type (trim (car type-val)))
                 (val (if (cdr type-val) (trim (cadr type-val)) nil))
                 (tag (if (and rest (cdr rest)) (trim (cadr rest)) nil)))
            ;; Append to the given node (which is at (car ast))
            (setf (cdar ast) (append (cdar ast) (list `(var ,name ,type ,val ,tag))))))
         
         ;; Step with arrow
         ((starts-with trimmed "Step")
          ;; Finish previous step if any
          (when current-step
            (push (nreverse current-step) ast))
          (setf current-section :steps)
          (let* ((step-parts (split-string trimmed #\→))
                 (step-content (trim (cadr step-parts))))
            (setf step-id (parse-integer (trim (subseq (car step-parts) 4))))
            ;; Check if step content starts with "If" - make it a conditional
            (if (starts-with (string-upcase step-content) "IF")
                (setf current-step (list (list 'if (trim (subseq step-content 2))) step-id 'step))
                (setf current-step (list (list 'action step-content) step-id 'step)))))
         
         ;; Because clause (causality explanation)
         ((and current-step indented (starts-with trimmed "Because:"))
          (push `(because ,(trim (subseq trimmed 8))) current-step))
         
         ;; Effect declaration
         ((and current-step indented (starts-with trimmed "Effect:"))
          (push `(effect ,(trim (subseq trimmed 7))) current-step))
         
         ;; Then clause (state transformation)
         ((and current-step indented (starts-with trimmed "Then:"))
          (push `(then ,(trim (subseq trimmed 5))) current-step))
         
         ;; Otherwise clause
         ((and current-step indented (starts-with trimmed "Otherwise:"))
          (push `(otherwise ,(trim (subseq trimmed 10))) current-step))
         
         ;; If conditional
         ((and current-step indented (starts-with trimmed "If"))
          (let ((cond-str (trim (subseq trimmed 2))))
            (push `(if ,cond-str) current-step)))
         
         ;; End section
         ((starts-with trimmed "End:")
          ;; Finish previous step if any
          (when current-step
            (push (nreverse current-step) ast))
          (setf current-section :end)
          (setf current-step nil)
          (let* ((end-content (trim (subseq trimmed 4)))
                 (return-value (if (starts-with (string-upcase end-content) "RETURN")
                                   (trim (subseq end-content 6))
                                   end-content)))
            (push `(end (return ,return-value) (because "computation complete")) ast))))))
    ;; Finish last step if any
    (when current-step
      (push (nreverse current-step) ast))
    (nreverse ast)))

;;; ============================================================================
;;; Expression Evaluator
;;; ============================================================================

(defun eval-expr (expr env &optional context)
  "Simple evaluator for expressions. Handles basic ops and vars.
   Optional context string provides better error messages."
  (cond
   ;; Already a number
   ((numberp expr) expr)
   
   ;; String expression - try to parse
   ((stringp expr)
    (handler-case
        (let ((trimmed (trim expr)))
          (cond
           ;; Variable lookup (use multiple-value-bind to check existence)
           ((multiple-value-bind (value exists) (gethash trimmed env)
              (when exists value)))
           
            ;; String literal: "hello"
            ((and (> (length trimmed) 1)
                  (char= (char trimmed 0) #\")
                  (char= (char trimmed (1- (length trimmed))) #\"))
             (subseq trimmed 1 (1- (length trimmed))))
            
            ;; List literal: [1, 2, 3]
            ((and (> (length trimmed) 1)
                  (char= (char trimmed 0) #\[)
                  (char= (char trimmed (1- (length trimmed))) #\]))
             (let* ((content (subseq trimmed 1 (1- (length trimmed))))
                    (items (split-string content #\,)))
               (mapcar (lambda (item) (eval-expr (trim item) env)) items)))
            
            ;; Try to parse as number
            ((every #'digit-char-p trimmed) (parse-integer trimmed))
           
           ;; Comparison: n > 1
           ((position #\> trimmed)
            (let ((parts (split-string trimmed #\>)))
              (> (eval-expr (trim (car parts)) env)
                 (eval-expr (trim (cadr parts)) env))))
           
           ;; Comparison: n < 1
           ((position #\< trimmed)
            (let ((parts (split-string trimmed #\<)))
              (< (eval-expr (trim (car parts)) env)
                 (eval-expr (trim (cadr parts)) env))))
           
            ;; Comparison: n ≠ 1 (not equal)
            ((search "≠" trimmed)
             (let ((parts (split-string trimmed #\≠)))
               (/= (eval-expr (trim (car parts)) env)
                   (eval-expr (trim (cadr parts)) env))))
            
            ;; Comparison: n ≤ 1 (less than or equal)
            ((search "≤" trimmed)
             (let ((parts (split-string trimmed #\≤)))
               (<= (eval-expr (trim (car parts)) env)
                   (eval-expr (trim (cadr parts)) env))))
            
            ;; Comparison: n ≥ 1 (greater than or equal)
            ((search "≥" trimmed)
             (let ((parts (split-string trimmed #\≥)))
               (>= (eval-expr (trim (car parts)) env)
                   (eval-expr (trim (cadr parts)) env))))
            
            ;; Comparison: n = 1 (must come after ≠, ≤, ≥)
            ((position #\= trimmed)
             (let ((parts (split-string trimmed #\=)))
               (= (eval-expr (trim (car parts)) env)
                  (eval-expr (trim (cadr parts)) env))))
           
            ;; Boolean: NOT expression
            ((starts-with (string-upcase trimmed) "NOT ")
             (not (eval-expr (trim (subseq trimmed 4)) env)))
            
            ;; Boolean: a AND b
            ((search " AND " (string-upcase trimmed))
             (let* ((pos (search " AND " (string-upcase trimmed)))
                    (left (subseq trimmed 0 pos))
                    (right (subseq trimmed (+ pos 5))))
               (and (eval-expr (trim left) env)
                    (eval-expr (trim right) env))))
            
            ;; Boolean: a OR b
            ((search " OR " (string-upcase trimmed))
             (let* ((pos (search " OR " (string-upcase trimmed)))
                    (left (subseq trimmed 0 pos))
                    (right (subseq trimmed (+ pos 4))))
               (or (eval-expr (trim left) env)
                   (eval-expr (trim right) env))))
            
            ;; Assignment: n becomes n - 1
            ((search "becomes" trimmed)
            (let* ((parts (split-string trimmed #\Space))
                   (becomes-pos (position "becomes" parts :test #'string=))
                   (var-name (trim (car parts)))
                   (expr-parts (subseq parts (1+ becomes-pos)))
                   (expr (format nil "~{~A~^ ~}" expr-parts)))
              (setf (gethash var-name env)
                    (eval-expr expr env))))
           
           ;; Arithmetic: result * n
           ((search "*" trimmed)
            (let ((parts (split-string trimmed #\*)))
              (* (eval-expr (trim (car parts)) env)
                 (eval-expr (trim (cadr parts)) env))))
           
           ;; Arithmetic: n - 1
           ((search "-" trimmed)
            (let ((parts (split-string trimmed #\-)))
              (- (eval-expr (trim (car parts)) env)
                 (eval-expr (trim (cadr parts)) env))))
           
            ;; Arithmetic: n + 1
            ((search "+" trimmed)
             (let ((parts (split-string trimmed #\+)))
               (+ (eval-expr (trim (car parts)) env)
                  (eval-expr (trim (cadr parts)) env))))
            
            ;; Arithmetic: n / 2 (division)
            ((search "/" trimmed)
             (let ((parts (split-string trimmed #\/)))
               (floor (/ (eval-expr (trim (car parts)) env)
                         (eval-expr (trim (cadr parts)) env)))))
            
            ;; Arithmetic: n % 2 (modulo)
            ((search "%" trimmed)
             (let ((parts (split-string trimmed #\%)))
               (mod (eval-expr (trim (car parts)) env)
                    (eval-expr (trim (cadr parts)) env))))
            
            ;; List operations: length of list
            ((starts-with (string-upcase trimmed) "LENGTH OF ")
             (let ((var-name (trim (subseq trimmed 10))))
               (length (gethash var-name env))))
            
            ;; List operations: get item at index (0-based)
            ((search " AT " (string-upcase trimmed))
             (let* ((at-pos (search " AT " (string-upcase trimmed)))
                    (list-expr (trim (subseq trimmed 0 at-pos)))
                    (index-expr (trim (subseq trimmed (+ at-pos 4))))
                    (list-val (eval-expr list-expr env))
                    (index (eval-expr index-expr env)))
               (nth index list-val)))
            
            ;; Action: Multiply result by n
            ((starts-with (string-upcase trimmed) "MULTIPLY")
            (let ((parts (split-string trimmed #\Space)))
              (setf (gethash (trim (cadr parts)) env)
                    (* (gethash (trim (cadr parts)) env)
                       (gethash (trim (fourth parts)) env)))))
           
            ;; Boolean literals
            ((string-equal trimmed "TRUE") t)
            ((string-equal trimmed "FALSE") nil)
            ((string-equal trimmed "T") t)
            ((string-equal trimmed "NIL") nil)
            
            ;; Default: try variable lookup one more time, else error
            (t (or (gethash trimmed env)
                   (error "Unknown variable or expression: ~A" trimmed)))))
       (error (e)
         ;; Silently return nil for errors - don't spam user
         ;; Only show errors in non-context calls
         (unless context
           (format t "ERROR: Could not evaluate '~A' - ~A~%" expr e))
         nil)))
   
   ;; Fallback
   (t expr)))

;;; ============================================================================
;;; Effect System - Handle side effects
;;; ============================================================================

(defun substitute-vars (text env)
  "Replace {varname} with variable values in text."
  (let ((result text))
    (loop for start = (position #\{ result)
          while start
          do (let ((end (position #\} result :start start)))
               (when end
                 (let* ((var-name (subseq result (1+ start) end))
                        (value (gethash var-name env)))
                   (setf result (concatenate 'string
                                            (subseq result 0 start)
                                            (format nil "~A" value)
                                            (subseq result (1+ end))))))))
    result))

(defun apply-effect (effect-str env verbose)
  "Execute an effect (Print, Write, etc.)."
  (let ((trimmed (trim effect-str)))
    (cond
     ;; Print "text" or Print {var} or Print "text {var}"
     ((starts-with (string-upcase trimmed) "PRINT ")
      (let* ((msg (trim (subseq trimmed 6)))
             ;; Remove quotes if present
             (unquoted (if (and (> (length msg) 1)
                               (char= (char msg 0) #\")
                               (char= (char msg (1- (length msg))) #\"))
                          (subseq msg 1 (1- (length msg)))
                          msg))
             (expanded (substitute-vars unquoted env)))
        (format t ">>> ~A~%" expanded)
        (when verbose
          (format t "  Effect: Print~%"))))
     
     ;; Write to file
     ((starts-with (string-upcase trimmed) "WRITE ")
      (let* ((rest (trim (subseq trimmed 6)))
             (to-pos (search " TO " (string-upcase rest))))
        (when to-pos
          (let* ((content (trim (subseq rest 0 to-pos)))
                 ;; Remove quotes if present
                 (unquoted (if (and (> (length content) 1)
                                   (char= (char content 0) #\")
                                   (char= (char content (1- (length content))) #\"))
                              (subseq content 1 (1- (length content)))
                              content))
                 (filepath (trim (subseq rest (+ to-pos 4))))
                 (expanded (substitute-vars unquoted env)))
            (with-open-file (stream filepath :direction :output 
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
              (write-line expanded stream))
            (when verbose
              (format t "  Effect: Write to ~A~%" filepath))))))
     
     ;; Append to file
     ((starts-with (string-upcase trimmed) "APPEND ")
      (let* ((rest (trim (subseq trimmed 7)))
             (to-pos (search " TO " (string-upcase rest))))
        (when to-pos
          (let* ((content (trim (subseq rest 0 to-pos)))
                 ;; Remove quotes if present
                 (unquoted (if (and (> (length content) 1)
                                   (char= (char content 0) #\")
                                   (char= (char content (1- (length content))) #\"))
                              (subseq content 1 (1- (length content)))
                              content))
                 (filepath (trim (subseq rest (+ to-pos 4))))
                 (expanded (substitute-vars unquoted env)))
            (with-open-file (stream filepath :direction :output 
                                   :if-exists :append
                                   :if-does-not-exist :create)
              (write-line expanded stream))
            (when verbose
              (format t "  Effect: Append to ~A~%" filepath))))))
     
     ;; Default: just display
     (t (when verbose
          (format t "  Effect: ~A~%" effect-str))))))

;;; ============================================================================
;;; Interpreter: Execute the AST
;;; ============================================================================

(defun interpret-cns (ast &key (verbose t))
  "Interpret CNS AST, return result.
   If verbose is true, prints execution trace."
  (let ((env (make-hash-table :test #'equal))  ; State variables
        (steps '())                           ; List of steps
        (pc 0)                                ; Program counter (step index)
        (result nil))
    
    ;; Phase 1: Collect sections
    (dolist (node ast)
      (case (car node)
        (story 
         (when verbose
           (format t "~%=== Executing Story: ~A ===~%" (cadr node))))
        
        (given 
         (when verbose (format t "~%Given:~%"))
         (dolist (var (cdr node))
           (let ((name (cadr var))
                 (type (caddr var))
                 (val (cadddr var)))
             (setf (gethash name env) (if val (eval-expr val env) nil))
             (when verbose
               (format t "  ~A: ~A = ~A~%" name type (gethash name env))))))
        
        (step (push node steps))
        
        (end (setf result node))))
    
    (setf steps (nreverse steps))  ; Steps in order
    
    ;; Phase 2: Execute steps in loop
    (when verbose (format t "~%Execution Trace:~%"))
    (loop while (< pc (length steps)) do
           (let* ((step (nth pc steps))
                  (step-num (cadr step))
                  (step-body (cddr step))
                  (action (cadr (assoc 'action step-body)))
                  (because (cadr (assoc 'because step-body)))
                  (then-clauses (mapcar #'cadr (remove-if-not (lambda (x) (eq (car x) 'then)) step-body)))
                  (effects (mapcar #'cadr (remove-if-not (lambda (x) (eq (car x) 'effect)) step-body)))
                  (if-node (assoc 'if step-body))
                  (otherwise-clause (cadr (assoc 'otherwise step-body))))
             
             (when verbose
               ;; Display step - handle both actions and conditionals
               (if if-node
                   (format t "~%Step ~A: If ~A~%" step-num (cadr if-node))
                   (format t "~%Step ~A: ~A~%" step-num action))
               (when because
                 (format t "  Because: ~A~%" because)))
            
             ;; Execute action (if not a conditional)
             (when action
               (eval-expr action env (format nil "Step ~A action" step-num)))
             
             ;; Execute Then clauses if present (can be multiple)
             (dolist (then-clause then-clauses)
               (eval-expr then-clause env (format nil "Step ~A Then clause" step-num))
               (when verbose
                 (format t "  Then: ~A~%" then-clause)))
             
              ;; Apply effects (real implementation)
              (dolist (eff effects)
                (apply-effect eff env verbose))
             
             ;; Display current variable state
             (when verbose
               (let ((vars '()))
                 (maphash (lambda (k v) (push (cons k v) vars)) env)
                 (when vars
                   (format t "  State: ")
                   (loop for (k . v) in (sort vars #'string< :key #'car)
                         for first = t then nil
                         do (unless first (format t ", "))
                         do (format t "~A=~A" k v))
                   (format t "~%"))))
            
             ;; Handle conditional
             (if if-node
                 (let ((cond-expr (cadr if-node)))
                   (if (eval-expr cond-expr env (format nil "Step ~A condition" step-num))
                      ;; Condition true - check last Then clause for repeat
                      (let ((last-then (car (last then-clauses))))
                        (if (and last-then (search "repeat from Step" last-then))
                            (let* ((step-pos (search "Step " last-then))
                                   (num-start (+ step-pos 5))
                                   (target-step (parse-integer last-then :start num-start :junk-allowed t)))
                              (when verbose
                                (format t "  -> Jumping to Step ~A~%" target-step))
                              (setf pc (1- target-step)))
                            (incf pc)))
                     ;; Condition false - handle otherwise
                     (cond
                      ((and otherwise-clause (search "go to End" otherwise-clause))
                       (when verbose
                         (format t "  -> Going to End~%"))
                       (return))
                      ((and otherwise-clause (search "go to Step" otherwise-clause))
                       (let* ((step-pos (search "Step " otherwise-clause))
                              (num-start (+ step-pos 5))
                              (target-step (parse-integer otherwise-clause :start num-start :junk-allowed t)))
                         (when verbose
                           (format t "  -> Going to Step ~A~%"target-step))
                         (setf pc (1- target-step))))
                      (t (incf pc)))))
                ;; No conditional, just advance
                (incf pc))))
    
    ;; Phase 3: Handle End
    (when result
      (let* ((end-node result)
             (return-expr (cadr (assoc 'return (cdr end-node))))
             (because-clause (cadr (assoc 'because (cdr end-node)))))
         (setf result (eval-expr return-expr env "End Return"))
         (when verbose
           (format t "~%=== End ===~%")
           (format t "Return: ~A~%" result)
           (format t "Because: ~A~%" because-clause))))
    
    result))

;;; ============================================================================
;;; Convenience Functions
;;; ============================================================================

(defun load-cns-file (filepath)
  "Load and execute a CNS file."
  (with-open-file (stream filepath)
    (let ((code (make-string (file-length stream))))
      (read-sequence code stream)
      (let ((ast (parse-cns code)))
        (interpret-cns ast)))))

(defun cns-repl ()
  "Simple REPL for CNS code."
  (format t "CNS REPL - Enter CNS code (type 'quit' to exit)~%")
  (format t "Multi-line input: End with a line containing only '.'~%~%")
  (loop
   (format t "cns> ")
   (force-output)
   (let ((lines '())
         (line (read-line)))
     (if (string= line "quit")
         (return)
         (progn
           (push line lines)
           (loop for line = (read-line) 
                 until (string= (trim line) ".")
                 do (push line lines))
           (let* ((code (format nil "~{~A~%~}" (nreverse (cdr lines))))
                  (ast (parse-cns code)))
             (format t "~%Result: ~A~%~%" (interpret-cns ast))))))))

;;; ============================================================================
;;; Export
;;; ============================================================================

(provide "cns")
