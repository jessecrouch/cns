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

(defun parse-http-request (request-string)
  "Parse HTTP request string into structured data.
   Example: 'GET /path HTTP/1.1' -> (:method 'GET' :url '/path' :version 'HTTP/1.1')"
  (let* ((lines (split-string request-string #\Newline))
         (request-line (car lines)))
    (when request-line
      (let* ((parts (split-string request-line #\Space))
             (method (if (>= (length parts) 1) (trim (car parts)) "GET"))
             (url (if (>= (length parts) 2) (trim (cadr parts)) "/"))
             (version (if (>= (length parts) 3) (trim (caddr parts)) "HTTP/1.1")))
        (list :request 
              :method method 
              :url url 
              :version version)))))

(defun match-route (routes request-method request-url)
  "Find matching route in routes list.
   Routes format: ((method url response) ...)
   Returns response string or nil if not found."
  (dolist (route routes)
    (when (and (string-equal (car route) request-method)
               (string-equal (cadr route) request-url))
      (return-from match-route (caddr route))))
  nil)

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
                  (rest (cdr parts)))
             (when rest
               (let* ((type-and-value (trim (car rest)))
                      ;; Check if there's a semantic tag [...]
                      (bracket-start (position #\[ type-and-value))
                      (actual-type-val (if bracket-start
                                          (trim (subseq type-and-value 0 bracket-start))
                                          type-and-value))
                      (tag (if bracket-start
                              (let ((bracket-end (position #\] type-and-value :start bracket-start)))
                                (when bracket-end
                                  (subseq type-and-value (1+ bracket-start) bracket-end)))
                              nil))
                      (type-val-parts (split-string actual-type-val #\=))
                      (type (trim (car type-val-parts)))
                      (val (if (cdr type-val-parts) (trim (cadr type-val-parts)) nil)))
                 ;; Append to the given node (which is at (car ast))
                 (setf (cdar ast) (append (cdar ast) (list `(var ,name ,type ,val ,tag))))))))
         
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
          
          ;; Error section
          ((starts-with trimmed "Error:")
           ;; Finish previous step if any
           (when current-step
             (push (nreverse current-step) ast))
           (setf current-section :error)
           (setf current-step nil)
           (push '(error) ast))
          
          ;; Error section content (Return, Effect, Because)
          ((and (eql current-section :error) indented)
           (cond
            ((starts-with trimmed "Return")
             (let ((return-value (trim (subseq trimmed 6))))
               (setf (cdar ast) (append (cdar ast) (list `(return ,return-value))))))
            ((starts-with trimmed "Effect:")
             (let ((effect-str (trim (subseq trimmed 7))))
               (setf (cdar ast) (append (cdar ast) (list `(effect ,effect-str))))))
            ((starts-with trimmed "Because:")
             (let ((because-str (trim (subseq trimmed 8))))
               (setf (cdar ast) (append (cdar ast) (list `(because ,because-str))))))))
          
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
             
             ;; Property access: object.property (e.g., request.method)
             ((position #\. trimmed)
              (let* ((dot-pos (position #\. trimmed))
                     (obj-name (trim (subseq trimmed 0 dot-pos)))
                     (prop-name (trim (subseq trimmed (1+ dot-pos))))
                     (obj (gethash obj-name env)))
                (when (and obj (listp obj))
                  (let ((prop-keyword (intern (string-upcase prop-name) :keyword)))
                    (getf obj prop-keyword)))))
            
             ;; Socket: Create server socket on port
             ((starts-with (string-upcase trimmed) "CREATE ")
              (let* ((parts (split-string trimmed #\Space))
                     (socket-name (trim (cadr parts)))
                     (on-idx (position "on" parts :test #'string-equal)))
                (when on-idx
                  (let ((port (eval-expr (trim (nth (1+ on-idx) parts)) env)))
                    (setf (gethash socket-name env)
                          (list :socket :port port :listening t))
                    socket-name))))
             
             ;; Socket: Accept connection on socket
             ((starts-with (string-upcase trimmed) "ACCEPT ")
              (let* ((parts (split-string trimmed #\Space))
                     (on-idx (position "on" parts :test #'string-equal)))
                (when on-idx
                  (let ((socket-name (trim (nth (1+ on-idx) parts))))
                    ;; Return a mock connection object
                    (list :connection :socket socket-name :client "simulated")))))
             
             ;; HTTP: Parse HTTP request from variable
             ((starts-with (string-upcase trimmed) "PARSE HTTP REQUEST ")
              (let* ((rest (trim (subseq trimmed 19)))
                     (from-pos (search "FROM " (string-upcase rest))))
                (if from-pos
                    (let* ((var-name (trim (subseq rest (+ from-pos 5))))
                           (request-data (gethash var-name env)))
                      (if request-data
                          (parse-http-request request-data)
                          (list :request :method "GET" :url "/" :version "HTTP/1.1")))
                    ;; Default mock request
                    (list :request :method "GET" :url "/" :version "HTTP/1.1"))))
             
             ;; Socket: Find route
             ((starts-with (string-upcase trimmed) "FIND ROUTE ")
              ;; Simplified route matching - will enhance later
              ;; For now, just return found or not found
              :route-not-found)
             
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
     
      ;; Socket: Create socket
      ((starts-with (string-upcase trimmed) "CREATE SOCKET ")
       (let* ((rest (trim (subseq trimmed 14)))
              (on-pos (search " ON " (string-upcase rest))))
         (when on-pos
           (let* ((socket-name (trim (subseq rest 0 on-pos)))
                  (port-expr (trim (subseq rest (+ on-pos 4))))
                  (port (eval-expr port-expr env)))
             ;; For now, store socket metadata (will need usocket later)
             (setf (gethash socket-name env)
                   (list :socket :port port :listening t))
             (when verbose
               (format t "  Effect: Created socket ~A on port ~A~%" socket-name port))))))
      
      ;; Socket: Bind socket (part of socket creation)
      ((starts-with (string-upcase trimmed) "BIND SOCKET")
       (when verbose
         (format t "  Effect: Bind socket~%")))
      
      ;; Socket: Accept connection
      ((starts-with (string-upcase trimmed) "ACCEPT CONNECTION")
       (when verbose
         (format t "  Effect: Accept connection (simulated)~%")))
      
      ;; Socket: Network read
      ((starts-with (string-upcase trimmed) "NETWORK READ")
       (when verbose
         (format t "  Effect: Network read (simulated)~%")))
      
      ;; Socket: Network write
      ((starts-with (string-upcase trimmed) "NETWORK WRITE")
       (when verbose
         (format t "  Effect: Network write (simulated)~%")))
      
      ;; Socket: Send response
      ((starts-with (string-upcase trimmed) "SEND ")
       (let* ((rest (trim (subseq trimmed 5)))
              (to-pos (search " TO " (string-upcase rest))))
         (when to-pos
           (let* ((content (trim (subseq rest 0 to-pos)))
                  (target (trim (subseq rest (+ to-pos 4))))
                  (expanded (substitute-vars content env)))
             (when verbose
               (format t "  Effect: Send ~A to ~A~%" expanded target))))))
      
      ;; Socket: Close socket
      ((starts-with (string-upcase trimmed) "CLOSE SOCKET")
       (let ((socket-name (trim (subseq trimmed 13))))
         (setf (gethash socket-name env) nil)
         (when verbose
           (format t "  Effect: Close socket ~A~%" socket-name))))
      
      ;; Log (for error handling)
      ((starts-with (string-upcase trimmed) "LOG ")
       (let ((msg (trim (subseq trimmed 4))))
         (when verbose
           (format t "  Effect: Log ~A~%" msg))))
      
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
        (result nil)
        (error-block nil))
    
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
        
        (error (setf error-block node))
        
        (end (setf result node))))
    
    (setf steps (nreverse steps))  ; Steps in order
    
    ;; Phase 2: Execute steps in loop (with error handling)
    (when verbose (format t "~%Execution Trace:~%"))
    (handler-case
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
              
              ;; Execute Then clauses if present (only for non-conditional steps)
              (when (not if-node)
                (dolist (then-clause then-clauses)
                  (eval-expr then-clause env (format nil "Step ~A Then clause" step-num))
                  (when verbose
                    (format t "  Then: ~A~%" then-clause))))
              
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
                       ;; Condition true - execute Then clauses
                       (progn
                         ;; Execute all Then clauses for conditional
                         (dolist (then-clause then-clauses)
                           ;; Check if it's a control flow or assignment
                           (cond
                            ((search "repeat from Step" then-clause)
                             ;; Handle later in control flow
                             nil)
                            ((search "go to Step" then-clause)
                             ;; Handle later in control flow
                             nil)
                            (t
                             ;; Regular assignment or expression
                             (eval-expr then-clause env (format nil "Step ~A Then clause" step-num))
                             (when verbose
                               (format t "  Then: ~A~%" then-clause)))))
                         
                         ;; Check last Then clause for control flow
                         (let ((last-then (car (last then-clauses))))
                           (if (and last-then (search "repeat from Step" last-then))
                               (let* ((step-pos (search "Step " last-then))
                                      (num-start (+ step-pos 5))
                                      (target-step (parse-integer last-then :start num-start :junk-allowed t)))
                                 (when verbose
                                   (format t "  -> Jumping to Step ~A~%" target-step))
                                 (setf pc (1- target-step)))
                               (if (and last-then (search "go to Step" last-then))
                                   (let* ((step-pos (search "Step " last-then))
                                          (num-start (+ step-pos 5))
                                          (target-step (parse-integer last-then :start num-start :junk-allowed t)))
                                     (when verbose
                                       (format t "  -> Going to Step ~A~%" target-step))
                                     (setf pc (1- target-step)))
                                   (incf pc)))))
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
      
      ;; Error handler - execute Error: block if present
      (error (e)
        (when verbose
          (format t "~%!!! Error occurred: ~A~%" e))
        (when error-block
          (when verbose
            (format t "~%=== Executing Error Block ===~%"))
          (let* ((error-return (cadr (assoc 'return (cdr error-block))))
                 (error-effects (mapcar #'cadr (remove-if-not (lambda (x) (eq (car x) 'effect)) (cdr error-block))))
                 (error-because (cadr (assoc 'because (cdr error-block)))))
            ;; Apply error effects
            (dolist (eff error-effects)
              (apply-effect eff env verbose))
            ;; Set error result
            (when error-return
              (setf result (eval-expr error-return env "Error Return")))
            (when verbose
              (format t "Return: ~A~%" result)
              (when error-because
                (format t "Because: ~A~%" error-because)))))))
    
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
