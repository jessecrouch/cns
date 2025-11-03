;;; CNS Interpreter in Common Lisp
;;; A minimal implementation of Causal Narrative Script (CNS)
;;; CNS is a programming language optimized for LLM comprehension
;;; with explicit causality, narrative flow, and self-documenting structures.

;;; Load socket support
(require 'sb-bsd-sockets)

;;; Load Quicklisp if available
(handler-case
    (progn
      (load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
      (format t "Quicklisp loaded successfully~%"))
  (error ()
    nil))  ; Silently continue if Quicklisp not available

;;; Load SSL support (optional - gracefully degrades to HTTP-only if not available)
(defvar *https-enabled* nil)
(handler-case
    (progn
      (if (find-package :quicklisp)
          (progn
            (funcall (intern "QUICKLOAD" :ql) :cl+ssl :silent t)
            (funcall (intern "QUICKLOAD" :ql) :flexi-streams :silent t))
          (progn
            (require 'cl+ssl)
            (require 'flexi-streams)))
      (setf *https-enabled* t)
      (format t "HTTPS support enabled (cl+ssl + flexi-streams loaded)~%"))
  (error (e)
    (format *error-output* "HTTPS support unavailable (cl+ssl not found): ~A~%~
                            Install with: (ql:quickload :cl+ssl) (ql:quickload :flexi-streams)~%~
                            Falling back to HTTP-only mode.~%" e)))

;;; Load regex support (optional - gracefully degrades if not available)
(defvar *regex-enabled* nil)
(handler-case
    (progn
      (if (find-package :quicklisp)
          (funcall (intern "QUICKLOAD" :ql) :cl-ppcre :silent t)
          (require 'cl-ppcre))
      (setf *regex-enabled* t)
      (format t "Regex support enabled (cl-ppcre loaded)~%"))
  (error (e)
    (format *error-output* "Regex support unavailable (cl-ppcre not found): ~A~%~
                            Install with: (ql:quickload :cl-ppcre)~%~
                            MATCHES and EXTRACT operators will be unavailable.~%" e)))

;;; Load database support (optional - uses sqlite3 CLI tool)
(defvar *db-enabled* nil)
(defvar *db-connections* (make-hash-table :test #'equal)
  "Hash table mapping connection names to database file paths")

(handler-case
    (progn
      ;; Check if sqlite3 command is available by trying to run it
      (let ((process (sb-ext:run-program "/usr/bin/sqlite3" '("--version")
                                        :output :stream
                                        :error :stream
                                        :wait t
                                        :search t)))
        (when process
          (let ((exit-code (sb-ext:process-exit-code process)))
            (when (and exit-code (zerop exit-code))
              (setf *db-enabled* t))))))
  (error (e)
    (format *error-output* "Database support unavailable (sqlite3 not found): ~A~%~
                            Install: sudo apt-get install sqlite3~%~
                            DB operations will be no-ops.~%" e)))

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

(defun replace-all (string old new)
  "Replace all occurrences of OLD with NEW in STRING."
  (let ((pos (search old string)))
    (if pos
        (replace-all 
         (concatenate 'string
                     (subseq string 0 pos)
                     new
                     (subseq string (+ pos (length old))))
         old new)
         string)))

;;; ============================================================================
;;; Enhanced Error Handling - LLM-friendly error messages
;;; ============================================================================

(defvar *current-file* nil "Current CNS file being interpreted")
(defvar *current-step* nil "Current step number being executed")
(defvar *strict-mode* nil "Enable strict mode with immediate NIL failures")
(defvar *max-iterations* 10000 "Maximum iterations before throwing error (prevents infinite loops)")
(defvar *iteration-counter* 0 "Current iteration count")

(defun make-cns-error (type message &key cause fix example context)
  "Create a structured, LLM-friendly error message.
   
   Arguments:
   - type: Error category (e.g., :variable-undefined, :expression-invalid, :control-flow)
   - message: Main error description
   - cause: Why this error occurred (optional)
   - fix: How to fix it (optional)
   - example: Working code example (optional)
   - context: Where the error occurred (file, step, line - optional)"
  (with-output-to-string (s)
    (format s "~%=== CNS ERROR ===~%")
    (format s "TYPE: ~A~%" type)
    (when context
      (format s "LOCATION: ~A~%" context))
    (when *current-file*
      (format s "FILE: ~A~%" *current-file*))
    (when *current-step*
      (format s "STEP: ~A~%" *current-step*))
    (format s "~%ERROR: ~A~%" message)
    (when cause
      (format s "~%CAUSE: ~A~%" cause))
    (when fix
      (format s "~%FIX: ~A~%" fix))
    (when example
      (format s "~%EXAMPLE:~%~A~%" example))
    (format s "~%===============~%")))

(defun cns-error-undefined-variable (var-name &optional expr)
  "Generate error for undefined variable."
  (make-cns-error 
   :variable-undefined
   (format nil "Variable '~A' is not defined" var-name)
   :cause "The variable has not been declared in the Given section or assigned in any previous step."
   :fix (format nil "1. Add '~A' to the Given section with a type and initial value~%   2. Or assign it in a previous step before using it" var-name)
   :example (format nil "Given:~%  ~A: Number = 0~%~%Step 1: Set ~A to 42~%  Then: Print ~A" var-name var-name var-name)
   :context (if expr (format nil "Expression: ~A" expr) nil)))

(defun cns-error-invalid-expression (expr &optional reason)
  "Generate error for invalid expression."
  (make-cns-error
   :expression-invalid
   (format nil "Cannot evaluate expression: ~A" expr)
   :cause (or reason "The expression syntax is not recognized or contains unsupported operations.")
   :fix "Check expression syntax:
   - Literal-first expressions fail: '3 * n' → use 'n * 3'
   - Multi-operator expressions fail: 'a + b * c' → split into steps
   - String comparisons need quotes: name = \"John\"
   - Variables need quotes in strings: Print \"Hello {name}\""
   :example "Working examples:
   result becomes n * 3
   temp becomes result + 1
   Print \"Value: {result}\""))

(defun cns-error-control-flow (clause &optional step-num)
  "Generate error for invalid control flow."
  (make-cns-error
   :control-flow-invalid
   (format nil "Invalid control flow: ~A" clause)
   :cause "Control flow statements (repeat from Step, go to Step, go to End) can only be used inside If/Otherwise branches."
   :fix "Move the control flow statement inside an If or Otherwise clause"
   :example (format nil "Step ~A: If n > 0~%  Then: repeat from Step 1~%  Otherwise: go to End" 
                    (or step-num "X"))))

(defun cns-error-nil-value (var-name expr)
  "Generate error for NIL value in strict mode."
  (make-cns-error
   :nil-value
   (format nil "Variable '~A' is NIL (strict mode)" var-name)
   :cause (format nil "Expression '~A' evaluated to NIL" expr)
   :fix "Check that:
   1. All variables in the expression are defined
   2. The expression syntax is correct
   3. Functions return expected values"
   :example "Given:
  x: Number = 10
  
Step 1: Set result to x * 2
  Then: Print result  # Will print 20"))

(defun cns-error-iteration-limit (iterations step-num env)
  "Generate error for exceeding iteration limit."
  (let ((state-snapshot (with-output-to-string (s)
                         (format s "State snapshot:~%")
                         (maphash (lambda (k v)
                                   (format s "    ~A = ~A~%" k (if (null v) "NIL" v)))
                                 env))))
    (make-cns-error
     :iteration-limit-exceeded
     (format nil "Iteration limit exceeded (~A iterations)" iterations)
     :cause "The program has been running for too long, likely due to an infinite loop"
     :fix (format nil "Common patterns:
   1. Variable became NIL, condition never met
   2. Loop condition always true
   3. Forgot to update loop counter
   
To increase limit: ./cns-run --max-iterations ~A yourfile.cns" 
                    (* iterations 5))
     :example state-snapshot
     :context (format nil "Currently at Step ~A" step-num))))

(defun format-error-context (step-num action)
  "Format context string for error location."
  (format nil "Step ~A: ~A" step-num action))

(defun check-strict-nil (var-name value expr step-num)
  "In strict mode, error on NIL values from expressions."
  (when (and *strict-mode* (null value) expr)
    (error (cns-error-nil-value var-name expr))))

(defun parse-json-value (json-str key)
  "Simple JSON parser to extract a value by key (LEGACY - use parse-json-full for new code).
   Handles: strings, numbers, booleans, null
   Example: {\"name\":\"John\",\"age\":30} with key 'name' -> 'John'"
  (handler-case
      (let* ((trimmed (trim json-str))
             ;; Find the key in JSON: "key":
             (key-pattern (format nil "\"~A\":" key))
             (key-pos (search key-pattern trimmed)))
        (if key-pos
            (let* ((value-start (+ key-pos (length key-pattern)))
                   (value-part (trim (subseq trimmed value-start)))
                   (first-char (char value-part 0)))
              (cond
                ;; String value: "value"
                ((char= first-char #\")
                 (let ((end-quote (position #\" value-part :start 1)))
                   (if end-quote
                       (subseq value-part 1 end-quote)
                       "")))
                ;; Boolean true
                ((starts-with value-part "true") t)
                ;; Boolean false
                ((starts-with value-part "false") nil)
                ;; Null
                ((starts-with value-part "null") nil)
                ;; Number
                (t (let ((end-pos (or (position #\, value-part)
                                     (position #\} value-part)
                                     (length value-part))))
                     (let ((num-str (trim (subseq value-part 0 end-pos))))
                       (handler-case
                           (parse-integer num-str)
                         (error () num-str)))))))
            ""))
    (error (e)
      (format *error-output* "JSON parse error: ~A~%" e)
      "")))

;;; ============================================================================
;;; Enhanced JSON Parser with Nested Objects, Arrays, and Dot Notation
;;; ============================================================================

(defun json-parse-string (str &optional (start 0))
  "Parse JSON string from position. Returns (values parsed-string new-position)."
  (let ((result (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
        (i (1+ start))
        (escaped nil))
    (loop while (< i (length str)) do
          (let ((ch (char str i)))
            (cond
              (escaped
               (case ch
                 (#\n (vector-push-extend #\Newline result))
                 (#\r (vector-push-extend #\Return result))
                 (#\t (vector-push-extend #\Tab result))
                 (#\" (vector-push-extend #\" result))
                 (#\\ (vector-push-extend #\\ result))
                 (#\/ (vector-push-extend #\/ result))
                 (t (vector-push-extend ch result)))
               (setf escaped nil))
              ((char= ch #\\) (setf escaped t))
              ((char= ch #\")
               ;; Use return-from to properly return from the function
               (return-from json-parse-string (values (coerce result 'string) (1+ i))))
              (t (vector-push-extend ch result)))
            (incf i)))
    (values (coerce result 'string) i)))

(defun json-skip-whitespace (str pos)
  "Skip whitespace and return new position."
  (loop while (and (< pos (length str))
                   (member (char str pos) '(#\Space #\Tab #\Newline #\Return)))
        do (incf pos))
  pos)

(defun json-parse-number (str start)
  "Parse JSON number. Returns (values number new-position)."
  (let ((end start))
    (loop while (and (< end (length str))
                     (or (digit-char-p (char str end))
                         (member (char str end) '(#\- #\+ #\. #\e #\E))))
          do (incf end))
    (let ((num-str (subseq str start end)))
      (values (if (position #\. num-str)
                  (read-from-string num-str)
                  (parse-integer num-str :junk-allowed t))
              end))))

(defun json-parse-value (str start)
  "Parse JSON value (string, number, boolean, null, object, array).
   Returns (values parsed-value new-position)."
  (let ((pos (json-skip-whitespace str start)))
    (when (>= pos (length str))
      (return-from json-parse-value (values nil pos)))
    
    (let ((ch (char str pos)))
      (cond
        ;; String
        ((char= ch #\")
         (json-parse-string str pos))
        
        ;; Object
        ((char= ch #\{)
         (let ((obj (make-hash-table :test 'equal))
               (pos (1+ pos)))
           (setf pos (json-skip-whitespace str pos))
           (when (and (< pos (length str)) (char= (char str pos) #\}))
             (return-from json-parse-value (values obj (1+ pos))))
           
           (loop
             ;; Parse key
             (setf pos (json-skip-whitespace str pos))
             (when (>= pos (length str))
               (error "Unexpected end of JSON in object"))
             (unless (char= (char str pos) #\")
               (error "Expected string key in object at position ~D, found: ~C" pos (char str pos)))
              (multiple-value-bind (key new-pos) (json-parse-string str pos)
                (setf pos (json-skip-whitespace str new-pos))
                (when (>= pos (length str))
                  (error "Unexpected end of JSON after key"))
                (unless (char= (char str pos) #\:)
                  (error "Expected : after key '~A' at position ~D, found: ~C" key pos (char str pos)))
               (setf pos (1+ pos))
               
               ;; Parse value
               (multiple-value-bind (value new-pos) (json-parse-value str pos)
                 (setf (gethash key obj) value)
                 (setf pos (json-skip-whitespace str new-pos))
                 
                 (when (>= pos (length str))
                   (error "Unexpected end of JSON in object"))
                 
                 ;; Check for comma or end
                 (cond
                   ((char= (char str pos) #\})
                    (return (values obj (1+ pos))))
                   ((char= (char str pos) #\,)
                    (setf pos (1+ pos)))
                   (t (error "Expected , or } in object at position ~D, found: ~C" pos (char str pos)))))))))
        
        ;; Array
        ((char= ch #\[)
         (let ((arr '())
               (pos (1+ pos)))
           (setf pos (json-skip-whitespace str pos))
           (when (char= (char str pos) #\])
             (return-from json-parse-value (values (nreverse arr) (1+ pos))))
           
           (loop
             (multiple-value-bind (value new-pos) (json-parse-value str pos)
               (push value arr)
               (setf pos (json-skip-whitespace str new-pos))
               
               (cond
                 ((char= (char str pos) #\])
                  (return (values (nreverse arr) (1+ pos))))
                 ((char= (char str pos) #\,)
                  (setf pos (1+ pos)))
                 (t (error "Expected , or ] in array")))))))
        
        ;; Boolean true
        ((and (<= (+ pos 4) (length str))
              (string= (subseq str pos (+ pos 4)) "true"))
         (values t (+ pos 4)))
        
        ;; Boolean false
        ((and (<= (+ pos 5) (length str))
              (string= (subseq str pos (+ pos 5)) "false"))
         (values nil (+ pos 5)))
        
        ;; Null
        ((and (<= (+ pos 4) (length str))
              (string= (subseq str pos (+ pos 4)) "null"))
         (values :null (+ pos 4)))
        
        ;; Number
        ((or (digit-char-p ch) (char= ch #\-))
         (json-parse-number str pos))
        
        (t (error "Unexpected character: ~A" ch))))))

(defun parse-json-full (json-str)
  "Parse JSON string into Lisp data structures.
   Objects -> hash tables
   Arrays -> lists
   Strings -> strings
   Numbers -> numbers
   Booleans -> t/nil
   Null -> :null
   
   Example: 
     (parse-json-full \"{\\\"name\\\":\\\"John\\\",\\\"age\\\":30}\")
     => #<HASH-TABLE {name: \"John\", age: 30}>"
  (handler-case
      (multiple-value-bind (value pos) (json-parse-value json-str 0)
        (declare (ignore pos))
        value)
    (error (e)
      (format *error-output* "JSON parse error: ~A~%" e)
      nil)))

(defun json-get-path (json-obj path)
  "Get value from JSON object using dot notation path.
   Supports:
     - Nested objects: \"user.profile.name\"
     - Array indexing: \"users[0].name\"
     - Mixed: \"data.items[2].title\"
   
   Example:
     (json-get-path obj \"user.profile.name\")
     (json-get-path obj \"items[0]\")"
  (handler-case
      (let ((current json-obj)
            (parts (split-string path #\.)))
        (dolist (part parts)
          (let* ((bracket-pos (position #\[ part))
                 (key (if bracket-pos (subseq part 0 bracket-pos) part))
                 (index (when bracket-pos
                          (let* ((end-bracket (position #\] part))
                                 (index-str (subseq part (1+ bracket-pos) end-bracket)))
                            (parse-integer index-str :junk-allowed t)))))
            
            ;; Get object property if key is not empty
            (when (and (> (length key) 0) (hash-table-p current))
              (setf current (gethash key current)))
            
            ;; Get array element if index specified
            (when (and index (listp current))
              (setf current (nth index current)))))
        
        current)
    (error (e)
      (format *error-output* "JSON path error: ~A~%" e)
      nil)))

(defun extract-quoted-string (str)
  "Extract a quoted string from str, handling escape sequences.
   Returns (values extracted-string rest-of-string).
   Supports: \\n \\r \\t \\\" \\\\
   Example: '\"Hello\\nWorld\" rest' -> 'Hello
World' and ' rest'"
  (let ((start (position #\" str)))
    (when start
      (let ((result (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
            (i (1+ start))
            (escaped nil))
        (loop while (< i (length str)) do
              (let ((ch (char str i)))
                 (cond
                  (escaped
                   (case ch
                     (#\n (vector-push-extend #\Newline result))
                     (#\r (vector-push-extend #\Return result))
                     (#\t (vector-push-extend #\Tab result))
                     (#\" (vector-push-extend #\" result))
                     (#\\ (vector-push-extend #\\ result))
                     (#\{ 
                      ;; Preserve as placeholder for substitute-vars
                      (vector-push-extend #\\ result)
                      (vector-push-extend #\{ result))
                     (#\} 
                      (vector-push-extend #\\ result)
                      (vector-push-extend #\} result))
                     (t (vector-push-extend ch result)))
                   (setf escaped nil))
                 ((char= ch #\\)
                  (setf escaped t))
                  ((char= ch #\")
                   ;; End of string
                   (return-from extract-quoted-string
                     (values (coerce result 'string) (subseq str (1+ i)))))
                  (t
                   (vector-push-extend ch result)))
                (incf i)))
        ;; If we get here, string was not closed
        (values (coerce result 'string) "")))))


(defun parse-query-string (query-string)
  "Parse URL query string into alist.
   Example: 'key1=val1&key2=val2' -> (('key1' . 'val1') ('key2' . 'val2'))"
  (when (and query-string (> (length query-string) 0))
    (let ((params '()))
      (dolist (pair (split-string query-string #\&))
        (let* ((kv (split-string pair #\=))
               (key (trim (car kv)))
               (value (if (cdr kv) (trim (cadr kv)) "")))
          (push (cons key value) params)))
      (nreverse params))))

(defun parse-http-headers (header-lines)
  "Parse HTTP headers from list of header lines.
   Returns alist of (header-name . header-value) pairs."
  (let ((headers '()))
    (dolist (line header-lines)
      (let ((colon-pos (position #\: line)))
        (when colon-pos
          (let ((name (string-downcase (trim (subseq line 0 colon-pos))))
                (value (trim (subseq line (1+ colon-pos)))))
            (push (cons name value) headers)))))
    (nreverse headers)))

(defun parse-http-request (request-string)
  "Parse HTTP request string into structured data with headers, query params, and body.
   Returns plist with:
   :method - HTTP method (GET, POST, etc.)
   :path - URL path without query string
   :query-params - Alist of query parameters
   :version - HTTP version
   :headers - Alist of headers
   :body - Request body (if present)"
  (let* ((lines (split-string request-string #\Newline))
         (request-line (car lines))
         (blank-line-pos (position-if (lambda (line) (string= (trim line) "")) 
                                       (cdr lines))))
    (when request-line
      (let* ((parts (split-string request-line #\Space))
             (method (if (>= (length parts) 1) (trim (car parts)) "GET"))
             (full-url (if (>= (length parts) 2) (trim (cadr parts)) "/"))
             (version (if (>= (length parts) 3) (trim (caddr parts)) "HTTP/1.1"))
             ;; Parse path and query string
             (query-pos (position #\? full-url))
             (path (if query-pos (subseq full-url 0 query-pos) full-url))
             (query-string (when query-pos (subseq full-url (1+ query-pos))))
             (query-params (parse-query-string query-string))
             ;; Parse headers (lines between request line and blank line)
             (header-lines (if blank-line-pos
                              (subseq (cdr lines) 0 blank-line-pos)
                              (cdr lines)))
             (headers (parse-http-headers header-lines))
             ;; Parse body (lines after blank line)
              (body (when blank-line-pos
                      (let ((body-lines (nthcdr (+ blank-line-pos 2) lines)))
                        (when body-lines
                          (format nil "~{~A~^~%~}" body-lines))))))
         (list :method method 
               :path path
               :query-params query-params
               :version version
               :headers headers
               :body body)))))

(defun match-route-pattern (pattern path)
  "Match a route pattern against a path.
   Pattern can contain:
   - Exact match: '/users'
   - Wildcard: '/users/*' matches '/users/123'
   - Named params: '/users/:id' matches '/users/123' and captures id=123
   Returns (values matched params-alist)"
  (cond
    ;; Exact match
    ((string= pattern path)
     (values t nil))
    
    ;; Wildcard match: /path/*
    ((and (> (length pattern) 2)
          (string= (subseq pattern (- (length pattern) 2)) "/*"))
     (let ((prefix (subseq pattern 0 (- (length pattern) 1))))
       (if (and (>= (length path) (length prefix))
                (string= (subseq path 0 (length prefix)) prefix))
           (values t nil)
           (values nil nil))))
    
    ;; Named parameter match: /path/:param
    ((position #\: pattern)
     (let ((pattern-parts (split-string pattern #\/))
           (path-parts (split-string path #\/))
           (params '())
           (matched t))
       (when (= (length pattern-parts) (length path-parts))
         (loop for pp in pattern-parts
               for pathp in path-parts
               do (cond
                    ;; Empty parts (leading /)
                    ((and (= (length pp) 0) (= (length pathp) 0))
                     nil)
                    ;; Named parameter
                    ((and (> (length pp) 0) (char= (char pp 0) #\:))
                     (push (cons (subseq pp 1) pathp) params))
                    ;; Exact match
                    ((string= pp pathp)
                     nil)
                    ;; No match
                    (t
                     (setf matched nil)
                     (return))))
         (if matched
             (values t (nreverse params))
             (values nil nil)))))
    
    ;; No match
    (t (values nil nil))))

(defun match-route (routes request-method request-path)
  "Find matching route in routes list with pattern support.
   Routes format: ((method pattern response) ...)
   Pattern can be exact path, wildcard, or contain named parameters.
   Returns (values response params-alist) or (nil nil) if not found."
  (dolist (route routes)
    (when (string-equal (car route) request-method)
      (multiple-value-bind (matched params)
          (match-route-pattern (cadr route) request-path)
        (when matched
          (return-from match-route (values (caddr route) params))))))
  (values nil nil))

;;; ============================================================================
;;; Real Socket Support (using sb-bsd-sockets)
;;; ============================================================================

(defun create-server-socket (port &key (reuse-address t))
  "Create and bind a TCP server socket to the given port.
   Returns the socket object."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream
                               :protocol :tcp)))
    (when reuse-address
      (setf (sb-bsd-sockets:sockopt-reuse-address socket) t))
    (sb-bsd-sockets:socket-bind socket #(0 0 0 0) port)
    (sb-bsd-sockets:socket-listen socket 5)
    socket))

(defun accept-connection (server-socket)
  "Accept a connection on the server socket.
   Returns (values client-socket client-stream)."
  (let ((client-socket (sb-bsd-sockets:socket-accept server-socket)))
    (values client-socket
            (sb-bsd-sockets:socket-make-stream client-socket
                                               :input t
                                               :output t
                                               :element-type 'character))))

(defun socket-send (stream data)
  "Send data through the socket stream."
  (write-string data stream)
  (finish-output stream))

(defun socket-receive (stream &key (timeout 5))
  "Receive data from socket stream with optional timeout.
   Returns the received string or nil on timeout."
  (handler-case
      (let ((line (read-line stream nil nil)))
        (when line
          ;; Read full HTTP request (until blank line)
          (with-output-to-string (result)
            (write-line line result)
            (loop for next-line = (read-line stream nil nil)
                  while (and next-line (not (string= (trim next-line) "")))
                  do (write-line next-line result)))))
    (end-of-file () nil)
    (error (e) 
      (format *error-output* "Socket receive error: ~A~%" e)
      nil)))

(defun close-socket (socket)
  "Close a socket."
  (when socket
    (ignore-errors
      (sb-bsd-sockets:socket-close socket))))

;;; ============================================================================
;;; HTTP Response Building
;;; ============================================================================

(defun build-http-response (status-code body &key (content-type "text/html") (headers nil))
  "Build a complete HTTP response.
   status-code: HTTP status code (200, 404, etc.)
   body: Response body content
   content-type: Content-Type header value
   headers: Additional headers as alist ((name . value) ...)
   Returns complete HTTP response string."
  (let ((status-text (case status-code
                      (200 "OK")
                      (201 "Created")
                      (204 "No Content")
                      (301 "Moved Permanently")
                      (302 "Found")
                      (400 "Bad Request")
                      (401 "Unauthorized")
                      (403 "Forbidden")
                      (404 "Not Found")
                      (500 "Internal Server Error")
                      (t "Unknown"))))
    (with-output-to-string (response)
      ;; Status line
      (format response "HTTP/1.1 ~D ~A~C~C" status-code status-text #\Return #\Newline)
      
      ;; Content-Type header
      (format response "Content-Type: ~A~C~C" content-type #\Return #\Newline)
      
      ;; Content-Length header
      (format response "Content-Length: ~D~C~C" (length body) #\Return #\Newline)
      
      ;; Additional headers
      (dolist (header headers)
        (format response "~A: ~A~C~C" (car header) (cdr header) #\Return #\Newline))
      
      ;; Blank line before body
      (format response "~C~C" #\Return #\Newline)
      
      ;; Body
      (write-string body response))))

(defun build-json-response (status-code json-data &key (headers nil))
  "Build HTTP response with JSON content.
   json-data: String containing JSON data
   Returns complete HTTP response with application/json content-type."
  (build-http-response status-code json-data 
                       :content-type "application/json"
                       :headers headers))

(defun build-error-response (status-code message)
  "Build HTML error response.
   Returns complete HTTP response with error HTML."
  (let ((html (format nil "<html><head><title>Error ~D</title></head><body><h1>~D ~A</h1><p>~A</p></body></html>"
                      status-code status-code
                      (case status-code
                        (400 "Bad Request")
                        (404 "Not Found")
                        (500 "Internal Server Error")
                        (t "Error"))
                      message)))
    (build-http-response status-code html)))

;;; ============================================================================
;;; HTTP Client (using raw sockets - zero dependencies!)
;;; ============================================================================

(defun parse-url (url)
  "Parse URL into components.
   Returns (values protocol host port path).
   Examples:
     http://example.com/path -> (http example.com 80 /path)
     https://api.co:443/v1 -> (https api.co 443 /v1)"
  (let* ((protocol-end (search "://" url))
         (protocol (if protocol-end
                      (subseq url 0 protocol-end)
                      "http"))
         (after-protocol (if protocol-end
                            (subseq url (+ protocol-end 3))
                            url))
         (slash-pos (position #\/ after-protocol))
         (host-port (if slash-pos
                       (subseq after-protocol 0 slash-pos)
                       after-protocol))
         (path (if slash-pos
                  (subseq after-protocol slash-pos)
                  "/"))
         (colon-pos (position #\: host-port))
         (host (if colon-pos
                  (subseq host-port 0 colon-pos)
                  host-port))
         (port (if colon-pos
                  (parse-integer (subseq host-port (1+ colon-pos)))
                  (if (string= protocol "https") 443 80))))
    (values protocol host port path)))

(defun http-request (url &key (method "GET") (body nil) (headers nil))
  "Make HTTP/HTTPS request to URL using raw sockets (HTTP) or CL+SSL (HTTPS).
   method: GET, POST, PUT, DELETE, etc.
   body: Request body (string)
   headers: Alist of additional headers ((name . value) ...)
   Returns (values status-code response-headers response-body).
   
   Example:
     (http-request \"https://api.example.com\" :method \"POST\" :body \"{\\\"key\\\":\\\"value\\\"}\")
   
   Note: HTTPS requires cl+ssl library. Falls back to HTTP if unavailable."
  (multiple-value-bind (protocol host port path)
      (parse-url url)
    (when (and (string= protocol "https") (not *https-enabled*))
      (format *error-output* "WARNING: HTTPS requested but cl+ssl not available, falling back to HTTP~%")
      (setf protocol "http")
      (setf port 80))
    
    (handler-case
        (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                     :type :stream
                                     :protocol :tcp)))
          (unwind-protect
               (progn
                 ;; Connect to server
                 (let ((host-ent (sb-bsd-sockets:get-host-by-name host)))
                   (sb-bsd-sockets:socket-connect socket 
                                                  (sb-bsd-sockets:host-ent-address host-ent)
                                                  port))
                 
                  ;; Create stream for reading/writing (with SSL if HTTPS)
                  (let ((stream (if (and (string= protocol "https") *https-enabled*)
                                    ;; HTTPS: wrap socket with SSL stream and flexi-stream for character I/O
                                    (let* ((binary-stream (sb-bsd-sockets:socket-make-stream socket
                                                                                :input t
                                                                                :output t
                                                                                :element-type '(unsigned-byte 8)))
                                           (ssl-stream (funcall (find-symbol "MAKE-SSL-CLIENT-STREAM" "CL+SSL")
                                                               binary-stream
                                                               :hostname host)))
                                      (funcall (find-symbol "MAKE-FLEXI-STREAM" "FLEXI-STREAMS")
                                               ssl-stream
                                               :external-format :utf-8))
                                    ;; HTTP: use plain socket stream
                                    (sb-bsd-sockets:socket-make-stream socket
                                                                       :input t
                                                                       :output t
                                                                       :element-type 'character))))
                   ;; Build HTTP request
                   (format stream "~A ~A HTTP/1.1~C~C" method path #\Return #\Newline)
                   (format stream "Host: ~A~C~C" host #\Return #\Newline)
                   (format stream "User-Agent: CNS/1.0~C~C" #\Return #\Newline)
                   (format stream "Accept: */*~C~C" #\Return #\Newline)
                   (format stream "Connection: close~C~C" #\Return #\Newline)
                   
                   ;; Add body length if present
                   (when body
                     (format stream "Content-Length: ~D~C~C" (length body) #\Return #\Newline)
                     (unless (assoc "Content-Type" headers :test #'string-equal)
                       (format stream "Content-Type: application/json~C~C" #\Return #\Newline)))
                   
                   ;; Add custom headers
                   (dolist (header headers)
                     (format stream "~A: ~A~C~C" (car header) (cdr header) #\Return #\Newline))
                   
                   ;; End headers
                   (format stream "~C~C" #\Return #\Newline)
                   
                   ;; Send body if present
                   (when body
                     (write-string body stream))
                   
                   (finish-output stream)
                   
                   ;; Read response
                   (let ((status-line (read-line stream nil nil))
                         (response-headers '())
                         (response-body nil))
                     
                     ;; Parse status line
                     (let* ((status-code 0)
                            (status-parts (when status-line (split-string status-line #\Space))))
                       (when (>= (length status-parts) 2)
                         (setf status-code (parse-integer (cadr status-parts) :junk-allowed t)))
                       
                       ;; Read headers
                       (loop for line = (read-line stream nil nil)
                             while (and line (not (string= (trim line) "")))
                             do (let ((colon-pos (position #\: line)))
                                  (when colon-pos
                                    (push (cons (trim (subseq line 0 colon-pos))
                                               (trim (subseq line (1+ colon-pos))))
                                          response-headers))))
                       
                       ;; Read body based on Content-Length or until connection closes
                       (let ((content-length-header (cdr (assoc "Content-Length" response-headers :test #'string-equal))))
                         (setf response-body
                               (if content-length-header
                                   ;; If Content-Length is specified, read exact number of chars
                                   (let* ((length (parse-integer content-length-header :junk-allowed t))
                                          (body-str (make-string length)))
                                     (read-sequence body-str stream)
                                     body-str)
                                   ;; Otherwise read until EOF (Connection: close)
                                   (with-output-to-string (out)
                                     (loop for ch = (read-char stream nil nil)
                                           while ch
                                           do (write-char ch out))))))
                       
                        (values status-code (nreverse response-headers) response-body)))))
            ;; Always close socket
            (close-socket socket)))
      (error (e)
        (format *error-output* "HTTP request error: ~A~%" e)
        (values 0 nil (format nil "ERROR: ~A" e))))))

;;; ============================================================================
;;; Database Helper Functions (SQLite via CLI)
;;; ============================================================================

(defun db-connect (db-name filepath)
  "Register a database connection."
  (when *db-enabled*
    (setf (gethash db-name *db-connections*) filepath)
    filepath))

(defun db-execute (db-name sql)
  "Execute SQL statement (INSERT, UPDATE, DELETE, CREATE, etc.) - no results returned."
  (unless *db-enabled*
    (return-from db-execute nil))
  
  (let ((filepath (gethash db-name *db-connections*)))
    (unless filepath
      (error "Database connection '~A' not found. Use DB CONNECT first." db-name))
    
    ;; Execute SQL using sqlite3 CLI with sb-ext:run-program
    (handler-case
        (let ((process (sb-ext:run-program "sqlite3" 
                                          (list filepath sql)
                                          :output :stream
                                          :error :stream
                                          :wait t
                                          :search t)))
          (when process
            (sb-ext:process-exit-code process)))
      (error (e)
        (format *error-output* "Database execute error: ~A~%" e)
        nil))))

(defun db-query (db-name sql)
  "Execute SQL query (SELECT) and return results as raw string (line format).
   Each row is separated by newline, format: column = value"
  (unless *db-enabled*
    (return-from db-query ""))
  
  (let ((filepath (gethash db-name *db-connections*)))
    (unless filepath
      (error "Database connection '~A' not found. Use DB CONNECT first." db-name))
    
    ;; Query with -line mode for easier parsing
    (handler-case
        (let ((process (sb-ext:run-program "sqlite3" 
                                          (list "-line" filepath sql)
                                          :output :stream
                                          :error :stream
                                          :wait t
                                          :search t)))
          (when process
            (with-output-to-string (result)
              (let ((stream (sb-ext:process-output process)))
                (loop for line = (read-line stream nil nil)
                      while line
                      do (format result "~A~%" line))))))
      (error (e)
        (format *error-output* "Database query error: ~A~%" e)
        ""))))

;;; ============================================================================
;;; CNSC: CNS Compact Format Support
;;; ============================================================================
;;; CNSC (Compact) Preprocessor
;;; ============================================================================

(defun contains-char (str ch)
  "Check if string contains character."
  (position ch str))

(defun expand-cnsc-to-cns (code)
  "Expand CNSC (compact) syntax to full CNS syntax.
   Transforms:
   - G: -> Given:
   - Sn→ -> Step n →
   - E: -> End:
   - var=expr -> Then: var becomes expr
   - cond? then : else -> If/Otherwise
   - ->Sn -> repeat from Step n
   - ->E -> go to End"
  (let ((lines (split-string code #\Newline))
        (result '()))
    (dolist (line lines)
      (let ((trimmed (trim line)))
        (cond
         ;; Story: (unchanged)
         ((starts-with trimmed "Story:")
          (push line result))
         
         ;; G: -> Given:
          ((starts-with trimmed "G:")
           (push "Given:" result)
           (let* ((vars-str (trim (subseq trimmed 2)))
                  (var-decls (split-string vars-str #\,)))
             (dolist (var-decl var-decls)
               (let* ((var-trimmed (trim var-decl))
                      (colon-pos (position #\: var-trimmed))
                      (equals-pos (position #\= var-trimmed)))
                 (when colon-pos
                   (let* ((name (subseq var-trimmed 0 colon-pos))
                          (type-abbr (if equals-pos
                                        (subseq var-trimmed (1+ colon-pos) equals-pos)
                                        (subseq var-trimmed (1+ colon-pos))))
                          (value (if equals-pos
                                    (subseq var-trimmed (1+ equals-pos))
                                    nil))
                          (type-full (cond
                                      ((string= type-abbr "I") "Integer")
                                      ((string= type-abbr "S") "String")
                                      ((string= type-abbr "L") "List")
                                      ((string= type-abbr "M") "Map")
                                      (t type-abbr))))
                     (if value
                         (push (format nil "  ~A: ~A = ~A" name type-full value) result)
                         (push (format nil "  ~A: ~A" name type-full) result))))))))
         
          ;; Sn→ -> Step n →
          ((and (>= (length trimmed) 3)
                (char= (char trimmed 0) #\S)
                (digit-char-p (char trimmed 1))
                (contains-char trimmed #\→))
           (let* ((arrow-pos (position #\→ trimmed))
                  (step-num (parse-integer (subseq trimmed 1 arrow-pos)))
                  (step-content (trim (subseq trimmed (1+ arrow-pos)))))
             ;; Parse step content for conditional or action
             (let ((question-pos (position #\? step-content))
                   (colon-pos (position #\: step-content)))
               (cond
                ;; Conditional: cond? then : else
                ((and question-pos colon-pos (< question-pos colon-pos))
                 (let* ((condition (trim (subseq step-content 0 question-pos)))
                        (then-part (trim (subseq step-content (1+ question-pos) colon-pos)))
                        (else-part (trim (subseq step-content (1+ colon-pos)))))
                   ;; Step header with IF condition
                   (push (format nil "Step ~D → If ~A" step-num condition) result)
                   (push "  Because: execution step" result)
                   ;; Expand then part
                   (cond
                    ((starts-with then-part "->S")
                     (let ((target-step (parse-integer (subseq then-part 3))))
                       (push (format nil "  Then: repeat from Step ~D" target-step) result)))
                    ((starts-with then-part "->E")
                     (push "  Then: go to End" result))
                    ((contains-char then-part #\=)
                     (let* ((eq-pos (position #\= then-part))
                            (var (trim (subseq then-part 0 eq-pos)))
                            (expr (trim (subseq then-part (1+ eq-pos)))))
                       (push (format nil "  Then: ~A becomes ~A" var expr) result)))
                    (t
                     (push (format nil "  Then: ~A" then-part) result)))
                   ;; Expand else part
                   (cond
                    ((starts-with else-part "->S")
                     (let ((target-step (parse-integer (subseq else-part 3))))
                       ;; Use "go to Step" for forward jumps in Otherwise clause
                       (push (format nil "  Otherwise: go to Step ~D" target-step) result)))
                    ((starts-with else-part "->E")
                     (push "  Otherwise: go to End" result))
                    ((contains-char else-part #\=)
                     (let* ((eq-pos (position #\= else-part))
                            (var (trim (subseq else-part 0 eq-pos)))
                            (expr (trim (subseq else-part (1+ eq-pos)))))
                       (push (format nil "  Otherwise: ~A becomes ~A" var expr) result)))
                    (t
                     (push (format nil "  Otherwise: ~A" else-part) result)))))
               
               ;; Effect: statements
               ((starts-with step-content "Effect:")
                (let ((effect-content (trim (subseq step-content 7))))
                  (push (format nil "Step ~D → Execute effect" step-num) result)
                  (push "  Because: execution step" result)
                  (push (format nil "  Effect: ~A" effect-content) result)))
               
               ;; Simple assignment(s): var=expr or var=expr; var2=expr2
               ((contains-char step-content #\=)
                (push (format nil "Step ~D → Execute assignments" step-num) result)
                (push "  Because: execution step" result)
                (let ((assignments (split-string step-content #\;)))
                  (dolist (assignment assignments)
                    (let* ((assign-trimmed (trim assignment))
                           (eq-pos (position #\= assign-trimmed)))
                      (when eq-pos
                        (let ((var (trim (subseq assign-trimmed 0 eq-pos)))
                              (expr (trim (subseq assign-trimmed (1+ eq-pos)))))
                          (push (format nil "  Then: ~A becomes ~A" var expr) result)))))))
               
               ;; Jump: ->Sn or ->E
               ((starts-with step-content "->")
                (push (format nil "Step ~D → Jump" step-num) result)
                (push "  Because: execution step" result)
                (cond
                 ((starts-with step-content "->S")
                  (let ((target-step (parse-integer (subseq step-content 3))))
                    (push (format nil "  Then: repeat from Step ~D" target-step) result)))
                 ((starts-with step-content "->E")
                  (push "  Then: go to End" result))))
               
               ;; Unknown - pass through
               (t
                (push (format nil "Step ~D → ~A" step-num step-content) result)
                (push "  Because: execution step" result)
                (push (format nil "  Then: ~A" step-content) result))))))
         
         ;; E: -> End:
         ((starts-with trimmed "E:")
          (let ((return-val (trim (subseq trimmed 2))))
            (push (format nil "End: Return ~A" return-val) result)))
         
         ;; Empty line
         ((emptyp trimmed)
          (push "" result))
         
         ;; Unknown - pass through
         (t
          (push line result)))))
     (format nil "~{~A~%~}" (nreverse result))))

;;; ============================================================================
;;; Function Registry - Support for reusable stories
;;; ============================================================================

(defvar *function-registry* (make-hash-table :test #'equal)
  "Global registry of function definitions.
   Maps function name -> AST of the function's story.")

(defun register-function (name ast)
  "Register a function definition in the global registry."
  (setf (gethash name *function-registry*) ast))

(defun get-function (name)
  "Retrieve a function definition from the registry."
  (gethash name *function-registry*))

(defun clear-functions ()
  "Clear all function definitions (useful for testing)."
  (clrhash *function-registry*))

(defun is-function-story (story-line)
  "Check if a story line declares a function.
   Returns (values is-function function-name)"
  (let ((trimmed (trim story-line)))
    (when (starts-with trimmed "Story:")
      (let ((rest (trim (subseq trimmed 6))))
        (if (search "(function)" rest :test #'char-equal)
            (let ((paren-pos (search "(" rest)))
              (values t (trim (subseq rest 0 paren-pos))))
            (values nil rest))))))

(defun split-multi-story-code (code)
  "Split code by --- separator into multiple stories.
   Returns list of code strings, one per story."
  (let ((separator "---")
        (result '())
        (current '())
        (lines (split-string code #\Newline)))
    (dolist (line lines)
      (if (string= (trim line) separator)
          (progn
            (when current
              (push (format nil "~{~A~^~%~}" (nreverse current)) result))
            (setf current '()))
          (push line current)))
    ;; Don't forget last story
    (when current
      (push (format nil "~{~A~^~%~}" (nreverse current)) result))
    (nreverse result)))

;;; ============================================================================
;;; Parser: Convert CNS string to S-expression AST
;;; ============================================================================

(defun is-cnsc-code (code)
  "Detect if code is in CNSC (compact) format.
   Checks for CNSC markers: G:, Sn→, E:"
  (or (search "G:" code)
      (and (search "S1→" code) (not (search "Step 1" code)))
      (and (search "E:" code) (not (search "End:" code)))))

(defun parse-single-cns (code)
  "Parse a single CNS story into AST.
   This is the core parser logic."
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
          (let* ((story-text (trim (subseq trimmed 6)))
                 (has-strict (search "[strict]" story-text :test #'char-equal))
                 (story-name (if has-strict
                                (trim (substitute #\Space #\] (substitute #\Space #\[ story-text)))
                                story-text)))
            (if has-strict
                (push `(story ,story-name :strict t) ast)
                (push `(story ,story-name) ast)))
          (setf current-section :story))
         
         ;; Given section (variable declarations)
         ((starts-with trimmed "Given:")
          (setf current-section :given)
          (push (list 'given) ast))
         
          ;; Variable declaration in Given section
          ((and (eql current-section :given) indented)
           (let* ((first-colon (position #\: trimmed))
                  (name (if first-colon (trim (subseq trimmed 0 first-colon)) trimmed))
                  (rest-str (if first-colon (trim (subseq trimmed (1+ first-colon))) nil)))
             (when rest-str
               (let* ((type-and-value rest-str)
                      ;; Split by = to separate type/value (only first =)
                      (first-equals (position #\= type-and-value))
                      (type-part (if first-equals 
                                    (trim (subseq type-and-value 0 first-equals))
                                    type-and-value))
                      (val-part (if first-equals 
                                   (trim (subseq type-and-value (1+ first-equals)))
                                   nil))
                      ;; Now check type-part for semantic tag [...] (NOT in val-part!)
                      (bracket-start (position #\[ type-part))
                      (type (if bracket-start
                               (trim (subseq type-part 0 bracket-start))
                               type-part))
                      (tag (if bracket-start
                              (let ((bracket-end (position #\] type-part :start bracket-start)))
                                (when bracket-end
                                  (subseq type-part (1+ bracket-start) bracket-end)))
                              nil))
                      (val (if val-part
                              ;; Keep value as-is (with quotes if present)
                              ;; eval-expr will handle parsing correctly
                              val-part
                              nil)))
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
            ;; Check step type: If, For each, or regular action
            (cond
             ;; Conditional step
             ((starts-with (string-upcase step-content) "IF")
              (setf current-step (list (list 'if (trim (subseq step-content 2))) step-id 'step)))
             ;; For each loop
             ((starts-with (string-upcase step-content) "FOR EACH")
              (let ((for-each-content (trim (subseq step-content 8))))
                (setf current-step (list (list 'for-each for-each-content) step-id 'step))))
             ;; Regular action
             (t
              (setf current-step (list (list 'action step-content) step-id 'step))))))
         
         ;; Because clause (causality explanation)
         ((and current-step indented (starts-with trimmed "Because:"))
          (push `(because ,(trim (subseq trimmed 8))) current-step))
         
          ;; Effect declaration
          ((and current-step indented (starts-with trimmed "Effect:"))
           (let ((effect-str (trim (subseq trimmed 7))))
              ;; Keep effect string as-is - let apply-effect handle quote extraction
              (push `(effect ,effect-str) current-step)))
         
          ;; Then clause (state transformation)
          ((and current-step indented (starts-with trimmed "Then:"))
           (push `(then ,(trim (subseq trimmed 5))) current-step))
         
         ;; Otherwise clause
         ((and current-step indented (starts-with trimmed "Otherwise:"))
          (push `(otherwise ,(trim (subseq trimmed 10))) current-step))
         
          ;; If conditional
          ((and current-step indented (starts-with trimmed "If"))
           (let* ((after-if (subseq trimmed 2))
                  (cond-str (if (and (> (length after-if) 0) (char= (char after-if 0) #\:))
                               (trim (subseq after-if 1))
                               (trim after-if))))
             (push `(if ,cond-str) current-step)))
          
          ;; Error section
          ((starts-with trimmed "Error:")
           ;; Finish previous step if any
           (when current-step
             (push (nreverse current-step) ast))
           (setf current-section :error)
           (setf current-step nil)
           (push (list 'error) ast))
          
          ;; Error section content (Return, Effect, Because)
          ((and (eql current-section :error) indented)
           (cond
            ((starts-with trimmed "Return")
             (let ((return-value (trim (subseq trimmed 6))))
               (setf (cdar ast) (append (cdar ast) (list `(return ,return-value))))))
              ((starts-with trimmed "Effect:")
               (let ((effect-str (trim (subseq trimmed 7))))
                  ;; Keep as-is - apply-effect will handle quote extraction
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
                  (return-value (if (and (> (length end-content) 0)
                                         (starts-with (string-upcase end-content) "RETURN"))
                                    (trim (subseq end-content 6))
                                    (if (> (length end-content) 0)
                                        end-content
                                        nil))))
             (push `(end (return ,return-value) (because "computation complete")) ast)))
          
          ;; End section content (Return, Effect, Because) - handle separate lines (with or without indentation)
          ((and (eql current-section :end))
           (cond
            ((starts-with trimmed "Return:")
             (let ((return-value (trim (subseq trimmed 7))))
               ;; Update the return value in the end node
               (when (and ast (eq (caar ast) 'end))
                 (setf (cadr (car ast)) `(return ,return-value)))))
            ((starts-with trimmed "Effect:")
             (let ((effect-str (trim (subseq trimmed 7))))
               (when (and ast (eq (caar ast) 'end))
                 (setf (cdar ast) (append (cdar ast) (list `(effect ,effect-str)))))))
            ((starts-with trimmed "Because:")
             (let ((because-str (trim (subseq trimmed 8))))
               ;; Update the because value in the end node
               (when (and ast (eq (caar ast) 'end))
                 (let ((end-node (car ast)))
                   (setf (caddr end-node) `(because ,because-str)))))))))))
     ;; Finish last step if any
     (when current-step
       (push (nreverse current-step) ast))
     (nreverse ast)))

(defun parse-cns (code)
  "Parse CNS code with support for multiple stories.
   Returns list of ASTs if multiple stories, single AST otherwise.
   Automatically detects and expands CNSC (compact) format."
  (let ((expanded-code (if (is-cnsc-code code)
                           (expand-cnsc-to-cns code)
                           code)))
    ;; Check if multi-story code (contains ---)
    (if (search "---" expanded-code)
        ;; Multiple stories - split and parse each
        (mapcar #'parse-single-cns (split-multi-story-code expanded-code))
        ;; Single story - parse and return as-is
        (parse-single-cns expanded-code))))

;;; ============================================================================
;;; Function Calling Mechanism
;;; ============================================================================

;; Forward declarations (these functions are defined later)
(declaim (ftype (function (t t &optional t) t) eval-expr))
(declaim (ftype (function (t t t) t) apply-effect))

(defun detect-function-call (expr)
  "Detect if expression is a function call: FuncName(arg1, arg2).
   Returns (values is-call function-name args-string)
   Example: 'Add(5, 10)' -> (t 'Add' '5, 10')"
  (when (stringp expr)
    (let ((trimmed (trim expr)))
      (let ((paren-open (position #\( trimmed))
            (paren-close (position #\) trimmed :from-end t)))
        (when (and paren-open paren-close 
                   (> paren-close paren-open)
                   (> paren-open 0))
          (let ((func-name (trim (subseq trimmed 0 paren-open)))
                (args-str (trim (subseq trimmed (1+ paren-open) paren-close))))
            ;; Verify function name looks valid (starts with letter, contains no operators)
            (when (and (> (length func-name) 0)
                       (alpha-char-p (char func-name 0))
                       ;; Function name must not contain operators
                       (not (find-if (lambda (ch) (member ch '(#\+ #\- #\* #\/ #\% #\= #\< #\> #\Space)))
                                    func-name)))
              (values t func-name args-str))))))))

(defun call-function (func-name args-list &key (verbose nil))
  "Call a function with given arguments.
   args-list: list of evaluated argument values
   Returns the function's return value."
  (let ((func-ast (get-function func-name)))
    (unless func-ast
      (error "Function ~A not defined" func-name))
    
    (when verbose
      (format t "~%  [Calling function: ~A(~{~A~^, ~})]~%" func-name args-list))
    
    ;; Create new environment for function
    (let ((func-env (make-hash-table :test #'equal))
          (steps '())
          (result nil)
          (error-block nil)
          (param-index 0))
      
      ;; Phase 1: Collect sections and bind parameters
      (dolist (node func-ast)
        (case (car node)
          (story nil)  ; Skip story header
          
          (given 
           ;; Process variables: first N are parameters, rest are locals
           (dolist (var (cdr node))
             (let ((name (cadr var))
                   (type (caddr var))
                   (val (cadddr var)))
               ;; If we have args remaining, use them as param values
               (if (< param-index (length args-list))
                   (progn
                     (setf (gethash name func-env) (nth param-index args-list))
                     (incf param-index)
                     (when verbose
                       (format t "    Param ~A = ~A~%" name (gethash name func-env))))
                   ;; Local variable - initialize normally
                   (progn
                     (setf (gethash name func-env) 
                           (if val (eval-expr val func-env) nil))
                     (when verbose
                       (format t "    Local ~A = ~A~%" name (gethash name func-env))))))))
          
          (step (push node steps))
          (error (setf error-block node))
          (end (setf result node))))
      
      (setf steps (nreverse steps))
      
      ;; Phase 2: Execute steps (simplified interpreter loop)
      (let ((pc 0))
        (handler-case
            (loop while (< pc (length steps)) do
                  ;; Check iteration limit
                  (incf *iteration-counter*)
                  (when (> *iteration-counter* *max-iterations*)
                    (error (cns-error-iteration-limit *iteration-counter* 
                                                     (if (< pc (length steps)) 
                                                         (cadr (nth pc steps))
                                                         "Unknown")
                                                     func-env)))
                  (let* ((step (nth pc steps))
                         (step-num (cadr step))
                         (step-body (cddr step))
                         (if-node (assoc 'if step-body))
                         (otherwise-pos (position 'otherwise step-body :key #'car))
                         ;; Split step-body into if-branch and otherwise-branch
                         (if-branch-body (if otherwise-pos
                                           (subseq step-body 0 otherwise-pos)
                                           step-body))
                         (otherwise-branch-body (when otherwise-pos
                                                 (subseq step-body (1+ otherwise-pos))))
                         ;; Extract clauses from appropriate branches
                         (then-clauses (mapcar #'cadr (remove-if-not (lambda (x) (eq (car x) 'then)) 
                                                                     (if if-node if-branch-body step-body))))
                         (effects (mapcar #'cadr (remove-if-not (lambda (x) (eq (car x) 'effect)) 
                                                                (if if-node if-branch-body step-body))))
                         (otherwise-then-clauses (mapcar #'cadr (remove-if-not (lambda (x) (eq (car x) 'then)) 
                                                                               otherwise-branch-body)))
                         (otherwise-effects (mapcar #'cadr (remove-if-not (lambda (x) (eq (car x) 'effect)) 
                                                                          otherwise-branch-body)))
                         (otherwise-clause (cadr (assoc 'otherwise step-body))))
                    
                    ;; Handle conditional
                    (cond
                     (if-node
                      (let ((cond-expr (cadr if-node)))
                        (if (eval-expr cond-expr func-env)
                            ;; True branch
                            (progn
                              (dolist (then-clause then-clauses)
                                (cond
                                 ((search "repeat from Step" then-clause :test #'char-equal)
                                  (let* ((step-pos (search "Step " then-clause))
                                         (num-start (+ step-pos 5))
                                         (target-step (parse-integer then-clause :start num-start :junk-allowed t)))
                                    (setf pc (1- target-step))))
                                 ((search "go to Step" then-clause :test #'char-equal)
                                  (let* ((step-pos (search "Step " then-clause))
                                         (num-start (+ step-pos 5))
                                         (target-step (parse-integer then-clause :start num-start :junk-allowed t)))
                                    (setf pc (1- target-step))))
                                 ((search "go to End" then-clause :test #'char-equal)
                                  (return))
                                 ;; Check if this is actually an effect keyword (PRINT, HTTP, etc.)
                                 ((or (starts-with (string-upcase (trim then-clause)) "PRINT ")
                                      (starts-with (string-upcase (trim then-clause)) "HTTP ")
                                      (starts-with (string-upcase (trim then-clause)) "HTTPS ")
                                      (starts-with (string-upcase (trim then-clause)) "SHELL ")
                                      (starts-with (string-upcase (trim then-clause)) "FIND ")
                                      (starts-with (string-upcase (trim then-clause)) "GREP ")
                                      (starts-with (string-upcase (trim then-clause)) "GIT ")
                                      (starts-with (string-upcase (trim then-clause)) "CSV ")
                                      (starts-with (string-upcase (trim then-clause)) "SQL ")
                                      (starts-with (string-upcase (trim then-clause)) "SOCKET "))
                                  (apply-effect then-clause func-env verbose))
                                 (t
                                  (eval-expr then-clause func-env))))
                              
                              ;; Execute effects for true branch
                              (dolist (eff effects)
                                (apply-effect eff func-env verbose))
                              
                              (unless (or (search "repeat from" (car (last then-clauses)) :test #'char-equal)
                                          (search "go to" (car (last then-clauses)) :test #'char-equal))
                                (incf pc)))
                             ;; False branch - handle otherwise
                             (cond
                               ((and otherwise-clause (search "go to End" otherwise-clause :test #'char-equal))
                                (return))
                               ((and otherwise-clause (search "go to Step" otherwise-clause :test #'char-equal))
                                (let* ((step-pos (search "Step " otherwise-clause))
                                       (num-start (+ step-pos 5))
                                       (target-step (parse-integer otherwise-clause :start num-start :junk-allowed t)))
                                  (setf pc (1- target-step))))
                               ((and otherwise-clause (search "repeat from Step" otherwise-clause :test #'char-equal))
                                (let* ((step-pos (search "Step " otherwise-clause))
                                       (num-start (+ step-pos 5))
                                       (target-step (parse-integer otherwise-clause :start num-start :junk-allowed t)))
                                  (setf pc (1- target-step))))
                               (t
                                ;; Execute otherwise-branch Then and Effect clauses
                                (dolist (then-clause otherwise-then-clauses)
                                  (cond
                                   ((search "repeat from Step" then-clause :test #'char-equal)
                                    (let* ((step-pos (search "Step " then-clause))
                                           (num-start (+ step-pos 5))
                                           (target-step (parse-integer then-clause :start num-start :junk-allowed t)))
                                      (setf pc (1- target-step))
                                      (return)))
                                   ((search "go to Step" then-clause :test #'char-equal)
                                    (let* ((step-pos (search "Step " then-clause))
                                           (num-start (+ step-pos 5))
                                           (target-step (parse-integer then-clause :start num-start :junk-allowed t)))
                                      (setf pc (1- target-step))
                                      (return)))
                                   ((search "go to End" then-clause :test #'char-equal)
                                    (return))
                                   ;; Check if this is actually an effect keyword
                                   ((or (starts-with (string-upcase (trim then-clause)) "PRINT ")
                                        (starts-with (string-upcase (trim then-clause)) "HTTP ")
                                        (starts-with (string-upcase (trim then-clause)) "HTTPS ")
                                        (starts-with (string-upcase (trim then-clause)) "SHELL ")
                                        (starts-with (string-upcase (trim then-clause)) "FIND ")
                                        (starts-with (string-upcase (trim then-clause)) "GREP ")
                                        (starts-with (string-upcase (trim then-clause)) "GIT ")
                                        (starts-with (string-upcase (trim then-clause)) "CSV ")
                                        (starts-with (string-upcase (trim then-clause)) "SQL ")
                                        (starts-with (string-upcase (trim then-clause)) "SOCKET "))
                                    (apply-effect then-clause func-env verbose))
                                   (t
                                    (eval-expr then-clause func-env))))
                                (dolist (eff otherwise-effects)
                                  (apply-effect eff func-env verbose))
                                ;; Check if last then-clause had control flow
                                (unless (and otherwise-then-clauses
                                           (or (search "repeat from" (car (last otherwise-then-clauses)) :test #'char-equal)
                                               (search "go to" (car (last otherwise-then-clauses)) :test #'char-equal)))
                                  (incf pc)))))))
                     
                     ;; Regular step
                     (t
                      (dolist (then-clause then-clauses)
                        ;; Check if this is actually an effect keyword
                        (if (or (starts-with (string-upcase (trim then-clause)) "PRINT ")
                                (starts-with (string-upcase (trim then-clause)) "HTTP ")
                                (starts-with (string-upcase (trim then-clause)) "HTTPS ")
                                (starts-with (string-upcase (trim then-clause)) "SHELL ")
                                (starts-with (string-upcase (trim then-clause)) "FIND ")
                                (starts-with (string-upcase (trim then-clause)) "GREP ")
                                (starts-with (string-upcase (trim then-clause)) "GIT ")
                                (starts-with (string-upcase (trim then-clause)) "CSV ")
                                (starts-with (string-upcase (trim then-clause)) "SQL ")
                                (starts-with (string-upcase (trim then-clause)) "SOCKET "))
                            (apply-effect then-clause func-env verbose)
                            (eval-expr then-clause func-env)))
                      (dolist (eff effects)
                        (apply-effect eff func-env verbose))
                      (incf pc)))))
          
          (error (e)
            (when error-block
              (let ((error-return (cadr (assoc 'return (cdr error-block)))))
                (when error-return
                  (setf result (eval-expr error-return func-env)))))
            (unless error-block
              (error "Error in function ~A: ~A" func-name e)))))
      
      ;; Phase 3: Return result
      (when result
        (let ((return-expr (cadr (assoc 'return (cdr result)))))
          (setf result (eval-expr return-expr func-env))
          (when verbose
            (format t "    -> Returns: ~A~%" result))
          result)))))

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
              ;; String literal: "hello" (check BEFORE variable lookup!)
              ;; But NOT if it contains operators - check for unescaped quote in middle
              ((and (> (length trimmed) 1)
                    (char= (char trimmed 0) #\")
                    (char= (char trimmed (1- (length trimmed))) #\")
                    ;; Make sure there's no unescaped quote in the middle (which would mean it's "str" = "str")
                    ;; Check for unescaped quotes by looking for \" that's not preceded by \
                    (not (loop for i from 1 below (1- (length trimmed))
                              when (and (char= (char trimmed i) #\")
                                       (or (= i 0) 
                                           (char/= (char trimmed (1- i)) #\\)))
                              return t)))
               (let ((raw-str (subseq trimmed 1 (1- (length trimmed)))))
                 ;; Process escape sequences: \\n -> \n, \\t -> \t, \\r -> \r, \\\\ -> \\
                 (let ((result (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
                       (i 0))
                   (loop while (< i (length raw-str))
                         do (if (and (< (+ i 1) (length raw-str))
                                    (char= (char raw-str i) #\\))
                                (let ((next-char (char raw-str (1+ i))))
                                  (case next-char
                                    (#\n (vector-push-extend #\Newline result))
                                    (#\t (vector-push-extend #\Tab result))
                                    (#\r (vector-push-extend #\Return result))
                                    (#\\ (vector-push-extend #\\ result))
                                    (#\" (vector-push-extend #\" result))  ;; Handle escaped quotes
                                    (t (progn
                                         ;; For any other escape like \d, \s, etc., keep single backslash
                                         (vector-push-extend #\\ result)
                                         (vector-push-extend next-char result))))
                                  (incf i 2))
                                (progn
                                  (vector-push-extend (char raw-str i) result)
                                  (incf i))))
                   (coerce result 'string))))
              
              ;; Environment variable: ENV("VAR_NAME") or ENV("VAR_NAME", "default") (MUST come before function call!)
              ((starts-with (string-upcase trimmed) "ENV(")
               (let* ((rest (subseq trimmed 4))
                      (close-paren (position #\) rest :from-end t))
                      (args-str (if close-paren (subseq rest 0 close-paren) rest))
                      (args (split-string args-str #\,))
                      (var-name-expr (trim (car args)))
                      (default-expr (when (cdr args) (trim (cadr args))))
                      ;; Evaluate and remove quotes from var name
                      (var-name-raw (eval-expr var-name-expr env))
                      ;; Coerce to simple-string for posix-getenv (handles VECTOR CHARACTER type from eval-expr)
                      (var-name (coerce (if (stringp var-name-raw) var-name-raw var-name-expr) 'simple-string))
                      ;; Evaluate default value if present
                      (default-val (when default-expr (eval-expr default-expr env)))
                      ;; Get environment variable
                      (env-val (sb-ext:posix-getenv var-name)))
                 (if env-val
                     env-val
                     (or default-val ""))))
              
              ;; Date/Time: NOW() - returns current universal time
              ((string= (string-upcase trimmed) "NOW()")
               (get-universal-time))
              
              ;; Date/Time: TIMESTAMP() - returns ISO 8601 formatted current time
              ((string= (string-upcase trimmed) "TIMESTAMP()")
               (multiple-value-bind (sec min hr day mon yr)
                   (decode-universal-time (get-universal-time))
                 (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D" 
                        yr mon day hr min sec)))
              
              ;; Function call: FuncName(arg1, arg2, ...) (check BEFORE variable lookup BUT NOT if it contains "becomes")
              ((and (not (search "becomes" trimmed))  ; Not a becomes statement
                    (multiple-value-bind (is-call func-name args-str)
                        (detect-function-call trimmed)
                      (when is-call
                        (let* ((arg-exprs (if (and args-str (> (length args-str) 0))
                                             (split-string args-str #\,)
                                             '()))
                               (arg-values (mapcar (lambda (arg) (eval-expr (trim arg) env)) arg-exprs)))
                          (return-from eval-expr (call-function func-name arg-values :verbose (and context t))))))))
              
              ;; Variable lookup (use multiple-value-bind to check existence)
              ((multiple-value-bind (value exists) (gethash trimmed env)
                 (when exists value)))
            
            ;; List literal: [1, 2, 3] or []
            ((and (> (length trimmed) 1)
                  (char= (char trimmed 0) #\[)
                  (char= (char trimmed (1- (length trimmed))) #\]))
             (let* ((content (trim (subseq trimmed 1 (1- (length trimmed))))))
               ;; Empty list: []
               (if (zerop (length content))
                   '()
                   ;; Non-empty list
                   (let ((items (split-string content #\,)))
                     (mapcar (lambda (item) (eval-expr (trim item) env)) items)))))
            
             ;; Try to parse as number (handles negative numbers)
             ((and (> (length trimmed) 0)
                   (or (digit-char-p (char trimmed 0))
                       (and (char= (char trimmed 0) #\-)
                            (> (length trimmed) 1))))
              (handler-case
                  (parse-integer trimmed)
                (error () nil)))
           
           ;; Comparison: n ≤ 1 (less than or equal, Unicode) - BEFORE < check
             ((search "≤" trimmed)
              (let ((parts (split-string trimmed #\≤)))
                (<= (eval-expr (trim (car parts)) env)
                    (eval-expr (trim (cadr parts)) env))))
             
             ;; Comparison: n <= 1 (less than or equal, ASCII) - BEFORE < check
             ((search "<=" trimmed)
              (let* ((pos (search "<=" trimmed))
                     (left (subseq trimmed 0 pos))
                     (right (subseq trimmed (+ pos 2))))
                (<= (eval-expr (trim left) env)
                    (eval-expr (trim right) env))))
             
             ;; Comparison: n ≥ 1 (greater than or equal, Unicode) - BEFORE > check
             ((search "≥" trimmed)
              (let ((parts (split-string trimmed #\≥)))
                (>= (eval-expr (trim (car parts)) env)
                    (eval-expr (trim (cadr parts)) env))))
             
             ;; Comparison: n >= 1 (greater than or equal, ASCII) - BEFORE > check
             ((search ">=" trimmed)
              (let* ((pos (search ">=" trimmed))
                     (left (subseq trimmed 0 pos))
                     (right (subseq trimmed (+ pos 2))))
                (>= (eval-expr (trim left) env)
                    (eval-expr (trim right) env))))
           
           ;; Comparison: n > 1 (must come AFTER >= check)
            ((position #\> trimmed)
             (let ((parts (split-string trimmed #\>)))
               (> (eval-expr (trim (car parts)) env)
                  (eval-expr (trim (cadr parts)) env))))
            
            ;; Comparison: n < 1 (must come AFTER <= check)
            ((position #\< trimmed)
             (let ((parts (split-string trimmed #\<)))
               (< (eval-expr (trim (car parts)) env)
                  (eval-expr (trim (cadr parts)) env))))
            
              ;; Comparison: n ≠ 1 (not equal)
              ;; Handles both numeric and boolean comparisons
              ((search "≠" trimmed)
               (let* ((parts (split-string trimmed #\≠))
                      (left-val (eval-expr (trim (car parts)) env))
                      (right-val (eval-expr (trim (cadr parts)) env)))
                 ;; If both values are numbers, use numeric inequality
                 ;; Otherwise use general inequality (for booleans, strings, etc.)
                 (if (and (numberp left-val) (numberp right-val))
                     (/= left-val right-val)
                     (not (equal left-val right-val)))))
            
             ;; Comparison: n = 1 (must come after ≠, ≤, ≥, <=, >=)
             ;; Make sure = is not part of <=, >=, "becomes", " AND ", or " OR "
             ;; Handles both numeric and boolean comparisons
             ((and (position #\= trimmed)
                   (not (search "<=" trimmed))
                   (not (search ">=" trimmed))
                   (not (search "becomes" trimmed))
                   (not (search " AND " (string-upcase trimmed)))
                   (not (search " OR " (string-upcase trimmed))))
              (let* ((parts (split-string trimmed #\=))
                     (left-val (eval-expr (trim (car parts)) env))
                     (right-val (eval-expr (trim (cadr parts)) env)))
                ;; If both values are numbers, use numeric comparison
                ;; Otherwise use general equality (for booleans, strings, etc.)
                (if (and (numberp left-val) (numberp right-val))
                    (= left-val right-val)
                    (equal left-val right-val))))
           
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
            
             ;; JSON parsing: PARSE JSON {json_string} GET "key" or GET "path.to.value[0]"
             ((starts-with (string-upcase trimmed) "PARSE JSON ")
              (let* ((rest (trim (subseq trimmed 11)))
                     (get-pos (search " GET " (string-upcase rest)))
                     (length-pos (search " LENGTH" (string-upcase rest))))
                (cond
                  ;; PARSE JSON {expr} GET "path.to.value[0]" [LENGTH]
                   (get-pos
                    (let* ((json-expr (trim (subseq rest 0 get-pos)))
                           (after-get (trim (subseq rest (+ get-pos 5))))
                           ;; Check if LENGTH appears after GET
                           (length-suffix-pos (search " LENGTH" (string-upcase after-get)))
                           (path-expr (if length-suffix-pos
                                         (trim (subseq after-get 0 length-suffix-pos))
                                         after-get))
                           (json-str (eval-expr json-expr env))
                           ;; Remove quotes from path if present
                           (path (if (and (> (length path-expr) 1)
                                         (char= (char path-expr 0) #\")
                                         (char= (char path-expr (1- (length path-expr))) #\"))
                                    (subseq path-expr 1 (1- (length path-expr)))
                                    path-expr)))
                     ;; Parse JSON fully and extract value using path
                     (let ((parsed (parse-json-full json-str)))
                       (let ((value (if (or (position #\. path) (position #\[ path))
                                       ;; Use json-get-path for nested paths or array indexing
                                       (json-get-path parsed path)
                                       ;; For simple keys, direct hash table lookup
                                       (if (hash-table-p parsed)
                                           (gethash path parsed)
                                           parsed))))
                         ;; If LENGTH suffix, return length of the extracted value
                         (if length-suffix-pos
                             (cond
                               ((hash-table-p value) (hash-table-count value))
                               ((listp value) (length value))
                               ((stringp value) (length value))
                               (t 0))
                             value)))))
                  
                  ;; PARSE JSON {expr} LENGTH - return array/object length
                  (length-pos
                   (let* ((json-expr (trim (subseq rest 0 length-pos)))
                          (json-str (eval-expr json-expr env))
                          (parsed (parse-json-full json-str)))
                     (cond
                       ((hash-table-p parsed) (hash-table-count parsed))
                       ((listp parsed) (length parsed))
                       (t 0))))
                  
                  ;; No GET/LENGTH - parse and return full structure
                  (t (let* ((json-expr (eval-expr rest env)))
                       (parse-json-full json-expr))))))
            
             ;; File reading: READ FROM FILE "filepath" (MUST come before - operator!)
             ((starts-with (string-upcase trimmed) "READ FROM FILE ")
              (let* ((filepath-expr (trim (subseq trimmed 15)))
                     ;; Remove quotes if present
                     (filepath (if (and (> (length filepath-expr) 1)
                                       (char= (char filepath-expr 0) #\")
                                       (char= (char filepath-expr (1- (length filepath-expr))) #\"))
                                  (subseq filepath-expr 1 (1- (length filepath-expr)))
                                  filepath-expr)))
                (handler-case
                    (with-open-file (stream filepath :direction :input :if-does-not-exist nil)
                      (if stream
                          (let ((contents (make-string (file-length stream))))
                            (read-sequence contents stream)
                            contents)
                          ""))
                  (error (e)
                    (format *error-output* "File read error: ~A~%" e)
                    ""))))
            
             ;; Assignment: n becomes n - 1 (MUST come before string ops that use becomes!)
             ((search " becomes " trimmed)
            (let* ((becomes-pos (search " becomes " trimmed))
                   (var-name (trim (subseq trimmed 0 becomes-pos)))
                   (right-expr (trim (subseq trimmed (+ becomes-pos 9)))))  ; 9 = length of " becomes "
              (let ((result (eval-expr right-expr env)))
                (setf (gethash var-name env) result))))
            
             ;; String operation: text STARTS WITH "prefix"
             ((search " STARTS WITH " (string-upcase trimmed))
              (let* ((pos (search " STARTS WITH " (string-upcase trimmed)))
                     (str-expr (trim (subseq trimmed 0 pos)))
                     (prefix-expr (trim (subseq trimmed (+ pos 13))))
                     (str-val (eval-expr str-expr env))
                     (prefix-val (eval-expr prefix-expr env)))
                (if (and (stringp str-val) (stringp prefix-val))
                    (and (>= (length str-val) (length prefix-val))
                         (string= (subseq str-val 0 (length prefix-val)) prefix-val))
                    nil)))
            
             ;; String operation: text CONTAINS "substring"
             ((search " CONTAINS " (string-upcase trimmed))
              (let* ((pos (search " CONTAINS " (string-upcase trimmed)))
                     (str-expr (trim (subseq trimmed 0 pos)))
                     (substr-expr (trim (subseq trimmed (+ pos 10))))
                     (str-val (eval-expr str-expr env))
                     (substr-val (eval-expr substr-expr env)))
                (if (and (stringp str-val) (stringp substr-val))
                    (if (search substr-val str-val)
                        t
                        nil)
                    nil)))
            
             ;; Regex operation: text MATCHES "pattern"
             ((search " MATCHES " (string-upcase trimmed))
              (if *regex-enabled*
                  (let* ((pos (search " MATCHES " (string-upcase trimmed)))
                         (str-expr (trim (subseq trimmed 0 pos)))
                         (pattern-expr (trim (subseq trimmed (+ pos 9))))
                         (str-val (eval-expr str-expr env))
                         (pattern-val (eval-expr pattern-expr env)))
                    (if (and (stringp str-val) (stringp pattern-val))
                        (handler-case
                            (if (funcall (symbol-function (intern "SCAN" :cl-ppcre))
                                       pattern-val str-val)
                                t
                                nil)
                          (error (e)
                            (format *error-output* "Regex error: ~A~%" e)
                            nil))
                        nil))
                  (progn
                    (format *error-output* "MATCHES operator requires cl-ppcre. Install with: (ql:quickload :cl-ppcre)~%")
                    nil)))
            
             ;; Regex operation: EXTRACT "pattern" FROM text [GROUP n]
             ((starts-with (string-upcase trimmed) "EXTRACT ")
              (if *regex-enabled*
                  (let* ((rest (trim (subseq trimmed 8)))
                         (from-pos (search " FROM " (string-upcase rest))))
                    (if from-pos
                        (let* ((pattern-expr (trim (subseq rest 0 from-pos)))
                               (after-from (trim (subseq rest (+ from-pos 6))))
                               ;; Check for GROUP clause
                               (group-pos (search " GROUP " (string-upcase after-from)))
                               (text-expr (if group-pos
                                            (trim (subseq after-from 0 group-pos))
                                            after-from))
                               (group-num (if group-pos
                                            (eval-expr (trim (subseq after-from (+ group-pos 7))) env)
                                            0))
                               (pattern-val (eval-expr pattern-expr env))
                               (text-val (eval-expr text-expr env)))
                          (if (and (stringp pattern-val) (stringp text-val))
                              (handler-case
                                  (multiple-value-bind (match groups)
                                      (funcall (symbol-function (intern "SCAN-TO-STRINGS" :cl-ppcre))
                                             pattern-val text-val)
                                    (cond
                                      ;; No match found
                                      ((null match) "")
                                      ;; GROUP 0 or no GROUP specified - return full match
                                      ((= group-num 0) match)
                                      ;; GROUP n - return specific capture group
                                      ((and groups (< (1- group-num) (length groups)))
                                       (aref groups (1- group-num)))
                                      ;; Group number out of range
                                      (t "")))
                                (error (e)
                                  (format *error-output* "Regex extraction error: ~A~%" e)
                                  ""))
                              ""))
                        ;; No FROM clause
                        ""))
                  (progn
                    (format *error-output* "EXTRACT operator requires cl-ppcre. Install with: (ql:quickload :cl-ppcre)~%")
                    "")))
            
             ;; String operation: SPLIT text BY "delimiter"
             ((starts-with (string-upcase trimmed) "SPLIT ")
              (let* ((rest (trim (subseq trimmed 6)))
                     (by-pos (search " BY " (string-upcase rest))))
                (if by-pos
                    (let* ((str-expr (trim (subseq rest 0 by-pos)))
                           (delim-expr (trim (subseq rest (+ by-pos 4))))
                           (str-val (eval-expr str-expr env))
                           (delim-val (eval-expr delim-expr env)))
                      (if (and (stringp str-val) (stringp delim-val))
                          (let ((result nil)
                                (start 0))
                            (loop
                              (let ((pos (search delim-val str-val :start2 start)))
                                (if pos
                                    (progn
                                      (push (subseq str-val start pos) result)
                                      (setf start (+ pos (length delim-val))))
                                    (progn
                                      (push (subseq str-val start) result)
                                      (return)))))
                            (nreverse result))
                          nil))
                    ;; No BY clause - error
                    nil)))
            
             ;; String operation: TRIM text
             ((starts-with (string-upcase trimmed) "TRIM ")
              (let* ((rest (trim (subseq trimmed 5)))
                     (str-val (eval-expr rest env)))
                (if (stringp str-val)
                    (trim str-val)
                    "")))
            
             ;; String operation: UPPERCASE text
             ((starts-with (string-upcase trimmed) "UPPERCASE ")
              (let* ((rest (trim (subseq trimmed 10)))
                     (str-val (eval-expr rest env)))
                (if (stringp str-val)
                    (string-upcase str-val)
                    "")))
            
             ;; String operation: LOWERCASE text
             ((starts-with (string-upcase trimmed) "LOWERCASE ")
              (let* ((rest (trim (subseq trimmed 10)))
                     (str-val (eval-expr rest env)))
                (if (stringp str-val)
                    (string-downcase str-val)
                    "")))
            
             ;; String operation: REPLACE "search" WITH "replacement" IN text
             ((starts-with (string-upcase trimmed) "REPLACE ")
              (let* ((rest (trim (subseq trimmed 8)))
                     (with-pos (search " WITH " (string-upcase rest)))
                     (in-pos (search " IN " (string-upcase rest))))
                (if (and with-pos in-pos)
                    (let* ((search-expr (trim (subseq rest 0 with-pos)))
                           (replace-expr (trim (subseq rest (+ with-pos 6) in-pos)))
                           (text-expr (trim (subseq rest (+ in-pos 4))))
                           (search-val (eval-expr search-expr env))
                           (replace-val (eval-expr replace-expr env))
                           (text-val (eval-expr text-expr env)))
                      (if (and (stringp search-val) (stringp replace-val) (stringp text-val))
                          (replace-all text-val search-val replace-val)
                          ""))
                    "")))
            
             ;; String operation: JOIN list WITH "delimiter"
             ((starts-with (string-upcase trimmed) "JOIN ")
              (let* ((rest (trim (subseq trimmed 5)))
                     (with-pos (search " WITH " (string-upcase rest))))
                (if with-pos
                    (let* ((list-expr (trim (subseq rest 0 with-pos)))
                           (delim-expr (trim (subseq rest (+ with-pos 6))))
                           (list-val (eval-expr list-expr env))
                           (delim-val (eval-expr delim-expr env)))
                      (if (and (listp list-val) (stringp delim-val))
                          (with-output-to-string (s)
                            (loop for item in list-val
                                  for first = t then nil
                                  unless first do (write-string delim-val s)
                                  do (princ item s)))
                          ""))
                    "")))
            
             ;; String operation: LENGTH_OF text
             ((starts-with (string-upcase trimmed) "LENGTH_OF ")
              (let* ((rest (trim (subseq trimmed 10)))
                     (val (eval-expr rest env)))
                (cond
                  ((stringp val) (length val))
                  ((listp val) (length val))
                  (t 0))))
            
             ;; CSV operation: CSV READ "filepath"
             ;; Returns list of maps, where each map has header keys
             ((starts-with (string-upcase trimmed) "CSV READ ")
              (let* ((rest (trim (subseq trimmed 9)))
                     (filepath (eval-expr rest env)))
                (when (stringp filepath)
                  (handler-case
                      (with-open-file (stream filepath :direction :input :if-does-not-exist nil)
                        (if stream
                            (let ((header-line (read-line stream nil nil))
                                  (rows '()))
                              (when header-line
                                (let ((headers (mapcar #'trim (split-string header-line #\,))))
                                  ;; Read each data row
                                  (loop for line = (read-line stream nil nil)
                                        while line
                                        do (let* ((values (mapcar #'trim (split-string line #\,)))
                                                  (row (make-hash-table :test #'equal)))
                                             ;; Create map from headers to values
                                             (loop for header in headers
                                                   for value in values
                                                   do (setf (gethash header row) value))
                                             (push row rows)))
                                  (nreverse rows))))
                            '()))
                    (error (e)
                      (format *error-output* "CSV READ error: ~A~%" e)
                      '())))))
            
             ;; Date/Time: FORMAT TIME value WITH "format"
             ((starts-with (string-upcase trimmed) "FORMAT TIME ")
              (let* ((rest (trim (subseq trimmed 12)))
                     (with-pos (search " WITH " (string-upcase rest))))
                (if with-pos
                    (let* ((time-expr (trim (subseq rest 0 with-pos)))
                           (format-expr (trim (subseq rest (+ with-pos 6))))
                           (time-val (eval-expr time-expr env))
                           (format-str (eval-expr format-expr env)))
                      (if (and (numberp time-val) (stringp format-str))
                          (multiple-value-bind (sec min hr day mon yr dow dst tz)
                              (decode-universal-time time-val)
                            (declare (ignore dow dst tz))
                            ;; Simple format string replacement
                            (let ((result format-str))
                              (setf result (replace-all result "YYYY" (format nil "~4,'0D" yr)))
                              (setf result (replace-all result "MM" (format nil "~2,'0D" mon)))
                              (setf result (replace-all result "DD" (format nil "~2,'0D" day)))
                              (setf result (replace-all result "HH" (format nil "~2,'0D" hr)))
                              (setf result (replace-all result "mm" (format nil "~2,'0D" min)))
                              (setf result (replace-all result "SS" (format nil "~2,'0D" sec)))
                              result))
                          ""))
                    "")))
            
             ;; Date/Time: ADD DAYS time BY n (or ADD_DAYS for backward compat)
             ((or (starts-with (string-upcase trimmed) "ADD DAYS ")
                  (starts-with (string-upcase trimmed) "ADD_DAYS("))
              (let* ((is-func-style (starts-with (string-upcase trimmed) "ADD_DAYS("))
                     (rest (if is-func-style
                              (let ((close-paren (position #\) trimmed :from-end t)))
                                (if close-paren
                                    (subseq trimmed 9 close-paren)
                                    (subseq trimmed 9)))
                              (trim (subseq trimmed 9))))
                     (by-pos (search " BY " (string-upcase rest)))
                     (comma-pos (if is-func-style (position #\, rest) nil)))
                (cond
                  ;; Function style: ADD_DAYS(time, n)
                  ((and is-func-style comma-pos)
                   (let* ((time-expr (trim (subseq rest 0 comma-pos)))
                          (days-expr (trim (subseq rest (1+ comma-pos))))
                          (time-val (eval-expr time-expr env))
                          (days-val (eval-expr days-expr env)))
                     (if (and (numberp time-val) (numberp days-val))
                         (+ time-val (* days-val 86400)) ; 86400 seconds per day
                         time-val)))
                  ;; Operator style: ADD DAYS time BY n
                  (by-pos
                   (let* ((time-expr (trim (subseq rest 0 by-pos)))
                          (days-expr (trim (subseq rest (+ by-pos 4))))
                          (time-val (eval-expr time-expr env))
                          (days-val (eval-expr days-expr env)))
                     (if (and (numberp time-val) (numberp days-val))
                         (+ time-val (* days-val 86400))
                         time-val)))
                  (t 0))))
            
             ;; Date/Time: ADD HOURS time BY n
             ((starts-with (string-upcase trimmed) "ADD HOURS ")
              (let* ((rest (trim (subseq trimmed 10)))
                     (by-pos (search " BY " (string-upcase rest))))
                (if by-pos
                    (let* ((time-expr (trim (subseq rest 0 by-pos)))
                           (hours-expr (trim (subseq rest (+ by-pos 4))))
                           (time-val (eval-expr time-expr env))
                           (hours-val (eval-expr hours-expr env)))
                      (if (and (numberp time-val) (numberp hours-val))
                          (+ time-val (* hours-val 3600)) ; 3600 seconds per hour
                          time-val))
                    0)))
            
             ;; Date/Time: ADD MINUTES time BY n
             ((starts-with (string-upcase trimmed) "ADD MINUTES ")
              (let* ((rest (trim (subseq trimmed 12)))
                     (by-pos (search " BY " (string-upcase rest))))
                (if by-pos
                    (let* ((time-expr (trim (subseq rest 0 by-pos)))
                           (mins-expr (trim (subseq rest (+ by-pos 4))))
                           (time-val (eval-expr time-expr env))
                           (mins-val (eval-expr mins-expr env)))
                      (if (and (numberp time-val) (numberp mins-val))
                          (+ time-val (* mins-val 60)) ; 60 seconds per minute
                          time-val))
                    0)))
            
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
           
             ;; Addition/Concatenation: n + 1 or "hello" + "world" or "a" + b + "c"
             ((search "+" trimmed)
              (let* ((parts (split-string trimmed #\+))
                     ;; Evaluate all parts
                     (values (mapcar (lambda (p) (eval-expr (trim p) env)) parts)))
                ;; Determine operation type by first two values
                (if (and (numberp (car values)) (numberp (cadr values)))
                    ;; All numeric: add them all
                    (reduce #'+ values)
                    ;; String concatenation: convert all to strings and concatenate
                    (apply #'concatenate 'string
                           (mapcar (lambda (v)
                                    (if (stringp v) v (format nil "~A" v)))
                                  values)))))
            
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
            
             ;; Length operation: length of list or string
             ((starts-with (string-upcase trimmed) "LENGTH OF ")
              (let* ((var-name (trim (subseq trimmed 10)))
                     (value (eval-expr var-name env)))
                (length value)))
            
             ;; List operation: FIND IN LIST {list_var} WHERE {key} = {value}
             ((starts-with (string-upcase trimmed) "FIND IN LIST ")
              (let* ((rest (trim (subseq trimmed 13)))
                     (where-pos (search " WHERE " (string-upcase rest))))
                (if where-pos
                    (let* ((list-var (trim (subseq rest 0 where-pos)))
                           (condition (trim (subseq rest (+ where-pos 7))))
                           (list-val (gethash list-var env)))
                      ;; Simple WHERE clause: key = value
                      ;; For now, just return first item (simplified for MVP)
                      (if (and list-val (listp list-val) (> (length list-val) 0))
                          (car list-val)
                          nil))
                    nil)))
            
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
             (t (multiple-value-bind (value exists) (gethash trimmed env)
                  (if exists
                      value
                      (error (cns-error-undefined-variable trimmed expr)))))))
       (error (e)
         ;; Re-raise CNS errors with full context
         (if (search "CNS ERROR" (format nil "~A" e))
             (error e)
             ;; Otherwise wrap in expression error
             (error (cns-error-invalid-expression expr (format nil "~A" e)))))))
   
   ;; Fallback
   (t expr)))

;;; ============================================================================
;;; Effect System - Handle side effects
;;; ============================================================================

(defun search-replace (string search replacement)
  "Replace all occurrences of search with replacement in string."
  (let ((result string)
        (search-len (length search)))
    (loop for pos = (search search result)
          while pos
          do (setf result (concatenate 'string
                                      (subseq result 0 pos)
                                      replacement
                                      (subseq result (+ pos search-len)))))
    result))

(defun substitute-vars (text env)
  "Replace {varname} with variable values in text. Supports \\{ and \\} for literal braces."
  (let ((result text))
    ;; First pass: replace escaped braces with placeholders
    (setf result (search-replace result "\\{" "<<<LBRACE>>>"))
    (setf result (search-replace result "\\}" "<<<RBRACE>>>"))
    
    ;; Second pass: substitute variables
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
    
    ;; Third pass: restore escaped braces
    (setf result (search-replace result "<<<LBRACE>>>" "{"))
    (setf result (search-replace result "<<<RBRACE>>>" "}"))
    result))

(defun apply-effect (effect-str env verbose)
  "Execute an effect (Print, Write, etc.)."
  (let ((trimmed (trim effect-str)))
     (cond
      ;; Print/Display "text" or Print {var} or Print "text {var}" or Print "text" + var + "more"
      ((or (starts-with (string-upcase trimmed) "PRINT ")
           (starts-with (string-upcase trimmed) "DISPLAY "))
       (let* ((msg (trim (subseq trimmed (if (starts-with (string-upcase trimmed) "PRINT ") 6 8))))
             ;; First try to evaluate as expression (handles + concatenation)
             (result (handler-case
                        (let ((val (eval-expr msg env)))
                          ;; If it's a string, apply variable substitution
                          (if (stringp val)
                              (substitute-vars val env)
                              (format nil "~A" val)))
                      (error ()
                        ;; Fallback to simple substitution for backward compatibility
                        (let* ((unquoted (if (and (> (length msg) 1)
                                                 (char= (char msg 0) #\")
                                                 (char= (char msg (1- (length msg))) #\"))
                                            (subseq msg 1 (1- (length msg)))
                                            msg)))
                          (substitute-vars unquoted env))))))
        (format t ">>> ~A~%" result)
        (when verbose
          (format t "  Effect: Print~%"))))
     
     ;; Read from file into variable
     ;; Syntax: "Read from file X into Y" or "Read from file 'filename' into var"
     ((starts-with (string-upcase trimmed) "READ FROM FILE ")
      (let* ((rest (trim (subseq trimmed 15)))
             (rest-upper (string-upcase rest))
             (into-pos (search " INTO " rest-upper)))
        (when into-pos
          (let* ((filepath-expr (trim (subseq rest 0 into-pos)))
                 (target-var (trim (subseq rest (+ into-pos 6))))
                 ;; Resolve filepath from variable or use literal
                 (filepath-raw (eval-expr filepath-expr env))
                 ;; Remove quotes if present
                 (filepath (if (and (stringp filepath-raw)
                                   (> (length filepath-raw) 1)
                                   (char= (char filepath-raw 0) #\")
                                   (char= (char filepath-raw (1- (length filepath-raw))) #\"))
                              (subseq filepath-raw 1 (1- (length filepath-raw)))
                              filepath-raw)))
            (handler-case
                (with-open-file (stream filepath :direction :input :if-does-not-exist nil)
                  (if stream
                      (let ((contents (make-string (file-length stream))))
                        (read-sequence contents stream)
                        (setf (gethash target-var env) contents)
                        (when verbose
                          (format t "  Effect: Read ~A bytes from ~A into ~A~%" 
                                  (length contents) filepath target-var)))
                      (progn
                        (setf (gethash target-var env) "")
                        (when verbose
                          (format t "  Effect: File ~A not found, set ~A to empty~%" 
                                  filepath target-var)))))
              (error (e)
                (setf (gethash target-var env) "")
                (when verbose
                  (format t "  Effect: File read error: ~A~%" e))))))))
     
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
     
      ;; Socket: Create socket (REAL implementation)
      ((starts-with (string-upcase trimmed) "CREATE SOCKET ")
       (let* ((rest (trim (subseq trimmed 14)))
              (on-pos (search " ON " (string-upcase rest))))
         (when on-pos
           (let* ((socket-name (trim (subseq rest 0 on-pos)))
                  (port-expr (trim (subseq rest (+ on-pos 4))))
                  (port (eval-expr port-expr env)))
             (handler-case
                 (let ((socket (create-server-socket port)))
                   ;; Store the actual socket object
                   (setf (gethash socket-name env) socket)
                   (when verbose
                     (format t "  Effect: Created REAL socket ~A on port ~A~%" socket-name port)))
               (error (e)
                 (when verbose
                   (format t "  Effect: Failed to create socket: ~A~%" e))))))))
      
      ;; Socket: Bind socket (part of socket creation)
      ((starts-with (string-upcase trimmed) "BIND SOCKET")
       (when verbose
         (format t "  Effect: Bind socket~%")))
      
      ;; Socket: Accept connection (REAL implementation)
      ;; Supports: "Accept connection on X" or "Accept connection on X into Y"
      ((starts-with (string-upcase trimmed) "ACCEPT CONNECTION")
       (let* ((rest (if (> (length trimmed) 17)
                       (trim (subseq trimmed 17))
                       ""))
              (rest-upper (string-upcase rest))
              ;; Handle "on" at start or with leading space
              (on-pos (cond
                       ((starts-with rest-upper "ON ") 0)
                       ((> (length rest-upper) 0) (search " ON " rest-upper))
                       (t nil)))
              (into-pos (when (> (length rest-upper) 0)
                         (search " INTO " rest-upper))))
         (if (numberp on-pos)
             (let* ((skip-chars (if (= on-pos 0) 3 4))  ; "ON " vs " ON "
                    (socket-part (if into-pos
                                    (trim (subseq rest (+ on-pos skip-chars) into-pos))
                                    (trim (subseq rest (+ on-pos skip-chars)))))
                    (client-var (when into-pos
                                 (trim (subseq rest (+ into-pos 6)))))
                    (server-socket (gethash socket-part env)))
               (if server-socket
                   (handler-case
                       (multiple-value-bind (client-socket client-stream)
                           (accept-connection server-socket)
                         ;; Store both socket and stream
                         (if client-var
                             ;; If "into Y" specified, use that variable name
                             (progn
                               (setf (gethash client-var env) client-socket)
                               (setf (gethash "client_stream" env) client-stream))
                             ;; Otherwise use default names
                             (progn
                               (setf (gethash "client_socket" env) client-socket)
                               (setf (gethash "client_stream" env) client-stream)))
                         (when verbose
                           (format t "  Effect: Accepted REAL connection from client~%")))
                     (error (e)
                       (when verbose
                         (format t "  Effect: Failed to accept connection: ~A~%" e))))
                   (when verbose
                     (format t "  Effect: Accept connection (socket '~A' not found)~%" socket-part))))
             (when verbose
               (format t "  Effect: Accept connection (no socket specified)~%")))))
      
       ;; Socket: Network read (REAL implementation)
       ;; Supports: "Network read" or "Network read from X into Y"
       ((starts-with (string-upcase trimmed) "NETWORK READ")
        (let* ((rest (if (> (length trimmed) 12)
                        (trim (subseq trimmed 12))
                        ""))
               (rest-upper (string-upcase rest))
               (from-pos (search " FROM " rest-upper))
               (into-pos (search " INTO " rest-upper))
               (source-var (when from-pos
                            (if into-pos
                                (trim (subseq rest (+ from-pos 6) into-pos))
                                (trim (subseq rest (+ from-pos 6))))))
               (target-var (when into-pos
                            (trim (subseq rest (+ into-pos 6)))))
               (stream (if source-var
                          (gethash source-var env)
                          (gethash "client_stream" env))))
          (if stream
              (handler-case
                  (let ((data (socket-receive stream)))
                    ;; Store in target variable if specified, otherwise use request_data
                    (if target-var
                        (setf (gethash target-var env) data)
                        (setf (gethash "request_data" env) data))
                    ;; Auto-parse HTTP request and extract body
                    (when data
                      (let ((parsed (parse-http-request data)))
                        (when parsed
                          ;; Store method, path, and body as special variables
                          (setf (gethash "REQUEST_METHOD" env) (getf parsed :method))
                          (setf (gethash "REQUEST_PATH" env) (getf parsed :path))
                          (setf (gethash "REQUEST_BODY" env) (or (getf parsed :body) ""))
                          (setf (gethash "REQUEST_QUERY" env) (getf parsed :query-params))
                          (setf (gethash "REQUEST_HEADERS" env) (getf parsed :headers)))))
                    (when verbose
                      (format t "  Effect: Read REAL network data (~A bytes)~%" 
                              (if data (length data) 0))))
                (error (e)
                  (when verbose
                    (format t "  Effect: Failed to read from network: ~A~%" e))))
              (when verbose
                (format t "  Effect: Network read (no stream available)~%")))))
      
      ;; HTTP GET: Effect: HTTP GET from "url" into variable  
      ((starts-with (string-upcase trimmed) "HTTP GET")
       (let* ((rest (trim (subseq trimmed 8)))
              (rest-upper (string-upcase rest))
              ;; Handle both "from" at start and " from " in middle
              (from-pos (or (and (starts-with rest-upper "FROM ") 0)
                           (search " FROM " rest-upper)))
              (into-pos (search " INTO " rest-upper)))
         (when (and (numberp from-pos) into-pos)
           (let* ((from-skip (if (= from-pos 0) 5 6))  ; "FROM " vs " FROM "
                  (url-expr (trim (subseq rest (+ from-pos from-skip) into-pos)))
                  (target-var (trim (subseq rest (+ into-pos 6))))
                  ;; Evaluate URL expression to resolve variables
                  (url (eval-expr url-expr env)))
             ;; Remove quotes from URL if present
             (when (and (stringp url) (> (length url) 1)
                       (char= (char url 0) #\")
                       (char= (char url (1- (length url))) #\"))
               (setf url (subseq url 1 (1- (length url)))))
             
             (when verbose
               (format t "  Effect: HTTP GET from ~A~%" url))
             
             (handler-case
                 (multiple-value-bind (status headers body)
                     (http-request url :method "GET")
                   ;; Store response in target variable
                   (setf (gethash target-var env) body)
                   ;; Store metadata in special variables
                   (setf (gethash "HTTP_STATUS" env) status)
                   (setf (gethash "HTTP_HEADERS" env) headers)
                   (when verbose
                     (format t "  Effect: HTTP GET completed (status ~D, ~A bytes)~%" 
                             status (length body))))
               (error (e)
                 (setf (gethash target-var env) "")
                 (when verbose
                   (format t "  Effect: HTTP GET failed: ~A~%" e))))))))
      
      ;; HTTP POST: Effect: HTTP POST to "url" with body_var into response_var
      ((starts-with (string-upcase trimmed) "HTTP POST")
       (let* ((rest (trim (subseq trimmed 9)))
              (rest-upper (string-upcase rest))
              ;; Handle both "to" at start and " to " in middle
              (to-pos (or (and (starts-with rest-upper "TO ") 0)
                         (search " TO " rest-upper)))
              (with-pos (search " WITH " rest-upper))
              (into-pos (search " INTO " rest-upper)))
         (when (and (numberp to-pos) into-pos)
           (let* ((to-skip (if (= to-pos 0) 3 4))  ; "TO " vs " TO "
                  (url-expr (if with-pos
                               (trim (subseq rest (+ to-pos to-skip) with-pos))
                               (trim (subseq rest (+ to-pos to-skip) into-pos))))
                  (body-expr (when with-pos
                              (trim (subseq rest (+ with-pos 6) into-pos))))
                  (target-var (trim (subseq rest (+ into-pos 6))))
                  ;; Evaluate expressions
                  (url (eval-expr url-expr env))
                  (body (when body-expr (eval-expr body-expr env))))
             
             ;; Remove quotes from URL if present
             (when (and (stringp url) (> (length url) 1)
                       (char= (char url 0) #\")
                       (char= (char url (1- (length url))) #\"))
               (setf url (subseq url 1 (1- (length url)))))
             
             ;; Remove quotes from body if it's a literal string
             (when (and body (stringp body) (> (length body) 1)
                       (char= (char body 0) #\")
                       (char= (char body (1- (length body))) #\"))
               (setf body (subseq body 1 (1- (length body)))))
             
             (when verbose
               (format t "  Effect: HTTP POST to ~A~%" url))
             
             (handler-case
                 (multiple-value-bind (status headers response-body)
                     (http-request url :method "POST" :body body)
                   ;; Store response in target variable
                   (setf (gethash target-var env) response-body)
                   ;; Store metadata
                   (setf (gethash "HTTP_STATUS" env) status)
                   (setf (gethash "HTTP_HEADERS" env) headers)
                   (when verbose
                     (format t "  Effect: HTTP POST completed (status ~D, ~A bytes)~%" 
                             status (length response-body))))
                (error (e)
                  (setf (gethash target-var env) "")
                  (when verbose
                    (format t "  Effect: HTTP POST failed: ~A~%" e))))))))
       
        ;; Database: DB CONNECT TO "filepath" AS db_name
        ((starts-with (string-upcase trimmed) "DB CONNECT")
         (let* ((rest (trim (subseq trimmed 10)))
                (rest-upper (string-upcase rest))
                ;; Handle both " TO " and "TO " at start
                (to-pos (or (and (starts-with rest-upper "TO ") 0)
                           (search " TO " rest-upper)))
                (as-pos (search " AS " rest-upper)))
           (when (and (numberp to-pos) as-pos)
             (let* ((to-skip (if (= to-pos 0) 3 4))  ; "TO " vs " TO "
                    (filepath-expr (trim (subseq rest (+ to-pos to-skip) as-pos)))
                    (db-name (trim (subseq rest (+ as-pos 4))))
                    ;; Handle string literals directly without eval-expr to avoid parsing SQL operators
                    (filepath (if (and (> (length filepath-expr) 1)
                                      (char= (char filepath-expr 0) #\")
                                      (char= (char filepath-expr (1- (length filepath-expr))) #\"))
                                 (subseq filepath-expr 1 (1- (length filepath-expr)))
                                 (eval-expr filepath-expr env))))
               
               (when *db-enabled*
                 (db-connect db-name filepath)
                 (format t "  Effect: Connected to database ~A at ~A~%" db-name filepath))
               (unless *db-enabled*
                 (format t "  Effect: DB CONNECT skipped (sqlite3 not available)~%"))))))
       
        ;; Database: DB EXECUTE "SQL" AS db_name
        ((starts-with (string-upcase trimmed) "DB EXECUTE")
         (let* ((rest (trim (subseq trimmed 10)))
                (rest-upper (string-upcase rest)))
           ;; Find the end of the SQL string first (closing quote)
           (when (and (> (length rest) 0) (char= (char rest 0) #\"))
             (let ((sql-end (position #\" rest :start 1)))
               (when sql-end
                 ;; Now look for AS after the SQL string (use uppercase for search)
                 (let* ((after-sql-upper (subseq rest-upper (1+ sql-end)))
                        (after-sql (subseq rest (1+ sql-end)))
                        (as-pos (or (and (starts-with (trim after-sql-upper) "AS ") 0)
                                   (search " AS " after-sql-upper))))
                   (when (numberp as-pos)
                     (let* ((sql (subseq rest 1 sql-end))  ; Extract SQL without quotes
                            (as-skip (if (= as-pos 0) 3 4))
                            (db-name (trim (subseq after-sql (+ as-pos as-skip)))))
                       
                       (when *db-enabled*
                          (handler-case
                              (progn
                                (db-execute db-name sql)
                                (when verbose
                                  (format t "  Effect: Executed SQL on ~A~%" db-name)))
                            (error (e)
                              (when verbose
                                (format t "  Effect: DB EXECUTE failed: ~A~%" e)))))
                        (unless *db-enabled*
                          (when verbose
                            (format t "  Effect: DB EXECUTE skipped (sqlite3 not available)~%")))))))))))
       
         ;; Database: DB QUERY "SQL" AS db_name INTO var
         ((starts-with (string-upcase trimmed) "DB QUERY")
          (let* ((rest (trim (subseq trimmed 8)))
                 (rest-upper (string-upcase rest)))
            ;; Find the end of the SQL string first (closing quote)
            (when (and (> (length rest) 0) (char= (char rest 0) #\"))
              (let ((sql-end (position #\" rest :start 1)))
                (when sql-end
                  ;; Now look for AS and INTO after the SQL string (use uppercase for search)
                  (let* ((after-sql-upper (subseq rest-upper (1+ sql-end)))
                         (after-sql (subseq rest (1+ sql-end)))
                         (as-pos (search " AS " after-sql-upper))
                         (into-pos (search " INTO " after-sql-upper)))
                    (when (and as-pos into-pos)
                      (let* ((sql (subseq rest 1 sql-end))  ; Extract SQL without quotes
                             (db-name (trim (subseq after-sql (+ as-pos 4) into-pos)))
                             (target-var (trim (subseq after-sql (+ into-pos 6)))))
                       
                        (when *db-enabled*
                          (handler-case
                              (let ((result (db-query db-name sql)))
                                (setf (gethash target-var env) result)
                                (when verbose
                                  (format t "  Effect: Queried database ~A, stored in ~A~%" db-name target-var)))
                            (error (e)
                              (setf (gethash target-var env) "")
                              (when verbose
                                (format t "  Effect: DB QUERY failed: ~A~%" e)))))
                          (unless *db-enabled*
                          (setf (gethash target-var env) "")
                          (when verbose
                            (format t "  Effect: DB QUERY skipped (sqlite3 not available)~%")))))))))))
       
        ;; CSV WRITE: CSV WRITE data TO "filepath" WITH HEADERS headers
        ((starts-with (string-upcase trimmed) "CSV WRITE ")
         (let* ((rest (trim (subseq trimmed 10)))
                (to-pos (search " TO " (string-upcase rest)))
                (with-pos (search " WITH HEADERS " (string-upcase rest))))
           (when (and to-pos with-pos)
             (let* ((data-expr (trim (subseq rest 0 to-pos)))
                    (file-expr (trim (subseq rest (+ to-pos 4) with-pos)))
                    (headers-expr (trim (subseq rest (+ with-pos 14))))
                    (data (eval-expr data-expr env))
                    (filepath (eval-expr file-expr env))
                    (headers (eval-expr headers-expr env)))
               (when (and (listp data) (stringp filepath) (listp headers))
                 (handler-case
                     (with-open-file (stream filepath :direction :output 
                                           :if-exists :supersede 
                                           :if-does-not-exist :create)
                       ;; Write header row
                       (format stream "~{~A~^,~}~%" headers)
                       ;; Write data rows
                       (dolist (row data)
                         (if (hash-table-p row)
                             ;; Map/hash-table: extract values in header order
                             (let ((values (mapcar (lambda (h) (gethash h row "")) headers)))
                               (format stream "~{~A~^,~}~%" values))
                             ;; List: write directly
                             (format stream "~{~A~^,~}~%" row)))
                       (when verbose
                         (format t "  Effect: Wrote ~D rows to CSV file ~A~%" (length data) filepath)))
                   (error (e)
                     (when verbose
                       (format t "  Effect: CSV WRITE failed: ~A~%" e)))))))))
       
        ;; Socket: Network write
       ((starts-with (string-upcase trimmed) "NETWORK WRITE")
        (when verbose
          (format t "  Effect: Network write (simulated)~%")))
      
       ;; Socket: Send response (REAL implementation)
       ((starts-with (string-upcase trimmed) "SEND ")
        (let* ((rest (trim (subseq trimmed 5))))
           ;; First, extract the quoted string if present
           (multiple-value-bind (content after-content)
               (if (and (> (length rest) 0) (char= (char rest 0) #\"))
                   (extract-quoted-string rest)
                   (values rest ""))
            ;; Now find " TO " in the remaining part
            (let* ((rest-after-quote (if (> (length after-content) 0)
                                        after-content
                                        rest))
                   (to-pos (search " TO " (string-upcase rest-after-quote))))
              (when to-pos
                (let* ((target (trim (subseq rest-after-quote (+ to-pos 4))))
                       (expanded (substitute-vars content env))
                       (stream (gethash "client_stream" env)))
             (if stream
                 (handler-case
                     (progn
                       (socket-send stream expanded)
                       (when verbose
                         (format t "  Effect: Sent REAL data (~A bytes) to ~A~%" 
                                 (length expanded) target)))
                   (error (e)
                     (when verbose
                       (format t "  Effect: Failed to send: ~A~%" e))))
                  (when verbose
                    (format t "  Effect: Send (no stream available)~%")))))))))
      
       ;; List operation: ADD {value} TO LIST {list_var}
       ((search " TO LIST " (string-upcase trimmed))
        (let* ((to-pos (search " TO LIST " (string-upcase trimmed)))
               (value-expr (trim (subseq trimmed 0 to-pos)))
               (list-var (trim (subseq trimmed (+ to-pos 9))))
               ;; Handle "ADD" prefix if present
               (clean-value-expr (if (starts-with (string-upcase value-expr) "ADD ")
                                    (trim (subseq value-expr 4))
                                    value-expr))
               (value (eval-expr clean-value-expr env))
               (current-list (gethash list-var env)))
          (if current-list
              (setf (gethash list-var env) (append current-list (list value)))
              (setf (gethash list-var env) (list value)))
          (when verbose
            (format t "  Effect: Added ~A to list ~A~%" value list-var))))
      
       ;; List operation: REMOVE {value} FROM LIST {list_var}
       ((search " FROM LIST " (string-upcase trimmed))
        (let* ((from-pos (search " FROM LIST " (string-upcase trimmed)))
               (value-expr (trim (subseq trimmed 0 from-pos)))
               (list-var (trim (subseq trimmed (+ from-pos 11))))
               ;; Handle "REMOVE" prefix if present
               (clean-value-expr (if (starts-with (string-upcase value-expr) "REMOVE ")
                                    (trim (subseq value-expr 7))
                                    value-expr))
               (value (eval-expr clean-value-expr env))
               (current-list (gethash list-var env)))
          (when current-list
            (setf (gethash list-var env) (remove value current-list :test #'equal)))
          (when verbose
            (format t "  Effect: Removed ~A from list ~A~%" value list-var))))
      
       ;; Socket: Close socket (REAL implementation)
       ((starts-with (string-upcase trimmed) "CLOSE SOCKET")
        (let* ((socket-name (trim (subseq trimmed 13)))
               (socket (gethash socket-name env)))
          (when socket
            (handler-case
                (progn
                  (close-socket socket)
                  (setf (gethash socket-name env) nil)
                 (when verbose
                   (format t "  Effect: Closed REAL socket ~A~%" socket-name)))
             (error (e)
               (when verbose
                 (format t "  Effect: Failed to close socket: ~A~%" e)))))))
      
      ;; Socket: Close connection
      ((starts-with (string-upcase trimmed) "CLOSE CONNECTION")
       (let ((client-socket (gethash "client_socket" env))
             (client-stream (gethash "client_stream" env)))
         (when client-stream
           (ignore-errors (close client-stream)))
         (when client-socket
           (ignore-errors (close-socket client-socket)))
         (setf (gethash "client_socket" env) nil)
         (setf (gethash "client_stream" env) nil)
         (when verbose
           (format t "  Effect: Closed client connection~%"))))
      
       ;; FIND: Search for files by pattern
       ;; Syntax: FIND "pattern" INTO files
       ;; Syntax: FIND "pattern" IN "directory" INTO files
       ;; Syntax: FIND "pattern" IN "directory" INTO files WITH COUNT count
       ((starts-with (string-upcase trimmed) "FIND ")
        (let* ((rest (trim (subseq trimmed 5)))
               (rest-upper (string-upcase rest))
               ;; Extract the pattern (quoted string)
               (pattern-end (position #\" rest :start 1))
               (pattern (if pattern-end
                           (subseq rest 1 pattern-end)
                           nil))
                (after-pattern (if pattern-end (trim (subseq rest (1+ pattern-end))) ""))
                (after-pattern-upper (string-upcase after-pattern))
                ;; Find IN, INTO, WITH COUNT positions
                (in-pos-space (search " IN " after-pattern-upper))
                (in-pos-start (when (starts-with after-pattern-upper "IN ") 0))
                (in-pos (or in-pos-space in-pos-start))
                (into-pos-space (search " INTO " after-pattern-upper))
                (into-pos-start (when (starts-with after-pattern-upper "INTO ") 0))
                (into-pos (or into-pos-space into-pos-start))
                (count-pos (search " WITH COUNT " after-pattern-upper))
                ;; Extract directory if present
                (directory (when (and in-pos into-pos)
                            (let* ((start (if (zerop in-pos) 3 (+ in-pos 4)))
                                   (end into-pos)
                                   (dir-str (trim (subseq after-pattern start end))))
                              (if (and (> (length dir-str) 1)
                                      (char= (char dir-str 0) #\")
                                      (char= (char dir-str (1- (length dir-str))) #\"))
                                  (subseq dir-str 1 (1- (length dir-str)))
                                  dir-str))))
               ;; Extract variable names
               (files-var (when into-pos
                           (let ((start (if (zerop into-pos) 5 (+ into-pos 6)))
                                 (end (or count-pos (length after-pattern))))
                             (trim (subseq after-pattern start end)))))
               (count-var (when count-pos
                           (trim (subseq after-pattern (+ count-pos 12))))))
          (if (and pattern files-var)
              (handler-case
                  (let* ((base-dir (or directory "."))
                         (matches '())
                         (match-count 0))
                    ;; Recursive directory traversal
                    (labels ((matches-pattern (name pattern)
                              (let ((pattern-regex (cl-ppcre:create-scanner
                                                    (cl-ppcre:regex-replace-all
                                                     "\\*" 
                                                     (cl-ppcre:regex-replace-all "\\." pattern "\\\\.")
                                                     ".*")
                                                    :case-insensitive-mode t)))
                                (cl-ppcre:scan pattern-regex name)))
                            (scan-directory (dir)
                              (when (probe-file dir)
                                (dolist (entry (uiop:directory-files dir))
                                  (let ((name (file-namestring entry)))
                                    (when (matches-pattern name pattern)
                                      (push (namestring entry) matches)
                                      (incf match-count))))
                                (dolist (subdir (uiop:subdirectories dir))
                                  (scan-directory subdir)))))
                      (scan-directory base-dir))
                    ;; Store results
                    (setf (gethash files-var env) (nreverse matches))
                    (when count-var
                      (setf (gethash count-var env) match-count))
                    (when verbose
                      (format t "  Effect: FIND ~S in ~S -> ~A files~%" pattern base-dir match-count)))
                (error (e)
                  (setf (gethash files-var env) '())
                  (when count-var
                    (setf (gethash count-var env) 0))
                  (when verbose
                    (format t "  Effect: FIND failed: ~A~%" e))))
              (when verbose
                (format t "  Effect: FIND (invalid syntax)~%")))))
       
       ;; GREP: Search file contents for pattern
       ;; Syntax: GREP "pattern" IN "file" INTO matches
       ;; Syntax: GREP "pattern" IN files INTO results (where files is a list)
       ;; Syntax: GREP "pattern" IN "file" INTO matches WITH COUNT count
       ((starts-with (string-upcase trimmed) "GREP ")
        (let* ((rest (trim (subseq trimmed 5)))
               (rest-upper (string-upcase rest))
               ;; Extract the pattern (quoted string)
               (pattern-end (position #\" rest :start 1))
               (pattern (if pattern-end
                           (subseq rest 1 pattern-end)
                           nil))
               (after-pattern (if pattern-end (trim (subseq rest (1+ pattern-end))) ""))
               (after-pattern-upper (string-upcase after-pattern))
               ;; Find IN, INTO, WITH COUNT positions
               (in-pos (search " IN " after-pattern-upper))
               (into-pos-space (search " INTO " after-pattern-upper))
               (into-pos-start (when (starts-with after-pattern-upper "INTO ") 0))
               (into-pos (or into-pos-space into-pos-start))
               (count-pos (search " WITH COUNT " after-pattern-upper))
               ;; Extract file/variable
               (file-or-var (when (and in-pos into-pos)
                             (let* ((start (+ in-pos 4))
                                    (end into-pos)
                                    (str (trim (subseq after-pattern start end))))
                               (if (and (> (length str) 1)
                                       (char= (char str 0) #\")
                                       (char= (char str (1- (length str))) #\"))
                                   (subseq str 1 (1- (length str)))
                                   str))))
               ;; Extract variable names
               (matches-var (when into-pos
                             (let ((start (if (zerop into-pos) 5 (+ into-pos 6)))
                                   (end (or count-pos (length after-pattern))))
                               (trim (subseq after-pattern start end)))))
               (count-var (when count-pos
                           (trim (subseq after-pattern (+ count-pos 12))))))
          (if (and pattern file-or-var matches-var)
              (handler-case
                  (let* ((regex (cl-ppcre:create-scanner pattern))
                         (all-matches '())
                         (total-count 0))
                    ;; Check if file-or-var is a variable containing a list
                    (let ((file-list (gethash file-or-var env)))
                      (cond
                        ;; It's a list variable
                        ((listp file-list)
                         (dolist (file file-list)
                           (when (and file (probe-file file))
                             (with-open-file (in file :direction :input 
                                                 :if-does-not-exist nil)
                               (when in
                                 (loop for line = (read-line in nil)
                                       for line-num from 1
                                       while line
                                       when (cl-ppcre:scan regex line)
                                       do (push (list :file file :line line-num :text line)
                                               all-matches)
                                          (incf total-count)))))))
                        ;; It's a file path
                        (t
                         (when (probe-file file-or-var)
                           (with-open-file (in file-or-var :direction :input
                                              :if-does-not-exist nil)
                             (when in
                               (loop for line = (read-line in nil)
                                     for line-num from 1
                                     while line
                                     when (cl-ppcre:scan regex line)
                                     do (push (list :file file-or-var :line line-num :text line)
                                             all-matches)
                                        (incf total-count))))))))
                    ;; Store results
                    (setf (gethash matches-var env) (nreverse all-matches))
                    (when count-var
                      (setf (gethash count-var env) total-count))
                    (when verbose
                      (format t "  Effect: GREP ~S -> ~A matches~%" pattern total-count)))
                (error (e)
                  (setf (gethash matches-var env) '())
                  (when count-var
                    (setf (gethash count-var env) 0))
                  (when verbose
                    (format t "  Effect: GREP failed: ~A~%" e))))
              (when verbose
                (format t "  Effect: GREP (invalid syntax)~%")))))
       
       ;; SHELL: Execute shell commands with output capture
       ;; Syntax: SHELL "command" INTO output WITH EXIT_CODE code
       ;; Syntax: SHELL "command" INTO output WITH EXIT_CODE code AND ERROR error
       ((starts-with (string-upcase trimmed) "SHELL ")
       (let* ((rest (trim (subseq trimmed 6)))
              (rest-upper (string-upcase rest))
              ;; Extract the command (quoted string)
              (cmd-end (position #\" rest :start 1))
              (command (if cmd-end
                          (subseq rest 1 cmd-end)
                          nil))
              (after-cmd (if cmd-end (trim (subseq rest (1+ cmd-end))) ""))
               (after-cmd-upper (string-upcase after-cmd))
               ;; Find INTO, WITH EXIT_CODE, AND ERROR positions
               ;; Handle both " INTO " and "INTO " at start
               (into-pos-space (search " INTO " after-cmd-upper))
               (into-pos-start (when (starts-with after-cmd-upper "INTO ") 0))
               (into-pos (or into-pos-space into-pos-start))
               (exit-pos (search " WITH EXIT_CODE " after-cmd-upper))
               (error-pos (search " AND ERROR " after-cmd-upper))
               ;; Extract variable names
               (output-var (when (and into-pos exit-pos)
                            (let ((start (if (zerop into-pos) 5 (+ into-pos 6))))
                              (trim (subseq after-cmd start exit-pos)))))
               (exit-var (when exit-pos
                          (let ((end (or error-pos (length after-cmd))))
                            (trim (subseq after-cmd (+ exit-pos 16) end)))))
               (error-var (when error-pos
                           (trim (subseq after-cmd (+ error-pos 11))))))
         (if command
             (handler-case
                 (let* ((process (sb-ext:run-program "/bin/sh" 
                                                    (list "-c" command)
                                                    :output :stream
                                                    :error :stream
                                                    :wait t
                                                    :search nil))
                        (output-stream (sb-ext:process-output process))
                        (error-stream (sb-ext:process-error process))
                        (output (with-output-to-string (s)
                                 (loop for line = (read-line output-stream nil nil)
                                       while line
                                       do (format s "~A~%" line))))
                        (error-output (with-output-to-string (s)
                                       (loop for line = (read-line error-stream nil nil)
                                             while line
                                             do (format s "~A~%" line))))
                        (exit-code (or (sb-ext:process-exit-code process) 0)))
                   ;; Store outputs in variables
                   (when output-var
                     (setf (gethash output-var env) (string-trim '(#\Newline) output)))
                   (when exit-var
                     (setf (gethash exit-var env) exit-code))
                   (when error-var
                     (setf (gethash error-var env) (string-trim '(#\Newline) error-output)))
                   ;; Close streams
                   (close output-stream)
                   (close error-stream)
                   (when verbose
                     (format t "  Effect: SHELL executed '~A' (exit: ~A)~%" command exit-code)))
               (error (e)
                 (when output-var (setf (gethash output-var env) ""))
                 (when exit-var (setf (gethash exit-var env) 1))
                 (when error-var (setf (gethash error-var env) (format nil "~A" e)))
                 (when verbose
                   (format t "  Effect: SHELL failed: ~A~%" e))))
             (when verbose
               (format t "  Effect: SHELL (invalid syntax)~%")))))
      
       ;; GIT: Execute git operations
       ;; Syntax: GIT CLONE "url" INTO "directory" WITH STATUS result
       ;; Syntax: GIT STATUS INTO output WITH EXIT_CODE code
       ;; Syntax: GIT CHECKOUT "branch" WITH STATUS result
       ;; Syntax: GIT DIFF INTO patch WITH EXIT_CODE code
       ;; Syntax: GIT DIFF "file" INTO patch WITH EXIT_CODE code
       ;; Syntax: GIT ADD "files" WITH STATUS result
       ;; Syntax: GIT COMMIT "message" WITH STATUS result
       ;; Syntax: GIT BRANCH INTO output WITH EXIT_CODE code
       ;; Syntax: GIT BRANCH CREATE "name" WITH STATUS result
       ;; Syntax: GIT BRANCH DELETE "name" WITH STATUS result
       ;; Syntax: GIT MERGE "branch" WITH STATUS result AND OUTPUT output
       ;; Syntax: GIT LOG INTO output WITH EXIT_CODE code
       ;; Syntax: GIT LOG "options" INTO output WITH EXIT_CODE code
       ((starts-with (string-upcase trimmed) "GIT ")
        (let* ((rest (trim (subseq trimmed 4)))
               (rest-upper (string-upcase rest))
               ;; Determine git subcommand
               (subcommand (cond
                            ((starts-with rest-upper "CLONE ") "clone")
                            ((starts-with rest-upper "STATUS") "status")
                            ((starts-with rest-upper "CHECKOUT ") "checkout")
                            ((starts-with rest-upper "DIFF") "diff")
                            ((starts-with rest-upper "ADD ") "add")
                            ((starts-with rest-upper "COMMIT ") "commit")
                            ((starts-with rest-upper "BRANCH") "branch")
                            ((starts-with rest-upper "MERGE ") "merge")
                            ((starts-with rest-upper "LOG") "log")
                            (t nil))))
         (cond
          ;; GIT CLONE "url" INTO "directory" WITH STATUS result
          ((string= subcommand "clone")
           (let* ((after-clone (trim (subseq rest 6)))
                  (url-end (position #\" after-clone :start 1))
                  (url (if url-end (subseq after-clone 1 url-end) nil))
                  (after-url (if url-end (trim (subseq after-clone (1+ url-end))) ""))
                  (after-url-upper (string-upcase after-url))
                  (into-pos (search " INTO " after-url-upper))
                  (status-pos (search " WITH STATUS " after-url-upper))
                  (dir-start (if into-pos (+ into-pos 6) nil))
                  (dir-end (if status-pos status-pos (length after-url)))
                  (directory (when dir-start
                              (let ((raw (trim (subseq after-url dir-start dir-end))))
                                (if (and (> (length raw) 1)
                                        (char= (char raw 0) #\")
                                        (char= (char raw (1- (length raw))) #\"))
                                    (subseq raw 1 (1- (length raw)))
                                    raw))))
                  (status-var (when status-pos
                               (trim (subseq after-url (+ status-pos 13))))))
             (if (and url directory)
                 (handler-case
                     (let* ((process (sb-ext:run-program "/usr/bin/git"
                                                        (list "clone" url directory)
                                                        :output :stream
                                                        :error :stream
                                                        :wait t
                                                        :search nil))
                            (exit-code (or (sb-ext:process-exit-code process) 0)))
                       (when status-var
                         (setf (gethash status-var env) exit-code))
                       (when verbose
                         (format t "  Effect: GIT CLONE ~A -> ~A (exit: ~A)~%" url directory exit-code)))
                   (error (e)
                     (when status-var (setf (gethash status-var env) 1))
                     (when verbose
                       (format t "  Effect: GIT CLONE failed: ~A~%" e))))
                 (when verbose
                   (format t "  Effect: GIT CLONE (invalid syntax)~%")))))
          
          ;; GIT STATUS INTO output WITH EXIT_CODE code
          ((string= subcommand "status")
           (let* ((into-pos (search " INTO " rest-upper))
                  (exit-pos (search " WITH EXIT_CODE " rest-upper))
                  (output-var (when (and into-pos exit-pos)
                               (trim (subseq rest (+ into-pos 6) exit-pos))))
                  (exit-var (when exit-pos
                             (trim (subseq rest (+ exit-pos 16))))))
             (handler-case
                 (let* ((process (sb-ext:run-program "/usr/bin/git"
                                                    '("status")
                                                    :output :stream
                                                    :error :stream
                                                    :wait t
                                                    :search nil))
                        (output-stream (sb-ext:process-output process))
                        (output (with-output-to-string (s)
                                 (loop for line = (read-line output-stream nil nil)
                                       while line
                                       do (format s "~A~%" line))))
                        (exit-code (or (sb-ext:process-exit-code process) 0)))
                   (when output-var
                     (setf (gethash output-var env) (string-trim '(#\Newline) output)))
                   (when exit-var
                     (setf (gethash exit-var env) exit-code))
                   (close output-stream)
                   (when verbose
                     (format t "  Effect: GIT STATUS (exit: ~A)~%" exit-code)))
               (error (e)
                 (when output-var (setf (gethash output-var env) ""))
                 (when exit-var (setf (gethash exit-var env) 1))
                 (when verbose
                   (format t "  Effect: GIT STATUS failed: ~A~%" e))))))
          
          ;; GIT CHECKOUT "branch" WITH STATUS result
          ((string= subcommand "checkout")
           (let* ((after-checkout (trim (subseq rest 9)))
                  (branch-end (position #\" after-checkout :start 1))
                  (branch (if branch-end (subseq after-checkout 1 branch-end) nil))
                  (after-branch (if branch-end (trim (subseq after-checkout (1+ branch-end))) ""))
                  (status-pos (search " WITH STATUS " (string-upcase after-branch)))
                  (status-var (when status-pos
                               (trim (subseq after-branch (+ status-pos 13))))))
             (if branch
                 (handler-case
                     (let* ((process (sb-ext:run-program "/usr/bin/git"
                                                        (list "checkout" branch)
                                                        :output :stream
                                                        :error :stream
                                                        :wait t
                                                        :search nil))
                            (exit-code (or (sb-ext:process-exit-code process) 0)))
                       (when status-var
                         (setf (gethash status-var env) exit-code))
                       (when verbose
                         (format t "  Effect: GIT CHECKOUT ~A (exit: ~A)~%" branch exit-code)))
                   (error (e)
                     (when status-var (setf (gethash status-var env) 1))
                     (when verbose
                       (format t "  Effect: GIT CHECKOUT failed: ~A~%" e))))
                 (when verbose
                   (format t "  Effect: GIT CHECKOUT (invalid syntax)~%")))))
          
           ;; GIT DIFF [file] INTO patch WITH EXIT_CODE code
           ((string= subcommand "diff")
            (let* ((after-diff (trim (subseq rest 4)))
                   (after-diff-upper (string-upcase after-diff))
                   ;; Check if file is specified
                   (file-spec (when (char= (char after-diff 0) #\")
                               (let ((file-end (position #\" after-diff :start 1)))
                                 (if file-end (subseq after-diff 1 file-end) nil))))
                   (after-file (if file-spec
                                  (trim (subseq after-diff (+ (length file-spec) 2)))
                                  after-diff))
                   (after-file-upper (string-upcase after-file))
                   (into-pos-space (search " INTO " after-file-upper))
                   (into-pos-start (when (starts-with after-file-upper "INTO ") 0))
                   (into-pos (or into-pos-space into-pos-start))
                   (exit-pos (search " WITH EXIT_CODE " after-file-upper))
                   (output-var (when (and into-pos exit-pos)
                                (let ((start (if (eql into-pos 0) 5 (+ into-pos 6))))
                                  (trim (subseq after-file start exit-pos)))))
                   (exit-var (when exit-pos
                              (trim (subseq after-file (+ exit-pos 16))))))
              (handler-case
                  (let* ((git-args (if file-spec
                                      (list "diff" "--unified" file-spec)
                                      '("diff" "--unified")))
                         (process (sb-ext:run-program "/usr/bin/git"
                                                     git-args
                                                     :output :stream
                                                     :error :stream
                                                     :wait t
                                                     :search nil))
                         (output-stream (sb-ext:process-output process))
                         (output (with-output-to-string (s)
                                  (loop for line = (read-line output-stream nil nil)
                                        while line
                                        do (format s "~A~%" line))))
                         (exit-code (or (sb-ext:process-exit-code process) 0)))
                    (when output-var
                      (setf (gethash output-var env) (string-trim '(#\Newline) output)))
                    (when exit-var
                      (setf (gethash exit-var env) exit-code))
                    (close output-stream)
                    (when verbose
                      (format t "  Effect: GIT DIFF~A (exit: ~A)~%" 
                              (if file-spec (format nil " ~A" file-spec) "") exit-code)))
                (error (e)
                  (when output-var (setf (gethash output-var env) ""))
                  (when exit-var (setf (gethash exit-var env) 1))
                  (when verbose
                    (format t "  Effect: GIT DIFF failed: ~A~%" e))))))
          
          ;; GIT ADD "files" WITH STATUS result
          ((string= subcommand "add")
           (let* ((after-add (trim (subseq rest 4)))
                  (files-end (position #\" after-add :start 1))
                  (files (if files-end (subseq after-add 1 files-end) nil))
                  (after-files (if files-end (trim (subseq after-add (1+ files-end))) ""))
                  (status-pos (search " WITH STATUS " (string-upcase after-files)))
                  (status-var (when status-pos
                               (trim (subseq after-files (+ status-pos 13))))))
             (if files
                 (handler-case
                     (let* ((process (sb-ext:run-program "/usr/bin/git"
                                                        (append '("add") (split-string files #\Space))
                                                        :output :stream
                                                        :error :stream
                                                        :wait t
                                                        :search nil))
                            (exit-code (or (sb-ext:process-exit-code process) 0)))
                       (when status-var
                         (setf (gethash status-var env) exit-code))
                       (when verbose
                         (format t "  Effect: GIT ADD ~A (exit: ~A)~%" files exit-code)))
                   (error (e)
                     (when status-var (setf (gethash status-var env) 1))
                     (when verbose
                       (format t "  Effect: GIT ADD failed: ~A~%" e))))
                 (when verbose
                   (format t "  Effect: GIT ADD (invalid syntax)~%")))))
          
           ;; GIT COMMIT "message" WITH STATUS result
           ((string= subcommand "commit")
            (let* ((after-commit (trim (subseq rest 7)))
                   (msg-end (position #\" after-commit :start 1))
                   (message (if msg-end (subseq after-commit 1 msg-end) nil))
                   (after-msg (if msg-end (trim (subseq after-commit (1+ msg-end))) ""))
                   (status-pos (search " WITH STATUS " (string-upcase after-msg)))
                   (status-var (when status-pos
                                (trim (subseq after-msg (+ status-pos 13))))))
              (if message
                  (handler-case
                      (let* ((process (sb-ext:run-program "/usr/bin/git"
                                                         (list "commit" "-m" message)
                                                         :output :stream
                                                         :error :stream
                                                         :wait t
                                                         :search nil))
                             (exit-code (or (sb-ext:process-exit-code process) 0)))
                        (when status-var
                          (setf (gethash status-var env) exit-code))
                        (when verbose
                          (format t "  Effect: GIT COMMIT '~A' (exit: ~A)~%" message exit-code)))
                    (error (e)
                      (when status-var (setf (gethash status-var env) 1))
                      (when verbose
                        (format t "  Effect: GIT COMMIT failed: ~A~%" e))))
                  (when verbose
                    (format t "  Effect: GIT COMMIT (invalid syntax)~%")))))
           
           ;; GIT BRANCH [CREATE "name" | DELETE "name"] [INTO output] WITH [STATUS|EXIT_CODE] var
           ((string= subcommand "branch")
            (let* ((after-branch (trim (subseq rest 6)))
                   (after-branch-upper (string-upcase after-branch))
                   (is-create (starts-with after-branch-upper "CREATE "))
                   (is-delete (starts-with after-branch-upper "DELETE "))
                   (operation (cond (is-create "create")
                                   (is-delete "delete")
                                   (t "list"))))
              (cond
               ;; GIT BRANCH CREATE "name" WITH STATUS result
               ((string= operation "create")
                (let* ((after-op (trim (subseq after-branch 7)))
                       (name-end (position #\" after-op :start 1))
                       (name (if name-end (subseq after-op 1 name-end) nil))
                       (after-name (if name-end (trim (subseq after-op (1+ name-end))) ""))
                       (status-pos (search " WITH STATUS " (string-upcase after-name)))
                       (status-var (when status-pos
                                    (trim (subseq after-name (+ status-pos 13))))))
                  (if name
                      (handler-case
                          (let* ((process (sb-ext:run-program "/usr/bin/git"
                                                             (list "branch" name)
                                                             :output :stream
                                                             :error :stream
                                                             :wait t
                                                             :search nil))
                                 (exit-code (or (sb-ext:process-exit-code process) 0)))
                            (when status-var
                              (setf (gethash status-var env) exit-code))
                            (when verbose
                              (format t "  Effect: GIT BRANCH CREATE ~A (exit: ~A)~%" name exit-code)))
                        (error (e)
                          (when status-var (setf (gethash status-var env) 1))
                          (when verbose
                            (format t "  Effect: GIT BRANCH CREATE failed: ~A~%" e))))
                      (when verbose
                        (format t "  Effect: GIT BRANCH CREATE (invalid syntax)~%")))))
               
               ;; GIT BRANCH DELETE "name" WITH STATUS result
               ((string= operation "delete")
                (let* ((after-op (trim (subseq after-branch 7)))
                       (name-end (position #\" after-op :start 1))
                       (name (if name-end (subseq after-op 1 name-end) nil))
                       (after-name (if name-end (trim (subseq after-op (1+ name-end))) ""))
                       (status-pos (search " WITH STATUS " (string-upcase after-name)))
                       (status-var (when status-pos
                                    (trim (subseq after-name (+ status-pos 13))))))
                  (if name
                      (handler-case
                          (let* ((process (sb-ext:run-program "/usr/bin/git"
                                                             (list "branch" "-d" name)
                                                             :output :stream
                                                             :error :stream
                                                             :wait t
                                                             :search nil))
                                 (exit-code (or (sb-ext:process-exit-code process) 0)))
                            (when status-var
                              (setf (gethash status-var env) exit-code))
                            (when verbose
                              (format t "  Effect: GIT BRANCH DELETE ~A (exit: ~A)~%" name exit-code)))
                        (error (e)
                          (when status-var (setf (gethash status-var env) 1))
                          (when verbose
                            (format t "  Effect: GIT BRANCH DELETE failed: ~A~%" e))))
                      (when verbose
                        (format t "  Effect: GIT BRANCH DELETE (invalid syntax)~%")))))
               
               ;; GIT BRANCH INTO output WITH EXIT_CODE code (list branches)
               (t
                (let* ((into-pos-space (search " INTO " after-branch-upper))
                       (into-pos-start (when (starts-with after-branch-upper "INTO ") 0))
                       (into-pos (or into-pos-space into-pos-start))
                       (exit-pos (search " WITH EXIT_CODE " after-branch-upper))
                       (output-var (when (and into-pos exit-pos)
                                    (let ((start (if (eql into-pos 0) 5 (+ into-pos 6))))
                                      (trim (subseq after-branch start exit-pos)))))
                       (exit-var (when exit-pos
                                  (trim (subseq after-branch (+ exit-pos 16))))))
                  (handler-case
                      (let* ((process (sb-ext:run-program "/usr/bin/git"
                                                         '("branch" "-a")
                                                         :output :stream
                                                         :error :stream
                                                         :wait t
                                                         :search nil))
                             (output-stream (sb-ext:process-output process))
                             (output (with-output-to-string (s)
                                      (loop for line = (read-line output-stream nil nil)
                                            while line
                                            do (format s "~A~%" line))))
                             (exit-code (or (sb-ext:process-exit-code process) 0)))
                        (when output-var
                          (setf (gethash output-var env) (string-trim '(#\Newline) output)))
                        (when exit-var
                          (setf (gethash exit-var env) exit-code))
                        (close output-stream)
                        (when verbose
                          (format t "  Effect: GIT BRANCH (exit: ~A)~%" exit-code)))
                    (error (e)
                      (when output-var (setf (gethash output-var env) ""))
                      (when exit-var (setf (gethash exit-var env) 1))
                      (when verbose
                        (format t "  Effect: GIT BRANCH failed: ~A~%" e)))))))))
           
           ;; GIT MERGE "branch" WITH STATUS result [AND OUTPUT output]
           ((string= subcommand "merge")
            (let* ((after-merge (trim (subseq rest 6)))
                   (branch-end (position #\" after-merge :start 1))
                   (branch (if branch-end (subseq after-merge 1 branch-end) nil))
                   (after-branch (if branch-end (trim (subseq after-merge (1+ branch-end))) ""))
                   (after-branch-upper (string-upcase after-branch))
                   (status-pos (search " WITH STATUS " after-branch-upper))
                   (and-output-pos (search " AND OUTPUT " after-branch-upper))
                   (status-var (when status-pos
                                (if and-output-pos
                                    (trim (subseq after-branch (+ status-pos 13) and-output-pos))
                                    (trim (subseq after-branch (+ status-pos 13))))))
                   (output-var (when and-output-pos
                                (trim (subseq after-branch (+ and-output-pos 12))))))
              (if branch
                  (handler-case
                      (let* ((process (sb-ext:run-program "/usr/bin/git"
                                                         (list "merge" branch)
                                                         :output :stream
                                                         :error :stream
                                                         :wait t
                                                         :search nil))
                             (output-stream (sb-ext:process-output process))
                             (error-stream (sb-ext:process-error process))
                             (output (with-output-to-string (s)
                                      (loop for line = (read-line output-stream nil nil)
                                            while line
                                            do (format s "~A~%" line))
                                      (loop for line = (read-line error-stream nil nil)
                                            while line
                                            do (format s "~A~%" line))))
                             (exit-code (or (sb-ext:process-exit-code process) 0)))
                        (when status-var
                          (setf (gethash status-var env) exit-code))
                        (when output-var
                          (setf (gethash output-var env) (string-trim '(#\Newline) output)))
                        (close output-stream)
                        (close error-stream)
                        (when verbose
                          (format t "  Effect: GIT MERGE ~A (exit: ~A)~%" branch exit-code)))
                    (error (e)
                      (when status-var (setf (gethash status-var env) 1))
                      (when output-var (setf (gethash output-var env) ""))
                      (when verbose
                        (format t "  Effect: GIT MERGE failed: ~A~%" e))))
                  (when verbose
                    (format t "  Effect: GIT MERGE (invalid syntax)~%")))))
           
           ;; GIT LOG ["options"] INTO output WITH EXIT_CODE code
           ((string= subcommand "log")
            (let* ((after-log (trim (subseq rest 3)))
                   (after-log-upper (string-upcase after-log))
                   ;; Check if options are specified
                   (options (when (and (> (length after-log) 0) (char= (char after-log 0) #\"))
                             (let ((opts-end (position #\" after-log :start 1)))
                               (if opts-end (subseq after-log 1 opts-end) nil))))
                   (after-opts (if options
                                  (trim (subseq after-log (+ (length options) 2)))
                                  after-log))
                   (after-opts-upper (string-upcase after-opts))
                   (into-pos-space (search " INTO " after-opts-upper))
                   (into-pos-start (when (starts-with after-opts-upper "INTO ") 0))
                   (into-pos (or into-pos-space into-pos-start))
                   (exit-pos (search " WITH EXIT_CODE " after-opts-upper))
                   (output-var (when (and into-pos exit-pos)
                                (let ((start (if (eql into-pos 0) 5 (+ into-pos 6))))
                                  (trim (subseq after-opts start exit-pos)))))
                   (exit-var (when exit-pos
                              (trim (subseq after-opts (+ exit-pos 16))))))
              (handler-case
                  (let* ((git-args (if options
                                      (cons "log" (split-string options #\Space))
                                      '("log" "--oneline" "-10")))
                         (process (sb-ext:run-program "/usr/bin/git"
                                                     git-args
                                                     :output :stream
                                                     :error :stream
                                                     :wait t
                                                     :search nil))
                         (output-stream (sb-ext:process-output process))
                         (output (with-output-to-string (s)
                                  (loop for line = (read-line output-stream nil nil)
                                        while line
                                        do (format s "~A~%" line))))
                         (exit-code (or (sb-ext:process-exit-code process) 0)))
                    (when output-var
                      (setf (gethash output-var env) (string-trim '(#\Newline) output)))
                    (when exit-var
                      (setf (gethash exit-var env) exit-code))
                    (close output-stream)
                    (when verbose
                      (format t "  Effect: GIT LOG~A (exit: ~A)~%" 
                              (if options (format nil " ~A" options) "") exit-code)))
                (error (e)
                  (when output-var (setf (gethash output-var env) ""))
                  (when exit-var (setf (gethash exit-var env) 1))
                  (when verbose
                    (format t "  Effect: GIT LOG failed: ~A~%" e))))))
           
           (t (when verbose
                (format t "  Effect: GIT (unknown subcommand)~%"))))))
      
      ;; Log (for error handling)
      ((starts-with (string-upcase trimmed) "LOG ")
       (let ((msg (trim (subseq trimmed 4))))
         (when verbose
           (format t "  Effect: Log ~A~%" msg))))
      
      ;; Default: just display
      (t (when verbose
           (format t "  Effect: ~A~%" effect-str))))))

;;; ============================================================================
;;; For Each Loop Execution
;;; ============================================================================

(defun execute-for-each (for-each-spec then-clauses effects env verbose)
  "Execute a For each loop.
   Syntax: 'For each item in list' or 'For each item, index in list'
   Examples:
   - For each month in months
   - For each value, index in numbers
   - For each name, score in names, scores"
  (let* ((spec-upper (string-upcase for-each-spec))
         (in-pos (search " IN " spec-upper)))
    (unless in-pos
      (error "For each syntax error: missing 'in'. Use 'For each item in list'"))
    
    ;; Parse variable names and list names
    (let* ((vars-part (trim (subseq for-each-spec 0 in-pos)))
           (lists-part (trim (subseq for-each-spec (+ in-pos 4))))
           (var-names (mapcar #'trim (split-string vars-part #\,)))
           (list-names (mapcar #'trim (split-string lists-part #\,))))
      
      (when verbose
        (format t "  Iterating: ~{~A~^, ~} over ~{~A~^, ~}~%" var-names list-names))
      
      ;; Get the lists from environment
      (let ((lists (mapcar (lambda (name)
                            (let ((val (gethash name env)))
                              (if (listp val)
                                  val
                                  (error (format nil "~A is not a list" name)))))
                          list-names)))
        
        ;; Verify all lists have the same length (or single list)
        (let ((lengths (mapcar #'length lists)))
          (unless (apply #'= lengths)
            (error (format nil "Lists have different lengths: ~A" lengths))))
        
        ;; Get the length
        (let ((list-length (length (car lists))))
          (when verbose
            (format t "  Loop iterations: ~A~%" list-length))
          
          ;; Loop over indices
          (loop for index from 0 below list-length do
                (when verbose
                  (format t "  [Iteration ~A]~%" (1+ index)))
                
                ;; Bind iteration variables
                (loop for var-name in var-names
                      for list in lists
                      do (setf (gethash var-name env) (nth index list))
                      do (when verbose
                           (format t "    ~A = ~A~%" var-name (nth index list))))
                
                ;; Execute Then clauses
                (dolist (then-clause then-clauses)
                  (eval-expr then-clause env "For each Then clause")
                  (when verbose
                    (format t "    Then: ~A~%" then-clause)))
                
                ;; Execute Effects
                (dolist (eff effects)
                  (apply-effect eff env verbose))))))))

;;; ============================================================================
;;; Interpreter - Execute CNS programs
;;; ============================================================================

;; Forward declaration
(declaim (ftype (function (t &key (:verbose t)) t) interpret-single-story))

(defun interpret-cns (ast-or-code &key (verbose t))
  "Interpret CNS code or AST with function support.
   Can accept either:
   - A string of CNS code (parsed automatically, supports functions)
   - A single AST (legacy mode, no function support)
   - A list of ASTs (multiple stories, supports functions)"
  (cond
   ;; String code - parse and handle functions
   ((stringp ast-or-code)
    (clear-functions)
    (let* ((parsed (parse-cns ast-or-code))
           (ast-list (if (and (listp (car parsed))
                              (listp (caar parsed)))
                         ;; Multiple stories: (((STORY ...) ...) ((STORY ...) ...))
                         parsed
                         ;; Single story: ((STORY ...) (GIVEN ...) ...)
                         (list parsed)))
           (entry-point-ast nil)
           (entry-point-name nil))
      
      ;; Register all functions and find entry point
      (dolist (ast ast-list)
        (let ((story-node (find 'story ast :key #'car)))
          (when story-node
            (let ((story-line (cadr story-node)))
              (multiple-value-bind (is-func func-name)
                  (is-function-story (format nil "Story: ~A" story-line))
                (if is-func
                    (progn
                      (register-function func-name ast)
                      (when verbose
                        (format t "Registered function: ~A~%" func-name)))
                    (progn
                      (setf entry-point-ast ast)
                      (setf entry-point-name story-line))))))))
      
      ;; If no entry point, use first story
      (unless entry-point-ast
        (setf entry-point-ast (car ast-list)))
      
      (when verbose
        (format t "~%=== Executing Entry Point ===~%"))
      
      (interpret-single-story entry-point-ast :verbose verbose)))
   
   ;; List of ASTs - handle as multi-story
   ((and (listp ast-or-code) 
         (listp (car ast-or-code))
         (eq (caar ast-or-code) 'story))
    (interpret-cns (format nil "~{~S~%~}" ast-or-code) :verbose verbose))
   
   ;; Single AST - legacy mode
   (t
    (interpret-single-story ast-or-code :verbose verbose))))

(defun interpret-single-story (ast &key (verbose t))
  "Interpret a single CNS story AST.
   This is the core interpreter logic."
  (let ((env (make-hash-table :test #'equal))  ; State variables
        (steps '())                           ; List of steps
        (pc 0)                                ; Program counter (step index)
        (result nil)
        (error-block nil)
        (*current-step* nil))                 ; Track current step for errors
    
    ;; Phase 1: Collect sections
    (dolist (node ast)
      (case (car node)
        (story 
         ;; Check for strict mode flag
         (when (and (cddr node) (eql (caddr node) :strict))
           (setf *strict-mode* t))
         (when verbose
           (format t "~%=== Executing Story: ~A ===~%~A" (cadr node)
                  (if *strict-mode* " [STRICT MODE]~%" "~%"))))
        
         (given 
          (when verbose (format t "~%Given:~%"))
          (dolist (var (cdr node))
            (let ((name (cadr var))
                  (type (caddr var))
                  (val (cadddr var)))
               ;; Parse value intelligently:
              ;; - If val is a list literal [x, y, z], evaluate it
              ;; - If val is a quoted string, use it directly
              ;; - Otherwise evaluate it as an expression
              (setf (gethash name env) 
                    (if val 
                        ;; Always evaluate the value - eval-expr handles strings, numbers, lists, etc.
                        (eval-expr val env)
                        nil))
              (when verbose
                (format t "  ~A: ~A = ~A~%" name type (gethash name env))))))
        
        (step (push node steps))
        
        (error (setf error-block node))
        
        (end (setf result node))))
    
    (setf steps (nreverse steps))  ; Steps in order
    
    ;; Phase 2: Execute steps in loop (with error handling)
    (when verbose (format t "~%Execution Trace:~%"))
    (setf *iteration-counter* 0)  ; Reset counter at start of execution
    (handler-case
        (loop while (< pc (length steps)) do
            ;; Check iteration limit
            (incf *iteration-counter*)
            (when (> *iteration-counter* *max-iterations*)
              (error (cns-error-iteration-limit *iteration-counter* 
                                               (if (< pc (length steps)) 
                                                   (cadr (nth pc steps))
                                                   "Unknown")
                                               env)))
            (let* ((step (nth pc steps))
                   (step-num (cadr step))
                   (step-body (cddr step))
                   (action (cadr (assoc 'action step-body)))
                   (for-each-node (assoc 'for-each step-body))
                   (because (cadr (assoc 'because step-body)))
                   (if-node (assoc 'if step-body))
                   (otherwise-pos (position 'otherwise step-body :key #'car))
                   ;; Split step-body into if-branch and otherwise-branch
                   (if-branch-body (if otherwise-pos
                                     (subseq step-body 0 otherwise-pos)
                                     step-body))
                   (otherwise-branch-body (when otherwise-pos
                                           (subseq step-body (1+ otherwise-pos))))
                   ;; Extract clauses from appropriate branches
                   (then-clauses (mapcar #'cadr (remove-if-not (lambda (x) (eq (car x) 'then)) 
                                                               (if if-node if-branch-body step-body))))
                   (effects (mapcar #'cadr (remove-if-not (lambda (x) (eq (car x) 'effect)) 
                                                          (if if-node if-branch-body step-body))))
                   (otherwise-then-clauses (mapcar #'cadr (remove-if-not (lambda (x) (eq (car x) 'then)) 
                                                                         otherwise-branch-body)))
                   (otherwise-effects (mapcar #'cadr (remove-if-not (lambda (x) (eq (car x) 'effect)) 
                                                                    otherwise-branch-body)))
                   (otherwise-clause (cadr (assoc 'otherwise step-body))))
              
              ;; Set current step for error reporting
              (setf *current-step* step-num)
              
              (when verbose
                ;; Display step - handle actions, conditionals, and for-each
                (cond
                 (for-each-node
                  (format t "~%Step ~A: For each ~A~%" step-num (cadr for-each-node)))
                 (if-node
                  (format t "~%Step ~A: If ~A~%" step-num (cadr if-node)))
                 (t
                  (format t "~%Step ~A: ~A~%" step-num action)))
                (when because
                  (format t "  Because: ~A~%" because)))
             
               ;; Execute action (only if it's a standalone action with no Then/If/For-each/Effects)
               ;; If there are Then/Effect clauses, the action is just a description
               (when (and action 
                         (not for-each-node) 
                         (not if-node)
                         (null then-clauses)   ; No Then clauses
                         (null effects))       ; No Effect clauses
                 (eval-expr action env (format nil "Step ~A action" step-num)))
               
               ;; Handle For each loop
               (when for-each-node
                 (let ((for-each-spec (cadr for-each-node)))
                   (execute-for-each for-each-spec then-clauses effects env verbose)))
              
              ;; Execute Then clauses if present (only for non-conditional, non-for-each steps)
              (when (and (not if-node) (not for-each-node))
                (dolist (then-clause then-clauses)
                  ;; Check if this is an effect keyword that should use apply-effect
                  (if (or (starts-with (string-upcase (trim then-clause)) "PRINT ")
                          (starts-with (string-upcase (trim then-clause)) "DISPLAY ")
                          (starts-with (string-upcase (trim then-clause)) "HTTP ")
                          (starts-with (string-upcase (trim then-clause)) "HTTPS ")
                          (starts-with (string-upcase (trim then-clause)) "SHELL ")
                          (starts-with (string-upcase (trim then-clause)) "FIND ")
                          (starts-with (string-upcase (trim then-clause)) "GREP ")
                          (starts-with (string-upcase (trim then-clause)) "GIT ")
                          (starts-with (string-upcase (trim then-clause)) "CSV ")
                          (starts-with (string-upcase (trim then-clause)) "SQL ")
                          (starts-with (string-upcase (trim then-clause)) "SOCKET "))
                      (apply-effect then-clause env verbose)
                      (eval-expr then-clause env (format nil "Step ~A Then clause" step-num)))
                  (when verbose
                    (format t "  Then: ~A~%" then-clause))))
              
               ;; Apply effects (real implementation) - not for for-each or conditionals (handled in branches)
               (when (and (not for-each-node) (not if-node))
                 (dolist (eff effects)
                   (apply-effect eff env verbose)))
              
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
              (cond
               ;; For-each loop - just increment PC after execution
               (for-each-node
                (incf pc))
               
               ;; Conditional step
               (if-node
                (let ((cond-expr (cadr if-node)))
                  (if (eval-expr cond-expr env (format nil "Step ~A condition" step-num))
                      ;; Condition true - execute Then clauses
                      (progn
                         ;; Execute all Then clauses for conditional
                         (dolist (then-clause then-clauses)
                           ;; Check if it's a control flow or assignment
                            (cond
                             ((search "repeat from Step" then-clause :test #'char-equal)
                              ;; Handle later in control flow
                              nil)
                             ((search "go to Step" then-clause :test #'char-equal)
                              ;; Handle later in control flow
                              nil)
                             ((search "go to End" then-clause :test #'char-equal)
                              ;; Handle later in control flow
                              nil)
                             ;; Print/Display effects in Then clauses
                             ((or (starts-with (string-upcase (trim then-clause)) "PRINT ")
                                  (starts-with (string-upcase (trim then-clause)) "DISPLAY ")
                                  (starts-with (string-upcase (trim then-clause)) "HTTP ")
                                  (starts-with (string-upcase (trim then-clause)) "HTTPS ")
                                  (starts-with (string-upcase (trim then-clause)) "SHELL ")
                                  (starts-with (string-upcase (trim then-clause)) "FIND ")
                                  (starts-with (string-upcase (trim then-clause)) "GREP ")
                                  (starts-with (string-upcase (trim then-clause)) "GIT ")
                                  (starts-with (string-upcase (trim then-clause)) "CSV ")
                                  (starts-with (string-upcase (trim then-clause)) "SQL ")
                                  (starts-with (string-upcase (trim then-clause)) "SOCKET "))
                              (apply-effect then-clause env verbose))
                            (t
                             ;; Regular assignment or expression
                             (eval-expr then-clause env (format nil "Step ~A Then clause" step-num))
                             (when verbose
                               (format t "  Then: ~A~%" then-clause)))))
                        
                        ;; Execute effects for true branch
                        (dolist (eff effects)
                          (apply-effect eff env verbose))
                        
                         ;; Check last Then clause for control flow
                         (let ((last-then (car (last then-clauses))))
                           (cond
                             ;; repeat from Step X
                             ((and last-then (search "repeat from Step" last-then :test #'char-equal))
                              (let* ((step-pos (search "Step " last-then))
                                     (num-start (+ step-pos 5))
                                     (target-step (parse-integer last-then :start num-start :junk-allowed t)))
                                (when verbose
                                  (format t "  -> Jumping to Step ~A~%" target-step))
                                (setf pc (1- target-step))))
                             ;; go to Step X
                             ((and last-then (search "go to Step" last-then :test #'char-equal))
                              (let* ((step-pos (search "Step " last-then))
                                     (num-start (+ step-pos 5))
                                     (target-step (parse-integer last-then :start num-start :junk-allowed t)))
                                (when verbose
                                  (format t "  -> Going to Step ~A~%" target-step))
                                (setf pc (1- target-step))))
                             ;; go to End
                             ((and last-then (search "go to End" last-then :test #'char-equal))
                              (when verbose
                                (format t "  -> Going to End~%"))
                              (return))
                             ;; No control flow, just increment
                             (t (incf pc)))))
                    ;; Condition false - handle otherwise
                    (cond
                      ((and otherwise-clause (search "go to End" otherwise-clause :test #'char-equal))
                       (when verbose
                         (format t "  -> Going to End~%"))
                       (return))
                      ((and otherwise-clause (search "go to Step" otherwise-clause :test #'char-equal))
                       (let* ((step-pos (search "Step " otherwise-clause))
                              (num-start (+ step-pos 5))
                              (target-step (parse-integer otherwise-clause :start num-start :junk-allowed t)))
                         (when verbose
                           (format t "  -> Going to Step ~A~%"target-step))
                         (setf pc (1- target-step))))
                      ((and otherwise-clause (search "repeat from Step" otherwise-clause :test #'char-equal))
                       (let* ((step-pos (search "Step " otherwise-clause))
                              (num-start (+ step-pos 5))
                              (target-step (parse-integer otherwise-clause :start num-start :junk-allowed t)))
                         (when verbose
                           (format t "  -> Repeating from Step ~A~%" target-step))
                         (setf pc (1- target-step))))
                      (t
                       ;; Execute otherwise-branch Then and Effect clauses
                       (dolist (then-clause otherwise-then-clauses)
                         (cond
                          ((search "repeat from Step" then-clause :test #'char-equal)
                           (let* ((step-pos (search "Step " then-clause))
                                  (num-start (+ step-pos 5))
                                  (target-step (parse-integer then-clause :start num-start :junk-allowed t)))
                             (when verbose
                               (format t "  Then: ~A~%" then-clause)
                               (format t "  -> Repeating from Step ~A~%" target-step))
                             (setf pc (1- target-step))
                             (return)))
                          ((search "go to Step" then-clause :test #'char-equal)
                           (let* ((step-pos (search "Step " then-clause))
                                  (num-start (+ step-pos 5))
                                  (target-step (parse-integer then-clause :start num-start :junk-allowed t)))
                             (when verbose
                               (format t "  Then: ~A~%" then-clause)
                               (format t "  -> Going to Step ~A~%" target-step))
                             (setf pc (1- target-step))
                             (return)))
                           ((search "go to End" then-clause :test #'char-equal)
                            (when verbose
                              (format t "  Then: ~A~%" then-clause)
                              (format t "  -> Going to End~%"))
                            (setf pc (length steps))  ; Force loop exit
                            (return))
                          ;; Check if it's an effect keyword
                          ((or (starts-with (string-upcase (trim then-clause)) "PRINT ")
                               (starts-with (string-upcase (trim then-clause)) "DISPLAY ")
                               (starts-with (string-upcase (trim then-clause)) "HTTP ")
                               (starts-with (string-upcase (trim then-clause)) "HTTPS ")
                               (starts-with (string-upcase (trim then-clause)) "SHELL ")
                               (starts-with (string-upcase (trim then-clause)) "FIND ")
                               (starts-with (string-upcase (trim then-clause)) "GREP ")
                               (starts-with (string-upcase (trim then-clause)) "GIT ")
                               (starts-with (string-upcase (trim then-clause)) "CSV ")
                               (starts-with (string-upcase (trim then-clause)) "SQL ")
                               (starts-with (string-upcase (trim then-clause)) "SOCKET "))
                           (apply-effect then-clause env verbose)
                           (when verbose
                             (format t "  Then: ~A~%" then-clause)))
                          (t
                           (eval-expr then-clause env (format nil "Step ~A Otherwise Then clause" step-num))
                           (when verbose
                             (format t "  Then: ~A~%" then-clause)))))
                       (dolist (eff otherwise-effects)
                         (apply-effect eff env verbose))
                       ;; Check if last then-clause had control flow
                       (unless (and otherwise-then-clauses
                                  (or (search "repeat from" (car (last otherwise-then-clauses)) :test #'char-equal)
                                      (search "go to" (car (last otherwise-then-clauses)) :test #'char-equal)))
                         (incf pc)))))))
               
               ;; Regular step - just advance
               (t (incf pc)))))
      
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
;;; Validation Functions
;;; ============================================================================

(defun validate-cns (ast)
  "Validate CNS AST for completeness and correctness.
   Returns (values valid-p error-messages)"
  (let ((errors '())
        (has-story nil)
        (has-given nil)
        (has-end nil)
        (declared-vars '())
        (steps '()))
    
    ;; Check structure
    (dolist (node ast)
      (case (car node)
        (story (setf has-story t))
        (given (setf has-given t)
               (dolist (var (cdr node))
                 (push (cadr var) declared-vars)))
        (step (push node steps))
        (end (setf has-end t))))
    
    ;; Check required sections
    (unless has-story
      (push "Missing Story: section" errors))
    (unless has-given
      (push "Missing Given: section (declare variables)" errors))
    (unless has-end
      (push "Missing End: section" errors))
    
    ;; Check steps have Because clauses
    (dolist (step steps)
      (let* ((step-num (cadr step))
             (step-body (cddr step))
             (because (assoc 'because step-body)))
        (unless because
          (push (format nil "Step ~A missing Because: clause" step-num) errors))))
    
    ;; Check for sequential step numbering (allow duplicates for alternate branches)
    (when steps
      (let* ((step-nums (mapcar #'cadr steps))
             (unique-nums (remove-duplicates step-nums))
             (sorted-unique (sort unique-nums #'<))
             (max-step (car (last sorted-unique))))
        (when max-step
          (loop for i from 1 to max-step
                unless (member i sorted-unique)
                do (push (format nil "Steps not sequential: missing Step ~A" i) errors)))))
    
    ;; Return validation result
    (values (null errors) (nreverse errors))))

(defun validate-cns-file (filepath)
  "Load and validate a CNS file, returning validation report."
  (handler-case
      (with-open-file (stream filepath)
        (let* ((code (make-string (file-length stream))))
          (read-sequence code stream)
          (let ((ast (parse-cns code)))
            (multiple-value-bind (valid-p errors)
                (validate-cns ast)
              (if valid-p
                  (format t "✓ ~A is valid CNS code~%" filepath)
                  (progn
                    (format t "✗ ~A has validation errors:~%" filepath)
                    (dolist (err errors)
                      (format t "  - ~A~%" err))))
              valid-p))))
    (error (e)
      (format t "✗ Failed to parse ~A: ~A~%" filepath e)
      nil)))

(defun validate-all-examples ()
  "Validate all example CNS files."
  (let ((example-dir "examples/")
        (valid-count 0)
        (invalid-count 0))
    (dolist (file (directory (concatenate 'string example-dir "*.cns")))
      (if (validate-cns-file (namestring file))
          (incf valid-count)
          (incf invalid-count)))
    (format t "~%Summary: ~A valid, ~A invalid~%" valid-count invalid-count)))

;;; ============================================================================
;;; Feedback Loop for LLM Error Correction
;;; ============================================================================

(defun extract-error-message (condition)
  "Extract human-readable error message from condition."
  (format nil "~A" condition))

(defun generate-correction-prompt (cns-code error-message)
  "Generate a prompt for LLM to fix CNS code based on error."
  (format nil "The following CNS code has an error:

```cns
~A
```

Error: ~A

Please fix the code to resolve this error. Output only the corrected CNS code.
" cns-code error-message))

(defun validate-and-correct (cns-code &key (max-retries 3) (verbose t))
  "Attempt to parse and validate CNS code, generating correction prompts if needed.
   Returns (values success-p final-code attempts error-prompts)"
  (let ((attempts 0)
        (current-code cns-code)
        (error-prompts '()))
    
    (loop while (< attempts max-retries) do
          (incf attempts)
          (when verbose
            (format t "~%Attempt ~A/~A...~%" attempts max-retries))
          
          (handler-case
              (let ((ast (parse-cns current-code)))
                ;; Try to validate
                (multiple-value-bind (valid-p errors)
                    (validate-cns ast)
                  (if valid-p
                      (progn
                        (when verbose
                          (format t "✓ Code is valid!~%"))
                        (return (values t current-code attempts error-prompts)))
                      (progn
                        (when verbose
                          (format t "✗ Validation errors:~%")
                          (dolist (err errors)
                            (format t "  - ~A~%" err)))
                        (let ((prompt (generate-correction-prompt 
                                      current-code 
                                      (format nil "~{~A~^; ~}" errors))))
                          (push prompt error-prompts)
                          (when verbose
                            (format t "~%Generated correction prompt.~%"))
                          ;; In real use, this would call an LLM API
                          ;; For now, just return failure
                          (return (values nil current-code attempts (nreverse error-prompts))))))))
            (error (e)
              (when verbose
                (format t "✗ Parse error: ~A~%" e))
              (let ((prompt (generate-correction-prompt 
                            current-code 
                            (extract-error-message e))))
                (push prompt error-prompts)
                (when verbose
                  (format t "~%Generated correction prompt.~%"))
                ;; In real use, would call LLM here
                (return (values nil current-code attempts (nreverse error-prompts)))))))
    
    ;; Max retries exceeded
    (when verbose
      (format t "~%Max retries (~A) exceeded.~%" max-retries))
    (values nil current-code attempts (nreverse error-prompts))))

(defun save-correction-prompts (prompts filepath)
  "Save correction prompts to a file for LLM processing."
  (with-open-file (stream filepath :direction :output 
                         :if-exists :supersede
                         :if-does-not-exist :create)
    (loop for i from 1
          for prompt in prompts
          do (format stream "=== Correction Attempt ~A ===~%~%~A~%~%" i prompt))))

;;; ============================================================================
;;; Convenience Functions
;;; ============================================================================

(defun load-cns-file (filepath)
  "Load and execute a CNS file."
  (with-open-file (stream filepath)
    (let ((code (make-string (file-length stream))))
      (read-sequence code stream)
      ;; Pass code directly to interpret-cns for function support
      (interpret-cns code))))

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
