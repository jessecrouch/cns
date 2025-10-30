;;; CNS Interpreter in Common Lisp
;;; A minimal implementation of Causal Narrative Script (CNS)
;;; CNS is a programming language optimized for LLM comprehension
;;; with explicit causality, narrative flow, and self-documenting structures.

;;; Load socket support
(require 'sb-bsd-sockets)

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
                    (values result (subseq str (1+ i)))))
                 (t
                  (vector-push-extend ch result)))
                (incf i)))
        ;; If we get here, string was not closed
        (values result "")))))

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
        (list :request
              :method method 
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
          (push `(story ,(trim (subseq trimmed 6))) ast)
          (setf current-section :story))
         
         ;; Given section (variable declarations)
         ((starts-with trimmed "Given:")
          (setf current-section :given)
          (push (list 'given) ast))
         
          ;; Variable declaration in Given section
          ((and (eql current-section :given) indented)
           (let* ((parts (split-string trimmed #\:))
                  (name (trim (car parts)))
                  (rest (cdr parts)))
             (when rest
               (let* ((type-and-value (trim (car rest)))
                      ;; First, split by = to separate type/value
                      (type-val-parts (split-string type-and-value #\=))
                      (type-part (trim (car type-val-parts)))
                      (val-part (if (cdr type-val-parts) (trim (cadr type-val-parts)) nil))
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
                              ;; Check if value is a quoted string
                              (if (and (> (length val-part) 0) (char= (char val-part 0) #\"))
                                  ;; Extract and re-wrap with quotes to preserve string literal
                                  (format nil "\"~A\"" (extract-quoted-string val-part))
                                  val-part)
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
           (let ((cond-str (trim (subseq trimmed 2))))
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
                  (return-value (if (starts-with (string-upcase end-content) "RETURN")
                                    (trim (subseq end-content 6))
                                    end-content)))
             (push `(end (return ,return-value) (because "computation complete")) ast))))))
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
                  (let* ((step (nth pc steps))
                         (step-num (cadr step))
                         (step-body (cddr step))
                         (then-clauses (mapcar #'cadr (remove-if-not (lambda (x) (eq (car x) 'then)) step-body)))
                         (effects (mapcar #'cadr (remove-if-not (lambda (x) (eq (car x) 'effect)) step-body)))
                         (if-node (assoc 'if step-body))
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
                                 (t
                                  (eval-expr then-clause func-env))))
                              (unless (or (search "repeat from" (car (last then-clauses)) :test #'char-equal)
                                          (search "go to" (car (last then-clauses)) :test #'char-equal))
                                (incf pc)))
                            ;; False branch
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
                             (t (incf pc))))))
                     
                     ;; Regular step
                     (t
                      (dolist (then-clause then-clauses)
                        (eval-expr then-clause func-env))
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
              ((and (> (length trimmed) 1)
                    (char= (char trimmed 0) #\")
                    (char= (char trimmed (1- (length trimmed))) #\"))
               (subseq trimmed 1 (1- (length trimmed))))
             
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
            
            ;; List literal: [1, 2, 3]
            ((and (> (length trimmed) 1)
                  (char= (char trimmed 0) #\[)
                  (char= (char trimmed (1- (length trimmed))) #\]))
             (let* ((content (subseq trimmed 1 (1- (length trimmed))))
                    (items (split-string content #\,)))
               (mapcar (lambda (item) (eval-expr (trim item) env)) items)))
            
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
             ((search "≠" trimmed)
              (let ((parts (split-string trimmed #\≠)))
                (/= (eval-expr (trim (car parts)) env)
                    (eval-expr (trim (cadr parts)) env))))
            
            ;; Comparison: n = 1 (must come after ≠, ≤, ≥, <=, >=)
            ;; Make sure = is not part of <=, >=
            ((and (position #\= trimmed)
                  (not (search "<=" trimmed))
                  (not (search ">=" trimmed)))
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
      ((starts-with (string-upcase trimmed) "ACCEPT CONNECTION")
       (let* ((rest (if (> (length trimmed) 17)
                       (trim (subseq trimmed 17))
                       ""))
              (rest-upper (string-upcase rest))
              (on-pos (when (> (length rest-upper) 0)
                       (search "ON" rest-upper))))
         (if on-pos
             (let* ((socket-name (trim (subseq rest (+ on-pos 2))))  ; Skip "on"
                    (server-socket (gethash socket-name env)))
               (if server-socket
                   (handler-case
                       (multiple-value-bind (client-socket client-stream)
                           (accept-connection server-socket)
                         ;; Store both socket and stream
                         (setf (gethash "client_socket" env) client-socket)
                         (setf (gethash "client_stream" env) client-stream)
                         (when verbose
                           (format t "  Effect: Accepted REAL connection from client~%")))
                     (error (e)
                       (when verbose
                         (format t "  Effect: Failed to accept connection: ~A~%" e))))
                   (when verbose
                     (format t "  Effect: Accept connection (socket '~A' not found)~%" socket-name))))
             (when verbose
               (format t "  Effect: Accept connection (no socket specified)~%")))))
      
      ;; Socket: Network read (REAL implementation)
      ((starts-with (string-upcase trimmed) "NETWORK READ")
       (let ((stream (gethash "client_stream" env)))
         (if stream
             (handler-case
                 (let ((data (socket-receive stream)))
                   (setf (gethash "request_data" env) data)
                   (when verbose
                     (format t "  Effect: Read REAL network data (~A bytes)~%" 
                             (if data (length data) 0))))
               (error (e)
                 (when verbose
                   (format t "  Effect: Failed to read from network: ~A~%" e))))
             (when verbose
               (format t "  Effect: Network read (no stream available)~%")))))
      
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
;;; Interpreter: Execute the AST
;;; ============================================================================

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
           (ast-list (if (listp (car parsed))
                         ;; Multiple stories
                         parsed
                         ;; Single story wrapped in list
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
               ;; Parse value intelligently:
              ;; - If it contains newlines, it's from extract-quoted-string -> keep as string
              ;; - If it's a simple number, parse it
              ;; - Otherwise evaluate it
              (setf (gethash name env) 
                    (if val 
                        (if (and (stringp val) (position #\Newline val))
                            val  ; Multiline string from extract-quoted-string
                            (eval-expr val env))  ; Number or expression
                        nil))
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
                   (for-each-node (assoc 'for-each step-body))
                   (because (cadr (assoc 'because step-body)))
                   (then-clauses (mapcar #'cadr (remove-if-not (lambda (x) (eq (car x) 'then)) step-body)))
                   (effects (mapcar #'cadr (remove-if-not (lambda (x) (eq (car x) 'effect)) step-body)))
                   (if-node (assoc 'if step-body))
                   (otherwise-clause (cadr (assoc 'otherwise step-body))))
              
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
             
               ;; Execute action (if not a conditional or for-each)
               (when (and action (not for-each-node))
                 (eval-expr action env (format nil "Step ~A action" step-num)))
               
               ;; Handle For each loop
               (when for-each-node
                 (let ((for-each-spec (cadr for-each-node)))
                   (execute-for-each for-each-spec then-clauses effects env verbose)))
              
              ;; Execute Then clauses if present (only for non-conditional, non-for-each steps)
              (when (and (not if-node) (not for-each-node))
                (dolist (then-clause then-clauses)
                  (eval-expr then-clause env (format nil "Step ~A Then clause" step-num))
                  (when verbose
                    (format t "  Then: ~A~%" then-clause))))
              
               ;; Apply effects (real implementation) - not for for-each (handled in loop)
               (when (not for-each-node)
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
                           (t
                            ;; Regular assignment or expression
                            (eval-expr then-clause env (format nil "Step ~A Then clause" step-num))
                            (when verbose
                              (format t "  Then: ~A~%" then-clause)))))
                        
                         ;; Check last Then clause for control flow
                         (let ((last-then (car (last then-clauses))))
                           (if (and last-then (search "repeat from Step" last-then :test #'char-equal))
                              (let* ((step-pos (search "Step " last-then))
                                     (num-start (+ step-pos 5))
                                     (target-step (parse-integer last-then :start num-start :junk-allowed t)))
                                (when verbose
                                  (format t "  -> Jumping to Step ~A~%" target-step))
                                (setf pc (1- target-step)))
                               (if (and last-then (search "go to Step" last-then :test #'char-equal))
                                  (let* ((step-pos (search "Step " last-then))
                                         (num-start (+ step-pos 5))
                                         (target-step (parse-integer last-then :start num-start :junk-allowed t)))
                                    (when verbose
                                      (format t "  -> Going to Step ~A~%" target-step))
                                    (setf pc (1- target-step)))
                                  (incf pc)))))
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
                      (t (incf pc))))))
               
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
