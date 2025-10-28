;;; Dataset Generation for CNS Training Data
;;; Converts CNS examples into JSON format for LLM training
;;; Output format suitable for fine-tuning or few-shot learning

(load "cns.lisp")

;;; JSON utilities (simple, no external dependencies)
(defun escape-json-string (str)
  "Escape special characters for JSON strings."
  (with-output-to-string (out)
    (loop for char across str do
      (case char
        (#\" (write-string "\\\"" out))
        (#\\ (write-string "\\\\" out))
        (#\Newline (write-string "\\n" out))
        (#\Return (write-string "\\r" out))
        (#\Tab (write-string "\\t" out))
        (t (write-char char out))))))

(defun write-json-string (str stream)
  "Write string as JSON to stream."
  (format stream "\"~A\"" (escape-json-string str)))

(defun write-json-object (obj stream &key (indent 0))
  "Write a plist as JSON object."
  (let ((indent-str (make-string indent :initial-element #\Space)))
    (format stream "{~%")
    (loop for (key value) on obj by #'cddr
          for first = t then nil
          do (progn
               (unless first (format stream ",~%"))
               (format stream "~A  \"~A\": " indent-str (string-downcase (symbol-name key)))
               (cond
                 ((stringp value) (write-json-string value stream))
                 ((numberp value) (format stream "~A" value))
                 ((eq value t) (format stream "true"))
                 ((eq value nil) (format stream "false"))
                 ((listp value) (write-json-array value stream :indent (+ indent 2)))
                 (t (write-json-string (format nil "~A" value) stream)))))
    (format stream "~%~A}" indent-str)))

(defun write-json-array (arr stream &key (indent 0))
  "Write list as JSON array."
  (let ((indent-str (make-string indent :initial-element #\Space)))
    (if (null arr)
        (format stream "[]")
        (progn
          (format stream "[")
          (loop for item in arr
                for first = t then nil
                do (progn
                     (unless first (format stream ", "))
                     (cond
                       ((stringp item) (write-json-string item stream))
                       ((numberp item) (format stream "~A" item))
                       ((listp item) (write-json-object item stream :indent (+ indent 2)))
                       (t (write-json-string (format nil "~A" item) stream)))))
          (format stream "]")))))

;;; Dataset entry structure
(defstruct dataset-entry
  id            ; Unique identifier
  prompt        ; Natural language task description
  cns-code      ; CNS implementation
  category      ; webserver, math, file-io, list, etc.
  tags          ; List of feature tags
  difficulty    ; easy, medium, hard
  expected      ; Expected output/behavior description
  notes)        ; Additional notes

;;; Categorization and metadata
(defun infer-category (filepath)
  "Infer category from filename or content."
  (let ((name (pathname-name filepath)))
    (cond
      ((search "webserver" name) "webserver")
      ((search "http" name) "webserver")
      ((or (search "factorial" name) (search "fibonacci" name) 
           (search "prime" name) (search "gcd" name)) "math")
      ((search "file" name) "file-io")
      ((search "list" name) "list")
      ((search "boolean" name) "boolean")
      ((search "print" name) "io")
      (t "general"))))

(defun infer-tags (filepath content)
  "Infer feature tags from content analysis."
  (let ((tags '()))
    (when (search "Step" content) (push "loops" tags))
    (when (search "Effect:" content) (push "effects" tags))
    (when (search "If" content) (push "conditionals" tags))
    (when (or (search "socket" content) (search "Socket" content)) 
      (push "networking" tags))
    (when (or (search "Print" content) (search "Write" content)) 
      (push "io" tags))
    (when (search "[" content) (push "lists" tags))
    (when (search "HTTP" content) (push "http" tags))
    (nreverse tags)))

(defun infer-difficulty (content)
  "Infer difficulty level from code complexity."
  (let ((lines (length (split-string content #\Newline)))
        (steps (count #\→ content)))
    (cond
      ((and (< lines 15) (<= steps 2)) "easy")
      ((and (< lines 40) (<= steps 5)) "medium")
      (t "hard"))))

(defun generate-prompt-from-story (cns-code)
  "Extract or generate natural language prompt from Story line."
  (let ((story-start (search "Story:" cns-code)))
    (when story-start
      (let* ((story-line-end (position #\Newline cns-code :start story-start))
             (story-text (subseq cns-code (+ story-start 6) story-line-end)))
        (trim story-text)))))

(defun generate-expected-output (cns-code category)
  "Generate description of expected output/behavior."
  (cond
    ((string= category "webserver")
     "Server responds to HTTP requests on specified port with configured routes")
    ((string= category "math")
     "Computes mathematical result with step-by-step causality")
    ((string= category "file-io")
     "Reads or writes files with explicit I/O effects")
    ((string= category "list")
     "Manipulates list data structures")
    (t "Executes narrative program with traceable steps")))

;;; Main conversion function
(defun convert-cns-file-to-entry (filepath)
  "Convert a CNS file to a dataset entry."
  (handler-case
      (with-open-file (stream filepath)
        (let* ((code (make-string (file-length stream)))
               (id (pathname-name filepath))
               (category (infer-category filepath)))
          (read-sequence code stream)
          (let ((prompt (generate-prompt-from-story code))
                (tags (infer-tags filepath code))
                (difficulty (infer-difficulty code))
                (expected (generate-expected-output code category)))
            (make-dataset-entry
             :id id
             :prompt (or prompt (format nil "Implement ~A" id))
             :cns-code code
             :category category
             :tags tags
             :difficulty difficulty
             :expected expected
             :notes ""))))
    (error (e)
      (format t "Error processing ~A: ~A~%" filepath e)
      nil)))

(defun dataset-entry-to-plist (entry)
  "Convert dataset entry to plist for JSON export."
  (list :id (dataset-entry-id entry)
        :prompt (dataset-entry-prompt entry)
        :cns-code (dataset-entry-cns-code entry)
        :category (dataset-entry-category entry)
        :tags (dataset-entry-tags entry)
        :difficulty (dataset-entry-difficulty entry)
        :expected (dataset-entry-expected entry)
        :notes (dataset-entry-notes entry)))

;;; Dataset generation
(defun generate-dataset-from-directory (dir-path output-file)
  "Generate dataset from all .cns files in directory."
  (let ((entries '())
        (files (directory (concatenate 'string dir-path "*.cns"))))
    
    (format t "~%Generating dataset from ~A files in ~A~%" (length files) dir-path)
    
    (dolist (file files)
      (format t "  Processing ~A..." (pathname-name file))
      (let ((entry (convert-cns-file-to-entry file)))
        (if entry
            (progn
              (push entry entries)
              (format t " ✓~%"))
            (format t " ✗~%"))))
    
    (setf entries (nreverse entries))
    
    ;; Write JSON output
    (with-open-file (out output-file 
                         :direction :output 
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (format out "{~%")
      (format out "  \"version\": \"1.0\",~%")
      (format out "  \"description\": \"CNS training dataset for LLM code generation\",~%")
      (format out "  \"generated\": \"~A\",~%" 
              (multiple-value-bind (sec min hour day month year)
                  (get-decoded-time)
                (format nil "~4,'0D-~2,'0D-~2,'0D" year month day)))
      (format out "  \"count\": ~A,~%" (length entries))
      (format out "  \"examples\": [~%")
      
      (loop for entry in entries
            for first = t then nil
            do (progn
                 (unless first (format out ",~%"))
                 (write-json-object (dataset-entry-to-plist entry) out :indent 4)))
      
      (format out "~%  ]~%")
      (format out "}~%"))
    
    (format t "~%✓ Generated dataset with ~A entries -> ~A~%" 
            (length entries) output-file)
    entries))

;;; Specialized webserver dataset generator
(defun generate-webserver-variant (base-template modifications)
  "Generate a webserver variant by applying modifications to base template.
   Modifications: (:port 8080 :route '(method url response) :feature 'query-params)"
  (let ((port (getf modifications :port 8080))
        (routes (getf modifications :routes '(("GET" "/" "Hello, World!"))))
        (feature (getf modifications :feature 'basic))
        (story (getf modifications :story "Run a simple webserver")))
    
    (with-output-to-string (code)
      (format code "Story: ~A~%~%" story)
      (format code "Given:~%")
      (format code "  port: Integer = ~A [network port]~%" port)
      (format code "  routes: List = ~A [URL to response mappings]~%" routes)
      (format code "  server_socket: Socket [network listener]~%")
      (format code "  connection_count: Integer = 0 [track connections]~%")
      (format code "~%")
      
      ;; Add feature-specific variables
      (case feature
        (query-params
         (format code "  query_params: List [parsed query parameters]~%"))
        (post-data
         (format code "  post_data: String [request body]~%")))
      
      (format code "Step 1 → Create server_socket on port~%")
      (format code "  Effect: Create socket server_socket on port~%")
      (format code "  Because: Listen for incoming HTTP connections~%")
      (format code "~%")
      
      (format code "Step 2 → Accept connection on server_socket~%")
      (format code "  Effect: Accept connection on server_socket~%")
      (format code "  Because: Receive client HTTP request~%")
      (format code "  Then: connection_count becomes connection_count + 1~%")
      (format code "~%")
      
      (format code "Step 3 → Read request from client~%")
      (format code "  Effect: Network read~%")
      (format code "  Because: Get HTTP request data~%")
      (format code "  Then: request becomes parse HTTP request from request_data~%")
      (format code "~%")
      
      (format code "Step 4 → Send HTTP response to client~%")
      (format code "  Effect: Send \"HTTP/1.1 200 OK\\r\\nContent-Type: text/html\\r\\n\\r\\n<h1>Hello!</h1>\" to client~%")
      (format code "  Because: Respond to client request~%")
      (format code "~%")
      
      (format code "Step 5 → If connection_count < 100, repeat from Step 2~%")
      (format code "  Because: Handle multiple client connections~%")
      (format code "  Otherwise: go to End~%")
      (format code "~%")
      
      (format code "End:~%")
      (format code "  Return \"Server stopped\"~%")
      (format code "  Because: Maximum connections reached~%"))))

(defun generate-webserver-examples (output-file)
  "Generate comprehensive webserver examples dataset."
  (let ((variants '()))
    
    ;; Basic webservers with different ports
    (loop for port in '(3000 8000 8080 9000) do
      (push (list :id (format nil "webserver-port-~A" port)
                  :prompt (format nil "Create a webserver on port ~A" port)
                  :modifications (list :port port :story (format nil "Run webserver on port ~A" port)))
            variants))
    
    ;; Different route configurations
    (push (list :id "webserver-multiple-routes"
                :prompt "Create a webserver with multiple routes"
                :modifications (list :story "Webserver with multiple route handling"
                                   :routes '(("GET" "/" "Home") ("GET" "/about" "About Page"))))
          variants)
    
    (push (list :id "webserver-rest-api"
                :prompt "Create a REST API webserver"
                :modifications (list :story "REST API server with CRUD endpoints"
                                   :routes '(("GET" "/api/users" "User List") 
                                           ("POST" "/api/users" "Create User"))))
          variants)
    
    ;; Feature variants
    (push (list :id "webserver-query-params"
                :prompt "Create a webserver that handles query parameters"
                :modifications (list :feature 'query-params
                                   :story "Webserver with query parameter parsing"))
          variants)
    
    (push (list :id "webserver-post-data"
                :prompt "Create a webserver that accepts POST data"
                :modifications (list :feature 'post-data
                                   :story "Webserver handling POST requests"))
          variants)
    
    ;; Generate CNS code for each variant
    (let ((entries '()))
      (dolist (variant variants)
        (let* ((id (getf variant :id))
               (prompt (getf variant :prompt))
               (mods (getf variant :modifications))
               (code (generate-webserver-variant nil mods))
               (entry (make-dataset-entry
                       :id id
                       :prompt prompt
                       :cns-code code
                       :category "webserver"
                       :tags '("networking" "http" "effects" "loops")
                       :difficulty "medium"
                       :expected "Server responds to HTTP requests on configured port"
                       :notes "Auto-generated variant")))
          (push entry entries)))
      
      ;; Write JSON
      (with-open-file (out output-file 
                           :direction :output 
                           :if-exists :supersede
                           :if-does-not-exist :create)
        (format out "{~%")
        (format out "  \"version\": \"1.0\",~%")
        (format out "  \"description\": \"CNS webserver examples for LLM training\",~%")
        (format out "  \"count\": ~A,~%" (length entries))
        (format out "  \"examples\": [~%")
        
        (loop for entry in (nreverse entries)
              for first = t then nil
              do (progn
                   (unless first (format out ",~%"))
                   (write-json-object (dataset-entry-to-plist entry) out :indent 4)))
        
        (format out "~%  ]~%")
        (format out "}~%"))
      
      (format t "~%✓ Generated ~A webserver variants -> ~A~%" 
              (length entries) output-file)
      entries)))

;;; Main entry point
(defun main ()
  "Generate all datasets."
  (format t "~%=== CNS Dataset Generator ===~%")
  
  ;; Generate from existing examples
  (generate-dataset-from-directory "examples/" "dataset/cns-examples.json")
  
  ;; Generate webserver variants
  (generate-webserver-examples "dataset/webserver-examples.json")
  
  (format t "~%✓ Dataset generation complete!~%"))

;; Run if executed directly (only if not loaded as library)
(when (not (member :generate-dataset-loaded *features*))
  (push :generate-dataset-loaded *features*)
  (main))
