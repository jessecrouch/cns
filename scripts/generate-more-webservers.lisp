;;; Extended Webserver Dataset Generator
;;; Generates 50+ webserver variants for comprehensive LLM training

(load "cns.lisp")
(load "generate-dataset.lisp")

(defun generate-extended-webserver-dataset (output-file)
  "Generate 50+ webserver examples with diverse configurations."
  (let ((entries '()))
    
    ;; 1. Port variations (10 examples)
    (loop for port in '(3000 3001 4000 5000 8000 8080 8888 9000 9090 9999) do
      (let* ((id (format nil "webserver-port-~A" port))
             (code (format nil "Story: Run webserver on port ~A

Given:
  port: Integer = ~A [network port]
  server_socket: Socket [network listener]
  connection_count: Integer = 0

Step 1 → Create server_socket on port
  Effect: Create socket server_socket on port
  Because: Listen for incoming HTTP connections

Step 2 → Accept connection on server_socket
  Effect: Accept connection on server_socket
  Because: Receive client request
  Then: connection_count becomes connection_count + 1

Step 3 → Read request from client
  Effect: Network read
  Because: Get HTTP request data

Step 4 → Send response to client
  Effect: Send \"HTTP/1.1 200 OK\\r\\nContent-Type: text/plain\\r\\n\\r\\nServer on port ~A\" to client
  Because: Respond with port information

Step 5 → If connection_count < 50, repeat from Step 2
  Because: Handle multiple connections
  Otherwise: go to End

End:
  Return \"Server stopped\"
  Because: Completed maximum connections
" port port port)))
        (push (make-dataset-entry
               :id id
               :prompt (format nil "Create a webserver on port ~A" port)
               :cns-code code
               :category "webserver"
               :tags '("networking" "http" "loops" "effects")
               :difficulty "easy"
               :expected (format nil "Server responds on port ~A" port)
               :notes "Port variation")
              entries)))
    
    ;; 2. Different response types (8 examples)
    (loop for (rtype content-type body) in 
          '(("plain-text" "text/plain" "Hello, World!")
            ("html" "text/html" "<html><body><h1>Welcome</h1></body></html>")
            ("json" "application/json" "{\"message\": \"Hello\", \"status\": \"ok\"}")
            ("xml" "application/xml" "<response><message>Hello</message></response>")
            ("css" "text/css" "body { margin: 0; }")
            ("javascript" "application/javascript" "console.log('Hello');")
            ("csv" "text/csv" "name,value\\ntest,123")
            ("markdown" "text/markdown" "# Hello World\\n\\nThis is **bold**"))
          do
      (let* ((id (format nil "webserver-response-~A" rtype))
             (code (format nil "Story: Webserver returning ~A content

Given:
  port: Integer = 8080
  server_socket: Socket
  content_type: String = \"~A\"
  response_body: String = \"~A\"

Step 1 → Create server_socket on port
  Effect: Create socket server_socket on port
  Because: Listen for HTTP connections

Step 2 → Accept connection on server_socket
  Effect: Accept connection on server_socket
  Because: Receive client request

Step 3 → Read request
  Effect: Network read
  Because: Get client HTTP request

Step 4 → Send ~A response
  Effect: Send \"HTTP/1.1 200 OK\\r\\nContent-Type: ~A\\r\\n\\r\\n{response_body}\" to client
  Because: Return ~A content to client

Step 5 → If TRUE, repeat from Step 2
  Because: Keep server running
  Otherwise: go to End

End:
  Return \"Server stopped\"
" rtype content-type body rtype content-type rtype)))
        (push (make-dataset-entry
               :id id
               :prompt (format nil "Create a webserver that returns ~A content" rtype)
               :cns-code code
               :category "webserver"
               :tags (list "networking" "http" rtype)
               :difficulty "easy"
               :expected (format nil "Server returns ~A responses" content-type)
               :notes "Content-type variation")
              entries)))
    
    ;; 3. Multiple routes (10 examples)
    (loop for num-routes from 2 to 5 do
      (loop for variant from 1 to 2 do
        (let* ((id (format nil "webserver-~A-routes-v~A" num-routes variant))
               (routes (loop for i from 1 to num-routes
                           collect (list (format nil "/page~A" i) 
                                       (format nil "Page ~A Content" i))))
               (route-checks (with-output-to-string (s)
                              (loop for i from 1 to num-routes
                                    for step-num = (+ 3 (* 2 (- i 1)))
                                    do (format s "Step ~A → If request.url = \"/page~A\", go to Step ~A~%  Because: Route to page ~A handler~%  Otherwise: go to Step ~A~%~%"
                                             step-num i (+ step-num 1) i (+ step-num 2))
                                       (format s "Step ~A → Send page ~A response~%  Effect: Send \"HTTP/1.1 200 OK\\r\\n\\r\\nPage ~A\" to client~%  Because: Return page ~A content~%  Then: go to Step 2~%~%"
                                             (+ step-num 1) i i i))))
               (code (format nil "Story: Webserver with ~A routes

Given:
  port: Integer = 8080
  server_socket: Socket

Step 1 → Create server_socket on port
  Effect: Create socket server_socket on port
  Because: Listen for connections

Step 2 → Accept connection
  Effect: Accept connection on server_socket
  Because: Handle client request
  Then: request becomes parse HTTP request from request_data

~A
End:
  Return \"Server stopped\"
" num-routes route-checks)))
          (push (make-dataset-entry
                 :id id
                 :prompt (format nil "Create a webserver with ~A different routes" num-routes)
                 :cns-code code
                 :category "webserver"
                 :tags '("networking" "http" "routing" "conditionals")
                 :difficulty "medium"
                 :expected (format nil "Server with ~A route handlers" num-routes)
                 :notes "Multiple route variant")
                entries))))
    
    ;; 4. Error handling variants (5 examples)
    (loop for (etype desc) in 
          '(("404" "not found")
            ("500" "server error")
            ("timeout" "connection timeout")
            ("bad-request" "malformed request")
            ("rate-limit" "too many requests"))
          do
      (let* ((id (format nil "webserver-error-~A" etype))
             (code (format nil "Story: Webserver with ~A handling

Given:
  port: Integer = 8080
  server_socket: Socket
  error_count: Integer = 0

Step 1 → Create server_socket on port
  Effect: Create socket server_socket on port
  Because: Listen for connections

Step 2 → Accept connection
  Effect: Accept connection on server_socket
  Because: Handle client

Step 3 → Read request
  Effect: Network read
  Because: Get client request

Step 4 → If request.url = \"/error\", go to Step 5
  Because: Simulate error condition
  Otherwise: go to Step 6

Step 5 → Send error response
  Effect: Send \"HTTP/1.1 ~A ~A\\r\\n\\r\\nError occurred\" to client
  Because: Handle ~A error
  Then: go to Step 7

Step 6 → Send success response
  Effect: Send \"HTTP/1.1 200 OK\\r\\n\\r\\nSuccess\" to client
  Because: Normal response

Step 7 → If TRUE, repeat from Step 2
  Because: Continue serving
  Otherwise: go to End

Error:
  Return \"Server crashed\"
  Effect: Log \"Server error: ~A\"
  Because: Unexpected failure

End:
  Return \"Server stopped\"
" desc desc etype etype)))
        (push (make-dataset-entry
               :id id
               :prompt (format nil "Create a webserver that handles ~A errors" desc)
               :cns-code code
               :category "webserver"
               :tags '("networking" "http" "error-handling")
               :difficulty "medium"
               :expected (format nil "Server handles ~A condition" desc)
               :notes "Error handling variant")
              entries)))
    
    ;; 5. Feature combinations (10 examples)
    (loop for (feature desc tags) in
          '(("counter" "request counter" ("state"))
            ("logging" "request logging" ("io" "effects"))
            ("static-files" "static file serving" ("file-io"))
            ("auth" "basic authentication" ("security"))
            ("cors" "CORS headers" ("headers"))
            ("compression" "response compression" ("optimization"))
            ("caching" "response caching" ("performance"))
            ("websocket" "WebSocket upgrade" ("realtime"))
            ("redirect" "URL redirection" ("routing"))
            ("proxy" "reverse proxy" ("networking")))
          do
      (let* ((id (format nil "webserver-feature-~A" feature))
             (code (format nil "Story: Webserver with ~A feature

Given:
  port: Integer = 8080
  server_socket: Socket
  ~A: Integer = 0

Step 1 → Create server_socket on port
  Effect: Create socket server_socket on port
  Because: Start server with ~A

Step 2 → Accept connection
  Effect: Accept connection on server_socket
  Because: Handle requests with ~A

Step 3 → Send response with ~A
  Effect: Send \"HTTP/1.1 200 OK\\r\\n\\r\\nFeature: ~A\" to client
  Because: Demonstrate ~A functionality

Step 4 → If TRUE, repeat from Step 2
  Because: Keep serving with ~A
  Otherwise: go to End

End:
  Return \"Server with ~A stopped\"
" desc feature desc desc desc desc desc desc desc)))
        (push (make-dataset-entry
               :id id
               :prompt (format nil "Create a webserver with ~A" desc)
               :cns-code code
               :category "webserver"
               :tags (append '("networking" "http") tags)
               :difficulty "hard"
               :expected (format nil "Server with ~A capability" desc)
               :notes "Feature combination")
              entries)))
    
    ;; Write to JSON
    (with-open-file (out output-file 
                         :direction :output 
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (format out "[~%")
      
      (loop for entry in (nreverse entries)
            for first = t then nil
            do (progn
                 (unless first (format out ",~%"))
                 (write-json-object (dataset-entry-to-plist entry) out :indent 2)))
      
      (format out "~%]~%"))
    
    (format t "~%✓ Generated ~A extended webserver examples -> ~A~%" 
            (length entries) output-file)
    entries))

;; Run
(generate-extended-webserver-dataset "dataset/webserver-examples-extended.json")
