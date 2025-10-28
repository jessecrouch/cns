;;; Simpler Extended Webserver Generator
;;; Creates 50+ webserver variants for LLM training

(load "cns.lisp")
(load "generate-dataset.lisp")

(defun make-webserver-entry (id prompt code category tags difficulty)
  "Helper to create a dataset entry for webserver."
  (make-dataset-entry
   :id id
   :prompt prompt
   :cns-code code
   :category category
   :tags tags
   :difficulty difficulty
   :expected "Server handles HTTP requests correctly"
   :notes "Generated variant"))

(defun generate-50-plus-webservers ()
  "Generate 50+ webserver examples."
  (let ((entries '()))
    
    ;; 1. Basic port variations (10 examples)
    (dolist (port '(3000 4000 5000 8000 8080 8888 9000 9090 9999 10000))
      (push (make-webserver-entry
             (format nil "ws-port-~A" port)
             (format nil "Create webserver on port ~A" port)
             (format nil "Story: Simple webserver on port ~A

Given:
  port: Integer = ~A
  server_socket: Socket

Step 1 → Create server_socket on port
  Effect: Create socket server_socket on port
  Because: Listen for connections

Step 2 → Accept connection
  Effect: Accept connection on server_socket
  Because: Handle client

Step 3 → Send response
  Effect: Send \"HTTP/1.1 200 OK\\r\\n\\r\\nHello on port ~A\" to client
  Because: Respond to request

Step 4 → If TRUE, repeat from Step 2
  Because: Keep serving

End: Return \"done\"
" port port port)
             "webserver" '("http" "basic") "easy")
            entries))
    
    ;; 2. Different HTTP methods (5 examples)
    (dolist (method '("GET" "POST" "PUT" "DELETE" "PATCH"))
      (push (make-webserver-entry
             (format nil "ws-method-~A" (string-downcase method))
             (format nil "Webserver handling ~A requests" method)
             (format nil "Story: Webserver for ~A requests

Given:
  port: Integer = 8080
  server_socket: Socket
  method: String = \"~A\"

Step 1 → Create server_socket on port
  Effect: Create socket server_socket on port
  Because: Start server

Step 2 → Accept connection
  Effect: Accept connection on server_socket
  Because: Handle ~A request

Step 3 → Send ~A response
  Effect: Send \"HTTP/1.1 200 OK\\r\\n\\r\\n~A handled\" to client
  Because: Respond to ~A

Step 4 → If TRUE, repeat from Step 2

End: Return \"done\"
" method method method method method method)
             "webserver" (list "http" (string-downcase method)) "easy")
            entries))
    
    ;; 3. Status code variations (6 examples)
    (dolist (status '(("200" "OK") ("201" "Created") ("204" "No Content") 
                     ("404" "Not Found") ("500" "Server Error") ("503" "Unavailable")))
      (let ((code-num (car status))
            (code-text (cadr status)))
        (push (make-webserver-entry
               (format nil "ws-status-~A" code-num)
               (format nil "Webserver returning ~A ~A" code-num code-text)
               (format nil "Story: Server with ~A status

Given:
  port: Integer = 8080
  server_socket: Socket

Step 1 → Create server_socket on port
  Effect: Create socket server_socket on port
  Because: Start server

Step 2 → Accept connection
  Effect: Accept connection on server_socket
  Because: Handle request

Step 3 → Send ~A response
  Effect: Send \"HTTP/1.1 ~A ~A\\r\\n\\r\\n\" to client
  Because: Return ~A ~A status

Step 4 → If TRUE, repeat from Step 2

End: Return \"done\"
" code-text code-num code-text code-num code-text)
               "webserver" '("http" "status-codes") "easy")
              entries)))
    
    ;; 4. Multi-route servers (10 examples - 2 to 6 routes)
    (loop for num-routes from 2 to 6 do
      (loop for variant from 1 to 2 do
        (let ((routes (loop for i from 1 to num-routes
                          collect (format nil "/route~A" i))))
          (push (make-webserver-entry
                 (format nil "ws-~A-routes-v~A" num-routes variant)
                 (format nil "Webserver with ~A routes" num-routes)
                 (with-output-to-string (s)
                   (format s "Story: Webserver with ~A routes~%~%" num-routes)
                   (format s "Given:~%  port: Integer = 8080~%  server_socket: Socket~%~%")
                   (format s "Step 1 → Create server_socket on port~%")
                   (format s "  Effect: Create socket server_socket on port~%")
                   (format s "  Because: Start server~%~%")
                   (format s "Step 2 → Accept connection~%")
                   (format s "  Effect: Accept connection on server_socket~%")
                   (format s "  Because: Handle request~%~%")
                   (loop for i from 1 to num-routes
                         for route in routes
                         do (format s "Step ~A → Send ~A response~%" (+ 2 i) route)
                            (format s "  Effect: Send \"HTTP/1.1 200 OK\\r\\n\\r\\nRoute ~A\" to client~%" i)
                            (format s "  Because: Handle ~A~%" route)
                            (if (< i num-routes)
                                (format s "  Then: go to Step 2~%~%")
                                (format s "~%")))
                   (format s "Step ~A → If TRUE, repeat from Step 2~%~%" (+ num-routes 3))
                   (format s "End: Return \"done\"~%"))
                 "webserver" '("http" "routing") "medium")
                entries))))
    
    ;; 5. Content-Type variations (8 examples)
    (dolist (ctype '(("text/plain" "plain") ("text/html" "html") 
                    ("application/json" "json") ("application/xml" "xml")
                    ("text/css" "css") ("application/javascript" "js")
                    ("text/csv" "csv") ("image/png" "image")))
      (let ((mime (car ctype))
            (short (cadr ctype)))
        (push (make-webserver-entry
               (format nil "ws-content-~A" short)
               (format nil "Webserver serving ~A content" short)
               (format nil "Story: Serve ~A content

Given:
  port: Integer = 8080
  server_socket: Socket
  content_type: String = \"~A\"

Step 1 → Create server_socket on port
  Effect: Create socket server_socket on port
  Because: Start server

Step 2 → Accept connection
  Effect: Accept connection on server_socket
  Because: Handle request

Step 3 → Send ~A content
  Effect: Send \"HTTP/1.1 200 OK\\r\\nContent-Type: ~A\\r\\n\\r\\nSample ~A\" to client
  Because: Return ~A data

Step 4 → If TRUE, repeat from Step 2

End: Return \"done\"
" short mime short mime short short)
               "webserver" (list "http" "content-type" short) "medium")
              entries)))
    
    ;; 6. Connection limit variations (5 examples)
    (dolist (limit '(1 5 10 50 100))
      (push (make-webserver-entry
             (format nil "ws-limit-~A" limit)
             (format nil "Webserver with ~A connection limit" limit)
             (format nil "Story: Server with ~A connection limit

Given:
  port: Integer = 8080
  server_socket: Socket
  count: Integer = 0
  max_connections: Integer = ~A

Step 1 → Create server_socket on port
  Effect: Create socket server_socket on port
  Because: Start server

Step 2 → Accept connection
  Effect: Accept connection on server_socket
  Because: Handle request
  Then: count becomes count + 1

Step 3 → Send response
  Effect: Send \"HTTP/1.1 200 OK\\r\\n\\r\\nConnection {count}\" to client
  Because: Respond with count

Step 4 → If count < max_connections, repeat from Step 2
  Because: Continue until limit
  Otherwise: go to End

End: Return \"done\"
" limit limit)
             "webserver" '("http" "state" "conditionals") "medium")
            entries))
    
    entries))

;; Generate and save
(let ((all-entries (generate-50-plus-webservers)))
  (format t "~%Generated ~A webserver examples~%" (length all-entries))
  
  ;; Write to JSON array format
  (with-open-file (out "dataset/webserver-examples-extended.json"
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (format out "[~%")
    (loop for entry in (reverse all-entries)
          for first = t then nil
          do (progn
               (unless first (format out ",~%"))
               (write-json-object (dataset-entry-to-plist entry) out :indent 2)))
    (format out "~%]~%"))
  
  (format t "✓ Saved to dataset/webserver-examples-extended.json~%"))
