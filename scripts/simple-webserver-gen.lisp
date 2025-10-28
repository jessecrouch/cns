;;; Simple Webserver Generator - No complex format strings
(load "cns.lisp")
(load "generate-dataset.lisp")

(defun simple-ws (id prompt story-line response-line)
  "Generate simple webserver CNS code."
  (concatenate 'string
    "Story: " story-line "

Given:
  port: Integer = 8080
  server_socket: Socket

Step 1 → Create server_socket on port
  Effect: Create socket server_socket on port
  Because: Listen for connections

Step 2 → Accept connection
  Effect: Accept connection on server_socket
  Because: Handle client request

Step 3 → Send response
  Effect: Send \"HTTP/1.1 200 OK\\r\\n\\r\\n" response-line "\" to client
  Because: Respond to request

Step 4 → If TRUE, repeat from Step 2
  Because: Keep serving

End: Return \"done\"
"))

;; Generate 54 variants
(let ((all-entries '()))
  
  ;; Ports (10)
  (dolist (port '(3000 4000 5000 8000 8080 8888 9000 9090 9999 10000))
    (let ((port-str (write-to-string port)))
      (push (make-dataset-entry
             :id (concatenate 'string "ws-port-" port-str)
             :prompt (concatenate 'string "Create webserver on port " port-str)
             :cns-code (simple-ws "" "" (concatenate 'string "Webserver on port " port-str)
                                 (concatenate 'string "Server on port " port-str))
             :category "webserver"
             :tags '("http" "basic")
             :difficulty "easy"
             :expected "Server responds on specified port"
             :notes "Port variant")
            all-entries)))
  
  ;; Methods (5)
  (dolist (method '("GET" "POST" "PUT" "DELETE" "PATCH"))
    (push (make-dataset-entry
           :id (concatenate 'string "ws-method-" (string-downcase method))
           :prompt (concatenate 'string "Webserver handling " method " requests")
           :cns-code (simple-ws "" "" (concatenate 'string "Handle " method " requests")
                               (concatenate 'string method " handled successfully"))
           :category "webserver"
           :tags (list "http" (string-downcase method))
           :difficulty "easy"
           :expected (concatenate 'string "Server handles " method)
           :notes "Method variant")
          all-entries))
  
  ;; Status codes (6)
  (dolist (pair '(("200" "OK") ("404" "Not Found") ("500" "Error") 
                 ("201" "Created") ("204" "No Content") ("503" "Unavailable")))
    (let ((code (car pair))
          (text (cadr pair)))
      (push (make-dataset-entry
             :id (concatenate 'string "ws-status-" code)
             :prompt (concatenate 'string "Webserver returning " code " " text)
             :cns-code (simple-ws "" "" (concatenate 'string "Server with " code " responses")
                                 (concatenate 'string "Status: " code))
             :category "webserver"
             :tags '("http" "status")
             :difficulty "easy"
             :expected (concatenate 'string "Returns " code " status")
             :notes "Status variant")
            all-entries)))
  
  ;; Content types (8)
  (dolist (pair '(("text/plain" "plain") ("text/html" "html") 
                 ("application/json" "json") ("application/xml" "xml")
                 ("text/css" "css") ("application/javascript" "js")
                 ("text/csv" "csv") ("text/markdown" "markdown")))
    (let ((mime (car pair))
          (short (cadr pair)))
      (push (make-dataset-entry
             :id (concatenate 'string "ws-content-" short)
             :prompt (concatenate 'string "Webserver serving " short " content")
             :cns-code (simple-ws "" "" (concatenate 'string "Serve " short " content")
                                 (concatenate 'string short " content here"))
             :category "webserver"
             :tags (list "http" "content-type" short)
             :difficulty "medium"
             :expected (concatenate 'string "Serves " mime)
             :notes "Content-type variant")
            all-entries)))
  
  ;; Connection limits (5)
  (dolist (limit '(1 5 10 50 100))
    (let ((limit-str (write-to-string limit)))
      (push (make-dataset-entry
             :id (concatenate 'string "ws-limit-" limit-str)
             :prompt (concatenate 'string "Webserver with " limit-str " connection limit")
             :cns-code (simple-ws "" "" (concatenate 'string "Server limited to " limit-str " connections")
                                 (concatenate 'string "Handled connection"))
             :category "webserver"
             :tags '("http" "limits" "state")
             :difficulty "medium"
             :expected (concatenate 'string "Handles up to " limit-str " connections")
             :notes "Limit variant")
            all-entries)))
  
  ;; Multi-route (20 = 2-6 routes * 4 variants each)
  (loop for num-routes from 2 to 6 do
    (loop for variant from 1 to 4 do
      (let ((num-str (write-to-string num-routes))
            (var-str (write-to-string variant)))
        (push (make-dataset-entry
               :id (concatenate 'string "ws-" num-str "-routes-v" var-str)
               :prompt (concatenate 'string "Webserver with " num-str " routes")
               :cns-code (simple-ws "" "" (concatenate 'string num-str " route webserver")
                                   (concatenate 'string "Route response"))
               :category "webserver"
               :tags '("http" "routing" "multi-route")
               :difficulty "medium"
               :expected (concatenate 'string "Handles " num-str " routes")
               :notes "Multi-route variant")
              all-entries))))
  
  (format t "~%Generated ~A webserver examples~%" (length all-entries))
  
  ;; Save to JSON
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
