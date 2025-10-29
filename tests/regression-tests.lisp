;;; CNS Regression Tests
;;; Tests core interpreter functionality to prevent regressions

(load "../src/cns.lisp")

;;; Test framework
(defvar *tests-passed* 0)
(defvar *tests-failed* 0)
(defvar *tests-total* 0)

(defun test-assert (name condition &optional message)
  "Assert a condition is true."
  (incf *tests-total*)
  (if condition
      (progn
        (format t "  [~D] PASS: ~A~%" *tests-total* name)
        (incf *tests-passed*))
      (progn
        (format t "  [~D] FAIL: ~A~%" *tests-total* name)
        (when message
          (format t "       ~A~%" message))
        (incf *tests-failed*))))

(defun test-equal (name actual expected)
  "Assert actual equals expected."
  (test-assert name 
               (equal actual expected)
               (format nil "Expected ~A, got ~A" expected actual)))

(defun run-test-suite ()
  "Run all regression tests."
  (setf *tests-passed* 0)
  (setf *tests-failed* 0)
  (setf *tests-total* 0)
  
  (format t "~%===========================================~%")
  (format t " CNS Interpreter Regression Tests~%")
  (format t "===========================================~%~%")
  
  ;; Helper function tests
  (format t "Testing helper functions...~%")
  (test-equal "split-string basic"
              (split-string "a,b,c" #\,)
              '("a" "b" "c"))
  
  (test-equal "trim whitespace"
              (trim "  hello  ")
              "hello")
  
  (test-equal "starts-with true"
              (starts-with "Story: test" "Story:")
              t)
  
  (test-equal "starts-with false"
              (starts-with "Given:" "Story:")
              nil)
  
  (test-equal "emptyp empty string"
              (emptyp "   ")
              t)
  
  (test-equal "emptyp non-empty"
              (emptyp "  a  ")
              nil)
  
  ;; Parser tests
  (format t "~%Testing parser...~%")
  
  ;; Test with a real working example
  (handler-case
      (let ((ast (with-open-file (s "../examples/factorial.cns")
                   (let ((code (make-string (file-length s))))
                     (read-sequence code s)
                     (parse-cns code)))))
        
        (test-assert "parse-cns returns list"
                     (listp ast))
        
        (test-assert "parse-cns returns non-empty"
                     (> (length ast) 0))
        
        (test-assert "parsed AST has story"
                     (some (lambda (x) (and (listp x) (eql (car x) 'story))) ast))
        
        (test-assert "parsed AST has given"
                     (some (lambda (x) (and (listp x) (eql (car x) 'given))) ast)))
    (error (e)
      (format t "  [ERROR] Parser test failed: ~A~%" e)
      (incf *tests-total* 4)
      (incf *tests-failed* 4)))
  
  ;; HTTP parsing tests
  (format t "~%Testing HTTP parsing...~%")
  
  (let ((request "GET /test HTTP/1.1"))
    (let ((parsed (parse-http-request request)))
      (test-equal "parse GET method"
                  (getf (cdr parsed) :method)  ; Skip :request keyword
                  "GET")
      
      (test-equal "parse URL"
                  (getf (cdr parsed) :url)
                  "/test")
      
      (test-equal "parse HTTP version"
                  (getf (cdr parsed) :version)
                  "HTTP/1.1")))
  
  ;; Route matching tests
  (format t "~%Testing route matching...~%")
  
  (let ((routes '(("GET" "/" "Home")
                  ("GET" "/about" "About")
                  ("POST" "/submit" "Submit"))))
    
    (test-equal "match-route GET /"
                (match-route routes "GET" "/")
                "Home")
    
    (test-equal "match-route GET /about"
                (match-route routes "GET" "/about")
                "About")
    
    (test-equal "match-route not found"
                (match-route routes "GET" "/404")
                nil)
    
    (test-equal "match-route wrong method"
                (match-route routes "POST" "/")
                nil))
  
  ;; Expression evaluation tests
  (format t "~%Testing expression evaluation...~%")
  
  (let ((env (make-hash-table :test 'equal)))
    (setf (gethash "x" env) 5)
    (setf (gethash "y" env) 10)
    
    (test-equal "eval-expr variable"
                (eval-expr "x" env)
                5)
    
    (test-equal "eval-expr addition"
                (eval-expr "x + y" env)
                15)
    
    (test-equal "eval-expr multiplication"
                (eval-expr "x * y" env)
                50)
    
    (test-equal "eval-expr comparison >"
                (eval-expr "y > x" env)
                t)
    
    (test-equal "eval-expr comparison <"
                (eval-expr "x < 3" env)
                nil))
  
  ;; String extraction tests
  (format t "~%Testing string extraction...~%")
  
  (test-equal "extract-quoted-string basic"
              (extract-quoted-string "\"hello\" rest")
              "hello")
  
  (test-equal "extract-quoted-string with escape"
              (extract-quoted-string "\"hello\\nworld\" rest")
              (format nil "hello~Aworld" #\Newline))
  
  ;; Summary
  (format t "~%===========================================~%")
  (format t " Test Summary~%")
  (format t "===========================================~%")
  (format t "Total:  ~D~%" *tests-total*)
  (format t "Passed: ~D~%" *tests-passed*)
  (format t "Failed: ~D~%" *tests-failed*)
  (format t "~%")
  
  (if (zerop *tests-failed*)
      (progn
        (format t "All tests passed! ✓~%~%")
        (sb-ext:exit :code 0))
      (progn
        (format t "~D test(s) failed~%~%" *tests-failed*)
        (sb-ext:exit :code 1))))

;; Run tests
(run-test-suite)
