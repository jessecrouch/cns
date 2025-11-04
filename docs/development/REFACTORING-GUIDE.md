# CONCRETE REFACTORING EXAMPLE

## Current Code vs. Refactored Code

Let's refactor JUST the arithmetic operators to show the improvement:

---

## CURRENT CODE (Lines 1940-2000 in cns.lisp)

```lisp
;; Arithmetic: multiplication (n * 3 or 3 * n both work now!)
((and (search "*" trimmed)
      ;; Make sure it's not a quoted string
      (not (and (> (length trimmed) 1)
               (char= (char trimmed 0) #\")
               (char= (char trimmed (1- (length trimmed))) #\"))))
 (let ((parts (split-string trimmed #\*)))
   (* (eval-expr (trim (car parts)) env)
      (eval-expr (trim (cadr parts)) env))))

;; Arithmetic: division (n / 2)
((and (search "/" trimmed)
      (not (and (> (length trimmed) 1)
               (char= (char trimmed 0) #\")
               (char= (char trimmed (1- (length trimmed))) #\"))))
 (let ((parts (split-string trimmed #\/)))
   (/ (eval-expr (trim (car parts)) env)
      (eval-expr (trim (cadr parts)) env))))

;; Arithmetic: subtraction (n - 1 or 10 - n both work!)
;; BUT: Don't match negative numbers like "-42"
;; AND: Don't match inside quoted strings like "YYYY-MM-DD"
;; AND: Don't match in complex expressions like FORMAT TIME
((and (search "-" trimmed)
      ;; Guard 1: Not a quoted string
      (not (and (> (length trimmed) 1)
               (char= (char trimmed 0) #\")
               (char= (char trimmed (1- (length trimmed))) #\")))
      ;; Guard 2: Not a filepath
      (not (and (> (length trimmed) 0)
               (char= (char trimmed 0) #\/)))
      ;; Guard 3-7: Not in special contexts
      (not (starts-with (string-upcase trimmed) "FORMAT TIME "))
      (not (starts-with (string-upcase trimmed) "ADD DAYS "))
      (not (starts-with (string-upcase trimmed) "ADD HOURS "))
      (not (starts-with (string-upcase trimmed) "ADD MINUTES "))
      (not (starts-with (string-upcase trimmed) "REPLACE "))
      ;; Guard 8: Has left operand
      (let ((minus-pos (position #\- trimmed)))
        (and minus-pos
             (> minus-pos 0)
             (> (length (trim (subseq trimmed 0 minus-pos))) 0))))
 (let ((parts (split-string trimmed #\-)))
   (- (eval-expr (trim (car parts)) env)
      (eval-expr (trim (cadr parts)) env))))

;; Addition/Concatenation (similar mess...)
```

**Problems:**
- 60+ lines for 4 operators
- Duplicated guard logic (quoted string check appears 4 times)
- Guards needed for each operator
- Hard to see precedence
- Hard to add new operators

---

## REFACTORED CODE (Option 1: Helper Functions)

### Step 1: Extract common predicates
```lisp
(defun quoted-string-p (str)
  "Check if string is a quoted literal"
  (and (> (length str) 1)
       (char= (char str 0) #\")
       (char= (char str (1- (length str))) #\")))

(defun filepath-p (str)
  "Check if string looks like an absolute path"
  (and (> (length str) 0)
       (char= (char str 0) #\/)))

(defun datetime-expr-p (str)
  "Check if string is a datetime expression"
  (let ((upper (string-upcase str)))
    (or (starts-with upper "FORMAT TIME ")
        (starts-with upper "ADD DAYS ")
        (starts-with upper "ADD HOURS ")
        (starts-with upper "ADD MINUTES "))))

(defun should-skip-operator-p (str)
  "Check if we should skip operator parsing for this string"
  (or (quoted-string-p str)
      (filepath-p str)
      (datetime-expr-p str)))
```

### Step 2: Extract operator handler
```lisp
(defun try-binary-operator (trimmed operator-char operator-fn env)
  "Try to parse a binary operator expression"
  (when (and (search (string operator-char) trimmed)
             (not (should-skip-operator-p trimmed)))
    (let ((parts (split-string trimmed operator-char)))
      (when (= (length parts) 2)
        (funcall operator-fn
                (eval-expr (trim (car parts)) env)
                (eval-expr (trim (cadr parts)) env))))))
```

### Step 3: Simplified operator matching
```lisp
;; In eval-expr:
(or
  ;; Try multiplication
  (try-binary-operator trimmed #\* #'* env)
  
  ;; Try division
  (try-binary-operator trimmed #\/ #'/ env)
  
  ;; Try subtraction (with special check for negative numbers)
  (when (and (search "-" trimmed)
             (not (should-skip-operator-p trimmed))
             (let ((pos (position #\- trimmed)))
               (and pos (> pos 0))))  ; Has left operand
    (let ((parts (split-string trimmed #\-)))
      (- (eval-expr (trim (car parts)) env)
         (eval-expr (trim (cadr parts)) env))))
  
  ;; Try addition
  (try-binary-operator trimmed #\+ #'+ env))
```

**Improvement:**
- 60 lines → ~35 lines
- Guards defined once, reused
- Easier to add operators
- Still order-dependent but cleaner

---

## REFACTORED CODE (Option 2: Precedence Table - Best)

### Step 1: Define operator metadata
```lisp
(defvar *binary-operators*
  '((#\* . (:precedence 40 :fn * :name "multiplication"))
    (#\/ . (:precedence 40 :fn / :name "division"))
    (#\+ . (:precedence 30 :fn + :name "addition"))
    (#\- . (:precedence 30 :fn - :name "subtraction" :requires-left t))
    (#\= . (:precedence 10 :fn equal :name "equality"))
    (#\< . (:precedence 20 :fn < :name "less-than"))
    (#\> . (:precedence 20 :fn > :name "greater-than"))))

(defun get-operator-info (op-char)
  (cdr (assoc op-char *binary-operators*)))

(defun operator-precedence (op-char)
  (getf (get-operator-info op-char) :precedence))

(defun operator-function (op-char)
  (getf (get-operator-info op-char) :fn))
```

### Step 2: Precedence-based parsing
```lisp
(defun find-operator-split-point (trimmed min-precedence)
  "Find the operator with lowest precedence to split on"
  (let ((best-pos nil)
        (best-op nil)
        (best-prec 999))
    (loop for i from 0 below (length trimmed) do
      (let* ((ch (char trimmed i))
             (op-info (get-operator-info ch)))
        (when (and op-info
                   (not (in-quoted-region-p trimmed i))
                   (<= (getf op-info :precedence) best-prec)
                   (>= (getf op-info :precedence) min-precedence))
          (setf best-pos i
                best-op ch
                best-prec (getf op-info :precedence)))))
    (values best-pos best-op)))

(defun eval-expr-with-precedence (trimmed env)
  "Parse expression respecting operator precedence"
  (multiple-value-bind (split-pos op-char) 
      (find-operator-split-point trimmed 0)
    (if split-pos
        ;; Found operator - split and recurse
        (let* ((left (trim (subseq trimmed 0 split-pos)))
               (right (trim (subseq trimmed (1+ split-pos))))
               (op-fn (operator-function op-char)))
          (funcall op-fn
                   (eval-expr left env)
                   (eval-expr right env)))
        ;; No operator - it's a literal or variable
        (eval-atomic trimmed env))))
```

**Benefits:**
- Precedence is EXPLICIT in table
- Adding operator = 1 line in table
- No more "MUST come before" comments
- Can easily change precedence
- Much easier to test

---

## SIDE-BY-SIDE COMPARISON

### Adding exponentiation operator `**`

**Current approach:**
```lisp
;; In eval-expr, somewhere around line 1950, need to figure out order:

;; Exponentiation: n ** 2 (MUST come AFTER quoted strings, 
;; BEFORE multiplication, etc.)
((and (search "**" trimmed)
      ;; Guard 1: Not quoted string
      (not (and (> (length trimmed) 1)
               (char= (char trimmed 0) #\")
               (char= (char trimmed (1- (length trimmed))) #\")))
      ;; Guard 2: Not filepath  
      (not (and (> (length trimmed) 0)
               (char= (char trimmed 0) #\/)))
      ;; ... more guards ...
      )
 (let ((parts (split-string trimmed "**")))  ; Oops, need multi-char split!
   (expt (eval-expr (trim (car parts)) env)
         (eval-expr (trim (cadr parts)) env))))
```

**Refactored approach (Option 2):**
```lisp
;; In *binary-operators* table:
(defvar *binary-operators*
  '((#\* . (:precedence 40 :fn * :name "multiplication"))
    (**  . (:precedence 50 :fn expt :name "exponentiation"))  ; ← ADD THIS
    (#\/ . (:precedence 40 :fn / :name "division"))
    ...))

;; Done. That's it. 1 line.
```

---

## TESTING COMPARISON

### Current approach - must test everything together:
```lisp
(assert (= (eval-expr "3 * 4 + 5" env) 17))  ; Pass or fail, hard to debug
```

### Refactored - test each layer:
```lisp
;; Test tokenizer
(assert (equal (tokenize "3 * 4 + 5")
               '((:NUM 3) (:OP *) (:NUM 4) (:OP +) (:NUM 5))))

;; Test parser
(assert (equal (parse '((:NUM 3) (:OP *) (:NUM 4) (:OP +) (:NUM 5)))
               '(:BINOP + (:BINOP * (:LIT 3) (:LIT 4)) (:LIT 5))))

;; Test evaluator
(assert (= (eval-ast '(:BINOP + (:LIT 12) (:LIT 5)) env) 17))
```

**Benefit:** When test fails, you know WHICH phase broke.

---

## SUMMARY

| Metric               | Current | Option 1 | Option 2 |
|---------------------|---------|----------|----------|
| Lines of code       | 60      | 35       | 45       |
| Duplicated guards   | 4x      | 1x       | 0x       |
| Add operator cost   | ~30 min | ~10 min  | ~2 min   |
| Precedence clarity  | Hidden  | Hidden   | Explicit |
| Testability         | Hard    | Medium   | Easy     |
| Debuggability       | Hard    | Medium   | Easy     |
| Maintenance         | Hard    | Medium   | Easy     |

**Recommendation:** Start with Option 1 (quick win), move to Option 2 when ready.

