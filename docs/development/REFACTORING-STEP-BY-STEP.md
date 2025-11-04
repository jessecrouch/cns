# CNS Architecture Refactoring: Step-by-Step Guide

**Start Date:** November 3, 2025  
**Status:** 📋 Planning  
**Approach:** Incremental, surgical, test-driven

---

## ⚠️ CRITICAL RULES - READ FIRST

### The Golden Rules

1. **NEVER skip testing** - Run `./test-all-examples.sh` after EVERY change
2. **ONE change at a time** - Extract one helper, test it, commit it, then move on
3. **If tests fail, REVERT** - Don't try to fix forward, go back to working state
4. **Commit frequently** - Every working change gets committed
5. **Document as you go** - Update comments to explain new helpers

### Current Baseline

**Before starting, verify tests pass:**
```bash
./test-all-examples.sh
```

**Expected results:** 36 PASS, 0 FAIL, 1 TIMEOUT (todo-api.cns)

If you don't have this baseline, **STOP** and fix existing issues first.

---

## Week 1: Extract Helper Predicates (4-6 hours)

### Goal
Eliminate duplicated guard code by extracting common predicate functions.

### Step 1.1: Extract `quoted-string-p` (30 minutes)

**Current code (appears ~20 times in eval-expr):**
```lisp
(not (and (> (length trimmed) 1)
         (char= (char trimmed 0) #\")
         (char= (char trimmed (1- (length trimmed))) #\")))
```

**Action:**
1. Add to helper functions section (around line 75):
```lisp
(defun quoted-string-p (str)
  "Check if string is a quoted literal like \"hello\""
  (and (> (length str) 1)
       (char= (char str 0) #\")
       (char= (char str (1- (length str))) #\")))
```

2. Find FIRST occurrence in eval-expr (around line 1961 in `*` operator)
3. Replace guard with `(not (quoted-string-p trimmed))`
4. Run tests: `./test-all-examples.sh`
5. If tests pass: commit with message "refactor: extract quoted-string-p helper (1/20)"
6. If tests fail: revert and check if you copied the logic correctly
7. Repeat for next occurrence
8. After all replacements done, verify tests still pass

**Success criteria:**
- All 36 tests pass
- Code is more readable
- Guard appears once instead of 20 times

### Step 1.2: Extract `filepath-p` (20 minutes)

**Current code (appears in 2 places):**
```lisp
(not (and (> (length trimmed) 0)
         (char= (char trimmed 0) #\/)))
```

**Action:**
1. Add helper:
```lisp
(defun filepath-p (str)
  "Check if string looks like an absolute filepath starting with /"
  (and (> (length str) 0)
       (char= (char str 0) #\/)))
```

2. Replace first occurrence (in `-` operator around line 1963)
3. Test: `./test-all-examples.sh`
4. Commit: "refactor: extract filepath-p helper"
5. Replace second occurrence
6. Test and commit again

### Step 1.3: Extract `datetime-expr-p` (30 minutes)

**Current code (appears in 4 operator guards):**
```lisp
(not (starts-with (string-upcase trimmed) "FORMAT TIME "))
(not (starts-with (string-upcase trimmed) "ADD DAYS "))
(not (starts-with (string-upcase trimmed) "ADD HOURS "))
(not (starts-with (string-upcase trimmed) "ADD MINUTES "))
```

**Action:**
1. Add helper:
```lisp
(defun datetime-expr-p (str)
  "Check if string is a datetime expression that shouldn't be operator-split"
  (let ((upper (string-upcase str)))
    (or (starts-with upper "FORMAT TIME ")
        (starts-with upper "ADD DAYS ")
        (starts-with upper "ADD HOURS ")
        (starts-with upper "ADD MINUTES "))))
```

2. Replace in `-` operator (around line 1966-1969)
3. Test: `./test-all-examples.sh`
4. Commit: "refactor: extract datetime-expr-p helper"
5. Replace in other operators if they exist
6. Test after each replacement

### Step 1.4: Extract `should-skip-operator-p` (45 minutes)

**Combine all guards into one master function:**

```lisp
(defun should-skip-operator-p (str)
  "Check if we should skip operator parsing for this string.
   Returns true for quoted strings, filepaths, datetime expressions, etc."
  (or (quoted-string-p str)
      (filepath-p str)
      (datetime-expr-p str)
      ;; Add other special cases here as needed
      ))
```

**Action:**
1. Add helper function
2. Update ONE operator (e.g., `*` multiplication) to use it:
```lisp
;; Before:
((and (search "*" trimmed)
      (not (quoted-string-p trimmed)))
 ...)

;; After:
((and (search "*" trimmed)
      (not (should-skip-operator-p trimmed)))
 ...)
```
3. Test thoroughly
4. Commit
5. Repeat for other operators one at a time

### Week 1 Completion Checklist

- [ ] `quoted-string-p` extracted and used everywhere
- [ ] `filepath-p` extracted and used everywhere
- [ ] `datetime-expr-p` extracted and used everywhere
- [ ] `should-skip-operator-p` created and tested
- [ ] All 36 tests still pass
- [ ] All changes committed with clear messages
- [ ] Code is more readable than before

**Time investment:** 4-6 hours  
**Benefit:** Reduced code duplication by ~100 lines, easier to add guards

---

## Week 2-3: Extract Operator Handlers (8-16 hours)

### Goal
Move operator logic into dedicated handler functions instead of inline `cond` branches.

### Step 2.1: Extract `try-binary-operator` helper (2 hours)

**Create a generic binary operator handler:**

```lisp
(defun try-binary-operator (trimmed operator-char operator-fn env)
  "Try to parse and evaluate a binary operator expression.
   Returns NIL if operator not found or should be skipped.
   Returns result if successfully evaluated."
  (when (and (search (string operator-char) trimmed)
             (not (should-skip-operator-p trimmed)))
    (let ((parts (split-string trimmed operator-char)))
      (when (= (length parts) 2)
        (funcall operator-fn
                (eval-expr (trim (car parts)) env)
                (eval-expr (trim (cadr parts)) env))))))
```

**Action:**
1. Add helper function after other helpers
2. Test it doesn't break compilation
3. Choose ONE simple operator to migrate (e.g., `/` division):

```lisp
;; Before:
((and (search "/" trimmed)
      (not (should-skip-operator-p trimmed)))
 (let ((parts (split-string trimmed #\/)))
   (/ (eval-expr (trim (car parts)) env)
      (eval-expr (trim (cadr parts)) env))))

;; After:
((try-binary-operator trimmed #\/ #'/ env))
```

4. Run tests immediately
5. If passes: commit "refactor: migrate / operator to try-binary-operator"
6. If fails: revert and debug the helper function

### Step 2.2: Migrate simple operators (4 hours)

**Migrate in this order (easiest first):**

1. Division `/` (done in 2.1)
2. Multiplication `*`
3. Addition `+` (but not string concatenation yet)
4. Modulo `%`

**For EACH operator:**
- Change code
- Run `./test-all-examples.sh`
- Verify the specific examples using that operator
- Commit with clear message
- Move to next operator

**Important:** Subtraction `-` is more complex (negative numbers, etc.) - leave it for later.

### Step 2.3: Handle special cases (4 hours)

**Subtraction needs special handling:**

```lisp
(defun try-subtraction (trimmed env)
  "Try to parse subtraction, handling negative numbers"
  (when (and (search "-" trimmed)
             (not (should-skip-operator-p trimmed)))
    (let ((minus-pos (position #\- trimmed)))
      (when (and minus-pos 
                 (> minus-pos 0)  ; Not at start (negative number)
                 (> (length (trim (subseq trimmed 0 minus-pos))) 0))
        (let ((parts (split-string trimmed #\-)))
          (- (eval-expr (trim (car parts)) env)
             (eval-expr (trim (cadr parts)) env)))))))
```

**Addition with concatenation:**

```lisp
(defun try-addition-or-concat (trimmed env)
  "Handle both numeric addition and string concatenation"
  (when (search "+" trimmed)
    (let* ((parts (split-string trimmed #\+))
           (values (mapcar (lambda (p) (eval-expr (trim p) env)) parts)))
      (if (and (numberp (car values)) (numberp (cadr values)))
          (reduce #'+ values)  ; Numeric addition
          (apply #'concatenate 'string 
                 (mapcar (lambda (v) (if (stringp v) v (format nil "~A" v))) 
                         values))))))  ; String concatenation
```

### Step 2.4: Comparison operators (2 hours)

Extract comparison operator handler:

```lisp
(defun try-comparison (trimmed operator-str comparison-fn env)
  "Try to parse and evaluate a comparison operator"
  (when (and (search operator-str trimmed)
             (not (should-skip-operator-p trimmed)))
    (let* ((parts (split-string trimmed operator-str))
           (left-val (eval-expr (trim (car parts)) env))
           (right-val (eval-expr (trim (cadr parts)) env)))
      (funcall comparison-fn left-val right-val))))
```

Migrate comparisons one at a time: `>`, `<`, `>=`, `<=`, `==`, `!=`, `=`

### Week 2-3 Completion Checklist

- [ ] `try-binary-operator` helper created and tested
- [ ] All simple arithmetic operators migrated
- [ ] Subtraction with negative number handling working
- [ ] Addition/concatenation working
- [ ] All comparison operators migrated
- [ ] All 36 tests still pass
- [ ] Committed after each successful migration

**Time investment:** 8-16 hours  
**Benefit:** Operator logic is reusable, easier to understand and test

---

## Week 3-4: Explicit Precedence Table (16-24 hours)

### Goal
Replace implicit order-based precedence with explicit precedence table.

### Step 3.1: Design the precedence table (2 hours)

**Create operator metadata:**

```lisp
(defvar *operator-precedence*
  '(;; Arithmetic (higher precedence = evaluated first)
    ("*" . (:level 40 :fn * :type arithmetic :name "multiplication"))
    ("/" . (:level 40 :fn / :type arithmetic :name "division"))
    ("%" . (:level 40 :fn mod :type arithmetic :name "modulo"))
    ("+" . (:level 30 :fn + :type arithmetic :name "addition"))
    ("-" . (:level 30 :fn - :type arithmetic :name "subtraction"))
    
    ;; Comparisons (lower precedence)
    ("==" . (:level 20 :fn equal :type comparison :name "equality"))
    ("!=" . (:level 20 :fn not-equal :type comparison :name "not-equal"))
    (">" . (:level 20 :fn > :type comparison :name "greater-than"))
    ("<" . (:level 20 :fn < :type comparison :name "less-than"))
    (">=" . (:level 20 :fn >= :type comparison :name "greater-or-equal"))
    ("<=" . (:level 20 :fn <= :type comparison :name "less-or-equal"))
    ("=" . (:level 10 :fn equal :type comparison :name "assignment-or-equality"))
    
    ;; Boolean logic (lowest precedence)
    ("AND" . (:level 5 :fn and-op :type boolean :name "logical-and"))
    ("OR" . (:level 5 :fn or-op :type boolean :name "logical-or")))
  "Operator precedence table. Higher level = evaluated first.
   Precedence levels:
   - 40: Multiplication, division, modulo
   - 30: Addition, subtraction
   - 20: Comparisons
   - 10: Assignment/equality
   - 5: Boolean logic")

(defun get-operator-info (op-string)
  "Get metadata for an operator"
  (cdr (assoc op-string *operator-precedence* :test #'string=)))

(defun operator-precedence (op-string)
  "Get precedence level for an operator (higher = evaluated first)"
  (getf (get-operator-info op-string) :level))
```

**Action:**
1. Add this to the top of the file (after helper functions)
2. Document the precedence levels clearly
3. Test that it compiles
4. Commit: "refactor: add operator precedence table"

### Step 3.2: Create precedence-aware parser (4 hours)

**This is complex - read "Crafting Interpreters" chapter on Pratt parsing first**

Basic structure:
```lisp
(defun find-lowest-precedence-operator (trimmed)
  "Find the operator with lowest precedence to split on (evaluated last)"
  ;; Loop through string
  ;; Track depth of quotes, brackets
  ;; Find operators not in quotes
  ;; Return position and operator with LOWEST precedence
  ...)
```

**Important:** This is the hardest step. Take your time, test incrementally.

### Step 3.3: Migrate to precedence-based parsing (8 hours)

**Do this VERY carefully:**

1. Create new function `eval-expr-precedence` that uses the table
2. Keep old `eval-expr` intact
3. Add flag to switch between old/new: `(defvar *use-precedence-parser* nil)`
4. Test new parser with `*use-precedence-parser*` set to `t`
5. If all tests pass, make it the default
6. Only after 100% confidence, remove old code

### Week 3-4 Completion Checklist

- [ ] Precedence table defined and documented
- [ ] Helper functions for precedence lookup working
- [ ] Precedence-aware parser implemented
- [ ] All operators migrated to use precedence table
- [ ] All 36 tests pass with new parser
- [ ] Old code removed (or kept as fallback with flag)
- [ ] Adding new operator now takes 1 line in table

**Time investment:** 16-24 hours  
**Benefit:** Precedence is explicit, maintainable, and correct

---

## Success Metrics

### Before Refactoring
- God functions: 562 + 871 = 1,433 lines (31% of codebase)
- Duplicated guards: ~100 lines
- Adding operator: 30 minutes, 20+ lines with guards
- Precedence: Hidden in code order
- Test coverage: 36 PASS, 0 FAIL, 1 TIMEOUT

### After Refactoring
- Helper functions: ~100 lines
- Core parsing: ~400 lines (extraction + precedence)
- Total: ~500 lines (65% reduction in complexity)
- Duplicated guards: 0 lines
- Adding operator: 2 minutes, 1 line in table
- Precedence: Explicit table with documentation
- Test coverage: 36 PASS, 0 FAIL, 1 TIMEOUT (no regressions!)

---

## Rollback Plan

If at ANY point things go wrong:

1. **Git is your friend:** Commit after every successful step
2. **Revert broken commits:** `git revert <commit-hash>`
3. **Go back to baseline:** `git reset --hard <last-good-commit>`
4. **Take smaller steps:** Break the change into smaller pieces
5. **Ask for help:** Document what broke and why

**Never push forward if tests fail - always revert to working state first.**

---

## Resources

- **ARCHITECTURE-CRITIQUE.md** - Understanding the problems
- **REFACTORING-GUIDE.md** - Concrete code examples
- **CRITIQUE-SUMMARY.md** - Quick reference
- **Crafting Interpreters** - Learn Pratt parsing (free online)
- **Git commits** - Your safety net, use it!

---

## Questions & Answers

**Q: What if I break something?**
A: Revert the commit immediately. Don't try to fix forward.

**Q: Can I skip testing between steps?**
A: NO. Testing is not optional. Every change must be tested.

**Q: What if tests are slow?**
A: Run specific test files during development, full suite before committing.

**Q: Can I do Week 3 before Week 2?**
A: NO. Follow the order. Each week builds on previous work.

**Q: How do I know if I'm ready for the next step?**
A: All tests pass + code is committed + you understand what you just did.

---

**Remember:** Slow is smooth, smooth is fast. Take your time, test everything, commit frequently.
