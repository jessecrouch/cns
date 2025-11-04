# eval-expr Refactoring Plan

**Date**: November 4, 2025  
**Scope**: Refactor the 606-line `eval-expr` function into a maintainable parser registry architecture  
**Goal**: Fix FILE EXISTS literal issue + improve maintainability for future development  
**Approach**: Same pattern that worked for effect handlers (Nov 3 refactoring)

---

## Current Problems

### 1. Monolithic Function
- `eval-expr` is 606 lines with 60+ conditions
- Order-dependent evaluation (earlier conditions shadow later ones)
- Hard to debug (spent 2+ hours on FILE EXISTS truncation issue)
- Hard to add new features safely

### 2. Implicit Precedence
Comments like "MUST come before - operator!" but no enforcement.

### 3. The FILE EXISTS Bug
`FILE EXISTS "/tmp/file.txt"` gets truncated to `FILE EXISTS "` during parsing because:
- Something before the FILE EXISTS check at line 4220 is intercepting expressions with quotes
- Unable to pinpoint exact cause due to complexity
- Workaround: Use variables (`FILE EXISTS path_var`)

---

## Architecture Design

### Parser Registry Pattern (Same as Effects)

Just like we refactored `apply-effect` into specialized handlers, we'll refactor `eval-expr` into specialized parsers.

**Before (Effect System - Old):**
```lisp
(defun apply-effect (effect-str env verbose)
  (cond
    ((starts-with trimmed "PRINT ") ...)
    ((starts-with trimmed "WRITE ") ...)
    ((starts-with trimmed "DELETE FILE ") ...)
    ;; 50+ more inline conditions
    ))
```

**After (Effect System - Nov 3 Refactoring):**
```lisp
(defun apply-effect (effect-str env verbose)
  (cond
    ((can-handle-print-effect-p trimmed)
     (handle-print-effect trimmed env verbose))
    ((can-handle-write-effect-p trimmed)
     (handle-write-effect trimmed env verbose))
    ((can-handle-delete-file-effect-p trimmed)
     (handle-delete-file-effect trimmed env verbose))
    ;; Clean dispatcher pattern
    ))
```

**We'll do the same for expressions:**

```lisp
(defun eval-expr (expr env &optional context)
  (cond
    ((numberp expr) expr)
    ((stringp expr)
     (let ((trimmed (trim expr)))
       (cond
         ;; PHASE 1: Literals (never split these)
         ((can-parse-string-literal-p trimmed)
          (try-parse-string-literal trimmed env))
         
         ((can-parse-filepath-literal-p trimmed)
          (try-parse-filepath-literal trimmed env))
         
         ;; PHASE 2: Built-in functions (high precedence)
         ((can-parse-env-p trimmed)
          (try-parse-env trimmed env))
         
         ((can-parse-arg-p trimmed)
          (try-parse-arg trimmed env))
         
         ((can-parse-file-exists-p trimmed)
          (try-file-exists trimmed env))  ; MOVED UP HERE!
         
         ;; PHASE 3: Operations
         ((can-parse-starts-with-p trimmed)
          (try-starts-with trimmed env))
         
         ;; PHASE 4: Operators (lowest precedence)
         ((can-parse-addition-p trimmed)
          (try-addition trimmed env))
         
         ;; Fallback
         (t (try-variable-lookup trimmed env context)))))))
```

---

## Refactoring Phases

### PHASE 1: Extract Parsers (No Behavior Change)
**Goal**: Extract existing logic into `can-parse-X-p` / `try-X` pairs  
**Risk**: Low (just moving code)  
**Test**: After each extraction, verify 38/38 tests pass

**Groups to Extract (in order):**

1. **Literals** (lines 3813-3858):
   - String literals: `"hello world"`
   - Filepath literals: `/absolute/path`
   - Boolean literals: `TRUE`, `FALSE`, `T`, `NIL`
   - Number literals (already handled)

2. **CLI/Environment** (lines 3859-3930):
   - `ENV("VAR")` / `ENV("VAR", "default")`
   - `ARG("--flag")` / `ARG("--flag", "default")`
   - `HAS_FLAG("--flag")`
   - `ARGS[n]`

3. **Date/Time** (lines 3931-3941):
   - `NOW()`
   - `TIMESTAMP()`

4. **Function Calls** (lines 3942-3962):
   - User-defined functions: `FuncName(arg1, arg2)`

5. **List Literals** (lines 3963-3973):
   - `[1, 2, 3]` or `[]`

6. **Assignment** (lines 3974-3983):
   - `var becomes expr`

7. **Math Functions** (lines 3984-4054):
   - Already extracted: `can-parse-sqrt-p`, etc.
   - Just need to verify they're all separate

8. **Boolean Operators** (lines 4155-4180):
   - Already extracted: `can-parse-boolean-or-p`, etc.

9. **Comparisons** (lines 4060-4090):
   - Already extracted: `can-parse-comparison-operator-p`, etc.

10. **Arithmetic Operators** (lines 4091-4150):
    - Need extraction: `+`, `-`, `*`, `/`, `%`

11. **JSON Operations** (lines 4161-4217):
    - `PARSE JSON`

12. **File Operations** (lines 4218-4227):
    - `READ FROM FILE`
    - `FILE EXISTS`

13. **String Operations** (lines 4228-4280):
    - Already extracted: `can-parse-starts-with-p`, etc.

---

### PHASE 2: Reorganize Order (Fix FILE EXISTS)
**Goal**: Move FILE EXISTS before operators  
**Risk**: Medium (changes evaluation order)  
**Test**: Verify FILE EXISTS works with literals

**New Order:**
```
1. Literals (string, number, boolean, filepath)
2. CLI/Environment (ENV, ARG, HAS_FLAG, ARGS)
3. Date/Time (NOW, TIMESTAMP)
4. File Operations (FILE EXISTS, READ FROM FILE) ← MOVED UP
5. Math Functions (SQRT, POW, etc.)
6. String Operations (STARTS WITH, CONTAINS, etc.)
7. JSON Operations (PARSE JSON)
8. Function Calls (user functions)
9. List Literals ([1,2,3])
10. Assignment (becomes)
11. Boolean Operators (OR, AND, NOT)
12. Comparisons (==, !=, <, >, etc.)
13. Arithmetic Operators (+, -, *, /, %)
14. Variable Lookup (fallback)
```

---

### PHASE 3: Add Debug Mode
**Goal**: Make debugging easier in the future  
**Risk**: Low (additive only)

```lisp
(defparameter *eval-expr-debug* nil
  "When T, print which parser matched each expression.")

(defun eval-string-expr (trimmed env context)
  (when *eval-expr-debug*
    (format t "[EVAL-EXPR] Parsing: ~S~%" trimmed))
  (cond
    ((can-parse-string-literal-p trimmed)
     (when *eval-expr-debug* (format t "[EVAL-EXPR] → STRING_LITERAL~%"))
     (try-parse-string-literal trimmed env))
    ;; ... etc
    ))
```

---

## Implementation Plan

### Step 1: Baseline Test ✓
```bash
./test-all-examples.sh
# Expect: 38/38 tests passing
```

### Step 2: Extract Literals (Iteration 1)
**Extract to new functions:**
- `can-parse-string-literal-p` / `try-parse-string-literal`
- `can-parse-filepath-literal-p` / `try-parse-filepath-literal`
- `can-parse-boolean-literal-p` / `try-parse-boolean-literal`

**Location**: After line 836 (near other parsers)

**Test after each extraction**:
```bash
./test-all-examples.sh
```

### Step 3: Extract CLI/Environment (Iteration 2)
**Extract to new functions:**
- `can-parse-env-p` / `try-parse-env`
- `can-parse-arg-p` / `try-parse-arg`
- `can-parse-has-flag-p` / `try-parse-has-flag`
- `can-parse-args-index-p` / `try-parse-args-index`

**Test after extraction**

### Step 4: Extract Remaining Groups (Iterations 3-10)
Continue pattern for each group.

### Step 5: Reorganize Order
Move FILE EXISTS to group 4 (before operators).

**Test specifically**:
```bash
# Test FILE EXISTS with literal
cat > /tmp/test-exists-literal.cns << 'EOF'
Story: Test
Given:
  x: Boolean
Step 1 → Test
  Then: x becomes FILE EXISTS "/tmp"
  Effect: Print "x = {x}"
End: Return 0
EOF
./cns-run /tmp/test-exists-literal.cns
```

### Step 6: Add Debug Mode
Add `*eval-expr-debug*` parameter and logging.

### Step 7: Final Testing
```bash
./test-all-examples.sh  # Must be 38/38
./tests/run-validation-tests.sh  # Must be 100%
```

---

## Success Criteria

1. **No Regressions**: 38/38 tests must pass after each phase
2. **Fix FILE EXISTS**: `FILE EXISTS "/tmp/file.txt"` works with literals
3. **Improved Maintainability**: Each parser in its own function
4. **Clear Precedence**: Order is explicit and documented
5. **Easy Debugging**: Debug mode shows which parser matched

---

## Rollback Plan

If any phase fails:
1. Git revert to last working commit
2. Analyze failure with smaller change
3. Fix issue before continuing

---

## Estimated Time

- Phase 1 (Extract): 2-3 hours (test after each group)
- Phase 2 (Reorder): 30 minutes
- Phase 3 (Debug): 30 minutes
- Total: 3-4 hours of careful, incremental work

---

## Notes

- **Test frequently**: After every 2-3 extractions
- **Commit frequently**: After each working group
- **Use git**: Easy rollback if something breaks
- **Follow effect pattern**: Same style as Nov 3 refactoring
- **Keep original comments**: They document intent

---

*This plan follows the proven pattern from the Nov 3, 2025 effect handler refactoring that successfully extracted 15 handlers with zero regressions.*
