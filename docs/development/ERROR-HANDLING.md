# Error Handling Strategy in CNS Interpreter

This document describes the error handling architecture, patterns, and best practices used in the CNS interpreter.

## Table of Contents
1. [Overview](#overview)
2. [Error Types](#error-types)
3. [Error Message Format](#error-message-format)
4. [Error Handling Patterns](#error-handling-patterns)
5. [Best Practices](#best-practices)
6. [Common Scenarios](#common-scenarios)

---

## Overview

### Design Philosophy

The CNS interpreter uses **LLM-friendly structured errors** designed to help both humans and AI systems understand and fix problems. Every error includes:
- **Clear categorization**: What type of error occurred
- **Root cause explanation**: Why it happened
- **Actionable fix instructions**: How to resolve it
- **Working examples**: Correct code to reference

### Error Handling Layers

```
┌─────────────────────────────────────────────────────────┐
│ User Code (CNS files)                                   │
│ ↓ parse/execute errors                                  │
│ Expression System (eval-expr)                           │
│ ↓ evaluation/parsing errors                             │
│ Effect System (handle-*-effect)                         │
│ ↓ runtime errors                                        │
│ Core System (file I/O, network, etc.)                   │
│ ↓ system errors                                         │
│ Common Lisp Runtime (SBCL)                              │
└─────────────────────────────────────────────────────────┘

Errors bubble up with context added at each layer
```

---

## Error Types

### 1. CNS Errors (Structured, User-Friendly)

**Location**: Lines 1373-1456  
**Function**: `make-cns-error`

These are CNS-specific errors with rich context:

#### `:variable-undefined`
**When**: Variable accessed before declaration/assignment
**Function**: `cns-error-undefined-variable` (lines 1420-1428)
**Example**:
```
CNS ERROR: Variable-Undefined
Variable 'count' is not defined

CAUSE: The variable has not been declared in the Given section or assigned
       in any previous step.

FIX: 1. Add 'count' to the Given section with a type and initial value
     2. Or assign it in a previous step before using it

EXAMPLE:
Given:
  count: Number = 0

Step 1: Set count to 42
  Then: Print count
```

#### `:expression-invalid`
**When**: Expression syntax is malformed or unsupported
**Function**: `cns-error-invalid-expression` (lines 1430-1444)
**Common causes**:
- Literal-first expressions: `3 * n` (should be `n * 3`)
- Multi-operator expressions: `a + b * c` (split into steps)
- Missing quotes in comparisons: `name = John` (should be `name = "John"`)
- Missing braces in strings: `Print "Hello name"` (should be `"Hello {name}"`)

#### `:control-flow-invalid`
**When**: Control flow statement used outside If/Otherwise
**Function**: `cns-error-control-flow` (lines 1446-1454)
**Triggers**:
- `go to Step 5` in main step body
- `repeat from Step 1` outside conditional
- `go to End` in Given section

#### `:nil-value`
**When**: Variable is NIL in strict mode (`--strict` flag)
**Function**: `cns-error-nil-value` (lines 1456-1467)
**Purpose**: Catch null pointer-like errors early

---

### 2. System Errors (Low-Level)

**Wrapped by**: `handler-case` blocks throughout codebase  
**Re-raised as**: CNS errors when possible

#### File I/O Errors
**Location**: File effect handlers (lines ~4900-5000)
**Wrapped by**: `handle-file-effect`
**Strategy**: Catch, log, continue (non-fatal)

```lisp
(handler-case
    (progn
      (with-open-file (stream filepath :direction :output)
        (write-string content stream)))
  (error (e)
    (when verbose
      (format t "  Effect: File write failed: ~A~%" e))))
```

#### Network Errors
**Location**: HTTP/socket effect handlers (lines ~3950-4100)
**Wrapped by**: `handle-http-effect`, `handle-socket-effect`
**Strategy**: Return error status, set result to empty string

```lisp
(handler-case
    (make-http-request url)
  (error (e)
    (setf (gethash "response" env) "")
    (when verbose
      (format t "  HTTP request failed: ~A~%" e))))
```

#### Database Errors
**Location**: Database effect handlers (lines ~3825-3923)
**Wrapped by**: `handle-database-effect`
**Strategy**: Set result to empty string, log error

```lisp
(handler-case
    (db-query db-name sql)
  (error (e)
    (setf (gethash target-var env) "")
    (when verbose
      (format t "  DB QUERY failed: ~A~%" e))))
```

#### Parsing Errors
**Location**: Expression evaluation (lines ~4280-4656)
**Wrapped by**: `eval-expr`
**Strategy**: Re-raise as `:expression-invalid` with context

```lisp
(handler-case
    (evaluate-expression expr env)
  (error (e)
    (if (search "CNS ERROR" (format nil "~A" e))
        (error e)  ; Re-raise CNS errors
        (error (cns-error-invalid-expression expr (format nil "~A" e))))))
```

---

## Error Message Format

### Structure

```lisp
(defun make-cns-error (type message &key cause fix example context)
  "Create a structured, LLM-friendly error message."
  (with-output-to-string (s)
    (format s "~%=== CNS ERROR ===~%")
    (format s "TYPE: ~A~%" type)
    (when context
      (format s "CONTEXT: ~A~%" context))
    (format s "~%ERROR: ~A~%" message)
    (when cause
      (format s "~%CAUSE: ~A~%" cause))
    (when fix
      (format s "~%FIX: ~A~%" fix))
    (when example
      (format s "~%EXAMPLE:~%~A~%" example))
    (format s "~%===============~%")))
```

### Context Enrichment

Errors automatically include execution context from global state:
- **File**: `*current-file*` - Which CNS file is running
- **Step**: `*current-step*` - Which step number failed
- **Line**: `*current-code-line*` - Exact code that failed

**Added by**: Error handlers before re-raising
**Example**:
```
CONTEXT: File: examples/factorial.cns, Step: 3
```

---

## Error Handling Patterns

### Pattern 1: Fail Fast (Strict Mode)

**Use when**: Data integrity is critical
**Enabled by**: `--strict` flag
**Behavior**: Any NIL value causes immediate error

```lisp
(when (and *strict-mode* (null value))
  (error (cns-error-nil-value var-name expr)))
```

**Example scenario**: Processing financial data where missing values are unacceptable.

---

### Pattern 2: Graceful Degradation (Default Mode)

**Use when**: Best-effort execution is acceptable
**Enabled by**: Default behavior
**Behavior**: Return empty string/"" for failed operations

```lisp
(handler-case
    (read-file filepath)
  (error (e)
    (setf (gethash "content" env) "")
    (when verbose (format t "File read failed: ~A~%" e))))
```

**Example scenario**: Optional configuration file - continue if missing.

---

### Pattern 3: Validation with Helpful Errors

**Use when**: User input needs validation
**Behavior**: Check constraints, return structured error

```lisp
(defun validate-cli-args (args min-count)
  (when (< (length args) min-count)
    (error (cns-error-invalid-expression
            "CLI ARGS"
            (format nil "Expected at least ~A arguments, got ~A" 
                    min-count (length args))))))
```

**Example scenario**: Command-line tool requiring specific arguments.

---

### Pattern 4: Try-Catch-Continue (Non-Fatal Effects)

**Use when**: Side effects should not crash the program
**Behavior**: Log error, continue execution

```lisp
(handler-case
    (send-network-request url)
  (error (e)
    (when *trace-mode*
      (format t "  Network request failed (non-fatal): ~A~%" e))))
```

**Example scenario**: Logging to external service - continue if unavailable.

---

### Pattern 5: Feature Detection (Optional Dependencies)

**Use when**: Features depend on external libraries/tools
**Behavior**: Check availability, skip gracefully if unavailable

```lisp
(if *db-enabled*
    (db-query db-name sql)
    (progn
      (when verbose (format t "SQLite not available, skipping query~%"))
      (setf (gethash target-var env) "")))
```

**Example scenario**: Database operations when sqlite3 is not installed.

---

## Best Practices

### 1. Always Provide Context

❌ **Bad**: Generic error
```lisp
(error "Invalid input")
```

✅ **Good**: Structured error with context
```lisp
(error (cns-error-invalid-expression 
        expr 
        "Expected number, got string"))
```

---

### 2. Include Execution Context

❌ **Bad**: Error without location
```lisp
(error "Variable not found")
```

✅ **Good**: Error with file/step context
```lisp
(error (make-cns-error 
        :variable-undefined
        (format nil "Variable '~A' not found" var-name)
        :context (format nil "File: ~A, Step: ~A" 
                        *current-file* *current-step*)))
```

---

### 3. Fail Fast for Programming Errors

**Programming errors** (bugs in CNS interpreter):
- Out-of-bounds access
- Type mismatches in internal functions
- Unexpected nil values

→ **Let these crash** with `(error "...")` to surface bugs quickly

**User errors** (mistakes in CNS code):
- Undefined variables
- Invalid syntax
- Type errors in user expressions

→ **Catch and wrap** with `make-cns-error` for helpful feedback

---

### 4. Use handler-case, not ignore-errors

❌ **Bad**: Silent failure
```lisp
(ignore-errors (risky-operation))
```

✅ **Good**: Controlled handling
```lisp
(handler-case
    (risky-operation)
  (error (e)
    (log-error e)
    (return-safe-default)))
```

**Why**: `handler-case` lets you log, retry, or provide fallback behavior.

---

### 5. Re-raise CNS Errors Unchanged

❌ **Bad**: Wrapping CNS errors
```lisp
(handler-case
    (eval-expr expr env)
  (error (e)
    (error (format nil "Evaluation failed: ~A" e))))  ; Loses structure
```

✅ **Good**: Pass through CNS errors
```lisp
(handler-case
    (eval-expr expr env)
  (error (e)
    (if (search "CNS ERROR" (format nil "~A" e))
        (error e)  ; Re-raise as-is
        (error (cns-error-invalid-expression expr (format nil "~A" e))))))
```

---

### 6. Document Error Conditions

For public/exported functions, document error conditions:

```lisp
(defun db-connect (db-name filepath)
  "Connect to SQLite database.
   
   Arguments:
   - db-name: String identifier for this connection
   - filepath: Path to .db file (must exist)
   
   Errors:
   - Raises error if filepath does not exist
   - Raises error if sqlite3 CLI not available
   - Raises error if connection fails
   
   Side effects:
   - Stores connection in *db-connections* hash table"
  ...)
```

---

## Common Scenarios

### Scenario 1: Adding a New Expression Type

**Task**: Add support for `SQRT expression`

**Steps**:
1. Add guard function: `can-parse-sqrt-p`
2. Add parser function: `try-sqrt`
3. Add error handling:
```lisp
(defun try-sqrt (trimmed env)
  (when (can-parse-sqrt-p trimmed)
    (let* ((rest (subseq trimmed 5))
           (value (eval-expr rest env)))
      (if (and (numberp value) (>= value 0))
          (sqrt value)
          (error (cns-error-invalid-expression
                  trimmed
                  "SQRT requires non-negative number"))))))
```

---

### Scenario 2: Adding a New Effect Type

**Task**: Add `EMAIL SEND` effect

**Steps**:
1. Add guard: `can-handle-email-effect-p`
2. Add handler with error handling:
```lisp
(defun handle-email-effect (trimmed env verbose)
  (when (can-handle-email-effect-p trimmed)
    (handler-case
        (progn
          (send-email (extract-recipient trimmed)
                     (extract-subject trimmed)
                     (extract-body trimmed env))
          (when verbose
            (format t "  Effect: Email sent~%")))
      (error (e)
        (when verbose
          (format t "  Effect: Email send failed: ~A~%" e))
        ;; Non-fatal: continue execution
        nil))))
```

---

### Scenario 3: Handling Optional Dependencies

**Task**: Add regex support with graceful degradation

**Pattern**:
```lisp
;; 1. Feature flag
(defvar *regex-enabled* nil)

;; 2. Check availability at startup
(handler-case
    (progn
      (require 'cl-ppcre)
      (setf *regex-enabled* t))
  (error (e)
    (format *error-output* "Regex unavailable: ~A~%" e)))

;; 3. Guard operations
(defun can-parse-matches-p (trimmed)
  (and *regex-enabled*
       (search " MATCHES " (string-upcase trimmed))))

;; 4. Fallback behavior
(defun try-matches (trimmed env)
  (if *regex-enabled*
      (perform-regex-match trimmed env)
      (progn
        (format *error-output* "MATCHES requires cl-ppcre~%")
        nil)))
```

---

### Scenario 4: Debugging Expression Parsing

**Problem**: Expression fails with "invalid expression" error

**Debug steps**:
1. Enable debug mode:
```lisp
(setf *eval-expr-debug* t)
```

2. Run failing code - see which parsers are attempted:
```
DEBUG: Trying literal parser... no match
DEBUG: Trying CLI parser... no match
DEBUG: Trying variable parser... no match
DEBUG: Trying addition operator... MATCH!
DEBUG: Left side: 5
DEBUG: Right side: ERROR - undefined variable
```

3. Fix root cause (undefined variable in this case)

---

### Scenario 5: Iteration Limit Exceeded

**Problem**: Script hits `*max-iterations*` limit

**Cause**: Infinite loop in user code

**Solution**:
1. Add loop termination condition
2. Or increase limit for legitimate use cases:
```lisp
;; In interpreter, before executing
(setf *max-iterations* 100000)  ; For large datasets
```

**Prevention**: Use trace mode to see loop iterations:
```bash
sbcl --script cns.lisp --trace my-script.cns
```

---

## Testing Error Handling

### Unit Test Pattern

```lisp
(defun test-undefined-variable-error ()
  (let ((env (make-hash-table :test #'equal)))
    (handler-case
        (progn
          (eval-expr "undefined_var" env)
          (error "Expected error but got none"))
      (error (e)
        (assert (search "CNS ERROR" (format nil "~A" e)))
        (assert (search "undefined_var" (format nil "~A" e)))
        t))))
```

### Integration Test Pattern

```bash
#!/bin/bash
# Test that undefined variable causes error
echo 'Step 1: Print undefined_var' > /tmp/test.cns
if sbcl --script cns.lisp /tmp/test.cns 2>&1 | grep "Variable.*not defined"; then
  echo "PASS: Error detected"
else
  echo "FAIL: No error raised"
fi
```

---

## Future Improvements

### 1. Error Recovery (v2.0)

Add `TRY/CATCH` blocks to CNS language:
```
Step 5: Try
  Then: Read file "/tmp/data.txt"
  Catch: Set content to "default value"
```

### 2. Warning System

Non-fatal warnings for code smells:
- Unused variables
- Unreachable code
- Deprecated syntax

### 3. Stack Traces

Add call stack to errors:
```
Stack trace:
  1. eval-expr: "calculate_total"
  2. try-variable: "calculate_total"
  3. call-function: "calculate_total"
  4. eval-expr: "price * quantity" <-- ERROR
```

### 4. Error Codes

Assign error codes for programmatic handling:
```
ERROR CNS-001: Variable Undefined
```

### 5. Localization

Support multiple languages for error messages (LLM translation).

---

## Summary

**Key Principles**:
1. **Be helpful**: Every error explains cause and solution
2. **Fail fast**: Catch bugs early, not in production
3. **Graceful degradation**: Continue when reasonable
4. **Context is king**: Always include file/step/line info
5. **Consistency**: Use patterns consistently across codebase

**Remember**: Errors are user interface. Make them excellent.
