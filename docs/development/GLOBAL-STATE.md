# Global State Management in CNS Interpreter

This document describes all global variables used in the CNS interpreter and their usage patterns.

## Overview

The CNS interpreter uses 15 global variables to manage:
- **Feature flags** (3 variables): Optional dependency status
- **Execution context** (5 variables): Runtime state and debugging
- **Configuration** (2 variables): Iteration limits and CLI arguments
- **Data structures** (3 variables): Shared resources (database connections, functions, operator precedence)
- **Debug flags** (2 variables): Expression and trace debugging

All global state is **thread-unsafe** and designed for single-threaded execution.

---

## Global Variables

### Feature Flags (Optional Dependencies)

#### `*https-enabled*`
- **Type**: Boolean
- **Default**: `nil`
- **Purpose**: Indicates if HTTPS support (cl+ssl + flexi-streams) is available
- **Set by**: Initial load sequence (lines 18-33)
- **Usage**: Checked before creating HTTPS requests
- **Thread-safe**: No (read-only after initialization)

```lisp
(when *https-enabled*
  (create-ssl-stream host port))
```

#### `*regex-enabled*`
- **Type**: Boolean  
- **Default**: `nil`
- **Purpose**: Indicates if regex support (cl-ppcre) is available
- **Set by**: Initial load sequence (lines 36-47)
- **Usage**: Checked before MATCHES and EXTRACT operations
- **Thread-safe**: No (read-only after initialization)

```lisp
(when *regex-enabled*
  (cl-ppcre:scan pattern text))
```

#### `*db-enabled*`
- **Type**: Boolean
- **Default**: `nil`
- **Purpose**: Indicates if SQLite3 CLI tool is available
- **Set by**: Initial load sequence (lines 50-73)
- **Usage**: Checked before database operations
- **Thread-safe**: No (read-only after initialization)

```lisp
(when *db-enabled*
  (db-execute db-name sql))
```

---

### Execution Context (Runtime State)

#### `*current-file*`
- **Type**: String or NIL
- **Default**: `nil`
- **Purpose**: Path to the CNS file currently being executed
- **Set by**: `interpret-cns-file` (line ~5200)
- **Usage**: Error messages, trace output
- **Thread-safe**: No (mutated during execution)
- **Scope**: File-level (set once per file)

```lisp
(setf *current-file* filepath)
(format t "~%File: ~A~%" *current-file*)
```

#### `*current-step*`
- **Type**: Integer or NIL
- **Default**: `nil`
- **Purpose**: Current step number being executed (1-indexed)
- **Set by**: `execute-steps` (line ~5050)
- **Usage**: Error messages, trace output, goto/repeat operations
- **Thread-safe**: No (mutated every step)
- **Scope**: Step-level (updated per step)

```lisp
(setf *current-step* step-num)
(when *trace-mode*
  (format t "Step ~A: ~A~%" *current-step* action))
```

#### `*current-code-line*`
- **Type**: String or NIL
- **Default**: `nil`
- **Purpose**: Source code of current line being executed
- **Set by**: `execute-steps` (line ~5050)
- **Usage**: Error messages, debugging
- **Thread-safe**: No (mutated every step)
- **Scope**: Line-level (updated per line)

```lisp
(setf *current-code-line* code-line)
(error (cns-error-invalid-expression expr))
```

#### `*strict-mode*`
- **Type**: Boolean
- **Default**: `nil`
- **Purpose**: When enabled, NIL values cause immediate errors
- **Set by**: Command-line flag `--strict` (line ~5350)
- **Usage**: Variable access, expression evaluation
- **Thread-safe**: No (set once at startup)
- **Scope**: Global (affects entire execution)

```lisp
(when (and *strict-mode* (null value))
  (error (cns-error-nil-value var-name expr)))
```

#### `*trace-mode*`
- **Type**: Boolean
- **Default**: `nil`
- **Purpose**: When enabled, prints detailed execution trace
- **Set by**: Command-line flag `--trace` (line ~5350)
- **Usage**: Step execution, variable changes, control flow
- **Thread-safe**: No (set once at startup)
- **Scope**: Global (affects entire execution)

```lisp
(when *trace-mode*
  (format t "  → ~A becomes ~A~%" var-name value))
```

---

### Configuration (Limits and Arguments)

#### `*max-iterations*`
- **Type**: Integer
- **Default**: `10000`
- **Purpose**: Maximum loop iterations before throwing error (prevents infinite loops)
- **Set by**: Default declaration (line 125)
- **Usage**: Incremented by `iterate-counter`, checked in loop operations
- **Thread-safe**: No (read-only in practice)
- **Scope**: Global (protects entire execution)

```lisp
(when (> *iteration-counter* *max-iterations*)
  (error "Maximum iterations exceeded"))
```

#### `*iteration-counter*`
- **Type**: Integer
- **Default**: `0`
- **Purpose**: Tracks total iterations across all loops in current execution
- **Set by**: Reset by `interpret-cns-file`, incremented by steps
- **Usage**: Compared against `*max-iterations*`
- **Thread-safe**: No (mutated frequently)
- **Scope**: File-level (reset per file)

```lisp
(setf *iteration-counter* 0) ; Reset at file start
(incf *iteration-counter*)    ; Increment each step
```

#### `*cli-args*`
- **Type**: List of strings
- **Default**: `'()`
- **Purpose**: Command-line arguments passed to CNS program
- **Set by**: `interpret-cns-file` from `(cdr sb-ext:*posix-argv*)` (line ~5200)
- **Usage**: CLI ARG operations, flag/positional access
- **Thread-safe**: No (set once per file)
- **Scope**: File-level (available to all steps)

```lisp
(setf *cli-args* args)
(let ((value (nth index *cli-args*)))
  ...)
```

---

### Data Structures (Shared Resources)

#### `*db-connections*`
- **Type**: Hash table (string → string)
- **Default**: `(make-hash-table :test #'equal)`
- **Purpose**: Maps connection names to database file paths
- **Set by**: `db-connect` effect (line ~3850)
- **Usage**: Database operations (EXECUTE, QUERY)
- **Thread-safe**: No (mutated by DB CONNECT effects)
- **Scope**: Global (shared across all files/steps)

```lisp
(setf (gethash db-name *db-connections*) filepath)
(let ((db-path (gethash db-name *db-connections*)))
  ...)
```

#### `*function-registry*`
- **Type**: Hash table (string → alist)
- **Default**: `(make-hash-table :test #'equal)`
- **Purpose**: Stores user-defined function definitions
- **Set by**: `handle-function-effect` (line ~3600)
- **Usage**: Function calls via `eval-expr`
- **Thread-safe**: No (mutated by DEFINE FUNCTION effects)
- **Scope**: Global (functions persist across steps)

```lisp
(setf (gethash func-name *function-registry*)
      (list (cons :params params) (cons :body body)))
```

#### `*operator-precedence*`
- **Type**: Property list
- **Default**: Defined at line 96-106
- **Purpose**: Defines operator precedence levels (50=multiply, 40=add, 30=relational, 20=equality)
- **Set by**: Declaration (never mutated)
- **Usage**: `get-operator-precedence` helper
- **Thread-safe**: Yes (read-only)
- **Scope**: Global (used by expression parser)

```lisp
(defvar *operator-precedence*
  '((:level 50 :ops (#\* #\/ #\%))
    (:level 40 :ops (#\+ #\-))
    (:level 30 :ops ("<=" ">=" "<" ">"))
    (:level 20 :ops ("==" "!=" "="))))
```

---

### Debug Flags

#### `*eval-expr-debug*`
- **Type**: Boolean
- **Default**: `nil`
- **Purpose**: When enabled, prints which parser matched each expression
- **Set by**: Manually (not exposed via CLI)
- **Usage**: Expression parsing debugging
- **Thread-safe**: No
- **Scope**: Global (for development debugging)

```lisp
(when *eval-expr-debug*
  (format t "DEBUG: Matched literal: ~A~%" result))
```

---

## Usage Patterns

### Initialization Sequence

```
1. Load Quicklisp (lines 10-15)
2. Load optional dependencies → set *https-enabled*, *regex-enabled* (lines 17-47)
3. Check sqlite3 → set *db-enabled* (lines 50-73)
4. Define operator precedence → set *operator-precedence* (lines 96-106)
5. Initialize data structures → *db-connections*, *function-registry* (lines 51, ~3500)
```

### Per-File Execution

```
1. Reset *iteration-counter* to 0
2. Set *current-file* to filepath
3. Set *cli-args* from command-line
4. Parse Given section → initialize environment hash table
5. Execute steps:
   - Set *current-step*, *current-code-line* for each step
   - Mutate *iteration-counter*
   - Optionally use *strict-mode*, *trace-mode*
6. Access shared resources: *db-connections*, *function-registry*
```

### Thread Safety

**None of these variables are thread-safe.** The CNS interpreter is designed for single-threaded execution only. Concurrent execution of multiple CNS files would require:
- Thread-local storage for execution context (*current-file*, *current-step*, etc.)
- Locks for shared data structures (*db-connections*, *function-registry*)
- Separate iteration counters per execution

---

## Best Practices

### Adding New Global State

When adding a new global variable:
1. **Document it** in this file with purpose, default, usage, thread-safety
2. **Choose appropriate scope**: File-level vs. global vs. read-only
3. **Consider alternatives**: Can it be passed as a parameter instead?
4. **Initialize properly**: Set defaults in defvar, reset in appropriate places

### Minimizing Global State

Prefer these alternatives:
- **Environment hash table** (`env`): For user variables and program state
- **Function parameters**: For values used within a single function/call chain
- **Return values**: For computed results that don't need persistence
- **Local bindings** (`let`, `let*`): For temporary values within a function

Only use global state when:
- Value is truly shared across multiple files/steps (like `*db-connections*`)
- Value is needed in deeply nested call chains (like `*current-step*` for errors)
- Value represents global configuration (like `*strict-mode*`)

---

## Future Improvements

### Potential Refactorings
1. **Execution context struct**: Bundle `*current-file*`, `*current-step*`, `*current-code-line*`, `*iteration-counter*` into a single struct passed to functions
2. **Registry object**: Wrap `*db-connections*` and `*function-registry*` in a registry object
3. **Feature flags struct**: Group `*https-enabled*`, `*regex-enabled*`, `*db-enabled*` into a features struct
4. **CLI argument object**: Replace `*cli-args*` list with a structured CLI argument object

### Thread-Safety (Future v2.0+)
If multi-threading is needed:
- Use `bordeaux-threads` or `sb-thread` for thread-local storage
- Protect shared data structures with locks (`sb-thread:mutex`)
- Consider message-passing architecture instead of shared state
