# CNS Testing Framework

## Overview

The CNS project has a comprehensive testing infrastructure to ensure code quality, prevent regressions, and validate LLM-generated code. The testing system consists of three main components:

1. **Validation System** - Syntax and semantic checking
2. **Automated Test Runners** - Batch validation and execution
3. **Regression Tests** - Core interpreter functionality tests

## Testing Tools

### 1. CNS Validator (`src/cns-validate`)

A comprehensive validation tool that checks CNS code before execution.

**Usage:**
```bash
./src/cns-validate <file.cns>
```

**What it checks:**
- **Structure**: Presence of required sections (Story, Given, Steps, End)
- **Syntax**: Step arrows (→), proper indentation, End: format
- **Semantics**: Step sequencing, control flow references, variable declarations
- **Causality**: Presence of Because: clauses for each step
- **Effects**: Valid effect declarations

**Example output:**
```
=== CNS Validation Results ===

✓ No errors found
⚠ 1 warning(s):

WARNING: Only 2 'Because:' clauses for 3 steps - causality not fully explained

Overall: VALID (ready for execution)
```

**Exit codes:**
- `0`: Valid (no errors, warnings OK)
- `1`: Invalid (has errors)

### 2. Validation Test Runner (`tests/run-validation-tests.sh`)

Batch validator that tests all CNS files in the project.

**Usage:**
```bash
./tests/run-validation-tests.sh
```

**Coverage:**
- All 15 examples in `examples/`
- All 10 LLM test files in `tests/llm-tests/`
- All 4 Grok iteration files in `tests/grok-iterations/`
- **Total: 29 files**

**Output:**
```
===========================================
   CNS Validation Test Suite
===========================================

Testing examples/...
  [  1] factorial.cns                           PASS
  [  2] fibonacci.cns                           PASS
  ...

===========================================
 Summary
===========================================
Total:         29
Passed:        29
Failed:        0
Pass Rate:     100%

All validation tests passed! ✓
```

### 3. Regression Test Suite (`tests/regression-tests.lisp`)

Unit tests for interpreter core functionality.

**Usage:**
```bash
cd tests && sbcl --script regression-tests.lisp
```

**Test coverage:**
- Helper functions (split-string, trim, starts-with, etc.)
- Parser (parse-cns on real examples)
- HTTP parsing (parse-http-request)
- Route matching (match-route)
- Expression evaluation (eval-expr)
- String extraction (extract-quoted-string)

**Total: 24 tests**

**Output:**
```
===========================================
 CNS Interpreter Regression Tests
===========================================

Testing helper functions...
  [1] PASS: split-string basic
  [2] PASS: trim whitespace
  ...

===========================================
 Test Summary
===========================================
Total:  24
Passed: 24
Failed: 0

All tests passed! ✓
```

### 4. Comprehensive Test Suite (`tests/run-all-tests.sh`)

Full test suite combining validation, execution, and dataset checks.

**Usage:**
```bash
./tests/run-all-tests.sh
```

**Phases:**
1. **Example Validation** - Validates all examples
2. **Example Execution** - Runs non-webserver examples
3. **LLM Test Validation** - Validates LLM-generated code
4. **Dataset Validation** - Checks JSON structure

**Note:** Some tests may timeout or require special setup (e.g., webservers need port availability).

## Test Integration in Development Workflow

### Before Committing Code

Always run validation tests:
```bash
./tests/run-validation-tests.sh
```

### After Interpreter Changes

Run regression tests to prevent breaking changes:
```bash
cd tests && sbcl --script regression-tests.lisp
```

### When Adding New Examples

1. Create the `.cns` file in `examples/`
2. Validate it:
   ```bash
   ./src/cns-validate examples/your-new-example.cns
   ```
3. Run the full validation suite:
   ```bash
   ./tests/run-validation-tests.sh
   ```

### When Modifying Parser/Interpreter

1. Run regression tests first (baseline)
2. Make your changes
3. Run regression tests again (verify no regressions)
4. Run validation suite to ensure examples still parse
5. Add new tests to `regression-tests.lisp` for new functionality

## Common Validation Errors

### 1. Missing Arrow in Step
```
ERROR: Step declaration missing arrow (→)
  Line 7
  Context: Step 2 Do something
```
**Fix:** Add arrow: `Step 2 → Do something`

### 2. Missing Because Clause
```
WARNING: Only 2 'Because:' clauses for 3 steps
```
**Fix:** Add `Because:` to each step explaining the reason

### 3. Wrong End: Format
```
WARNING: End: appears to be on separate line
  Line 15
  Context: End:
```
**Fix:** Use single-line format: `End: Return result`

### 4. References Non-existent Step
```
ERROR: References non-existent Step 5
  Context: Then: repeat from Step 5
```
**Fix:** Ensure referenced step exists or fix step numbering

### 5. Step Numbering Gap
```
WARNING: Step 3 found, expected Step 2
```
**Fix:** Number steps sequentially (1, 2, 3, ...)

## Writing New Tests

### Adding Validation Checks

Edit `src/cns-validator.lisp` and add a new validator function:

```lisp
(defun validate-my-feature (code)
  "Check that my feature is used correctly."
  (let ((errors '()))
    ;; Your validation logic here
    (when (some-error-condition)
      (push (make-error :semantic :warning
                       "Feature not used correctly"
                       :line line-num)
            errors))
    (nreverse errors)))
```

Then add it to the `validate-cns` function's validator list.

### Adding Regression Tests

Edit `tests/regression-tests.lisp` and add test cases:

```lisp
(format t "~%Testing my feature...~%")

(test-equal "my feature basic test"
            (my-function "input")
            "expected-output")

(test-assert "my feature condition"
             (some-condition-is-true))
```

### Adding Example Tests

Just add your `.cns` file to one of these directories:
- `examples/` - General examples
- `tests/llm-tests/` - LLM-generated code tests
- `tests/grok-iterations/` - Iterative improvement tests

The test runner will automatically discover and test it.

## Test Coverage Statistics

### Current Coverage (as of context reduction)

| Category | Files | Tests | Pass Rate |
|----------|-------|-------|-----------|
| Examples | 15 | 15 | 100% |
| LLM Tests | 10 | 10 | 100% |
| Grok Tests | 4 | 4 | 100% |
| Regression | - | 24 | 100% |
| **Total** | **29** | **53** | **100%** |

### Validation Features Tested

- ✓ Story: declaration
- ✓ Given: section
- ✓ Step numbering and arrows (→)
- ✓ Because: clauses
- ✓ End: format
- ✓ Indentation
- ✓ Control flow (repeat from, go to)
- ✓ Effect declarations
- ✓ Parse validity

### Interpreter Features Tested

- ✓ String manipulation (split, trim, starts-with)
- ✓ Parser (structure parsing)
- ✓ HTTP request parsing
- ✓ Route matching
- ✓ Expression evaluation (+, *, >, <, variables)
- ✓ String extraction (with escape sequences)

## Continuous Integration

### Future CI Setup

Recommended GitHub Actions workflow:

```yaml
name: CNS Tests

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Install SBCL
        run: sudo apt-get install sbcl
      - name: Run Validation Tests
        run: ./tests/run-validation-tests.sh
      - name: Run Regression Tests
        run: cd tests && sbcl --script regression-tests.lisp
```

## Testing Best Practices

1. **Test Early, Test Often** - Run validators before committing
2. **Comprehensive Coverage** - Every new feature needs tests
3. **Clear Error Messages** - Validation errors should guide fixes
4. **Fast Feedback** - Validation should be quick (<30s for all files)
5. **Isolated Tests** - Regression tests shouldn't depend on each other
6. **Real Examples** - Use actual CNS files, not synthetic test cases
7. **Document Failures** - If a test fails, understand why before fixing

## Troubleshooting

### Validator Won't Run
```bash
chmod +x src/cns-validate
```

### SBCL Not Found
```bash
# Ubuntu/Debian
sudo apt-get install sbcl

# macOS
brew install sbcl
```

### Timeout in Execution Tests
Some examples may have infinite loops or long execution. Adjust timeouts in `run-all-tests.sh`:
```bash
timeout="${4:-5}"  # Increase from 2 to 5 seconds
```

### False Positives in Validation
If validator reports errors for valid code, file a bug with:
- The CNS code that fails validation
- The error message
- Why you believe it should be valid

## Related Documentation

- **Implementation**: `src/cns.lisp` - Main interpreter
- **Examples**: `examples/` - Working CNS programs
- **Roadmap**: `docs/development/UPDATES.md` - Project direction
- **Agent Guide**: `docs/guides/AGENTS.md` - Development guidelines
