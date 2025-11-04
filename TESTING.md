# CNS Testing Guide

Complete guide to testing CNS programs and validating LLM code generation.

---

## Quick Start

### Test a CNS Program
```bash
# Validate syntax
./cns-validate examples/factorial.cns

# Run program
./cns-run examples/factorial.cns

# Debug with trace
./cns-run --trace examples/factorial.cns
```

### Test LLM Code Generation
```bash
# Test with Grok (default)
./scripts/test-llm --task "Calculate factorial of 10"

# Test with GPT-4
./scripts/test-llm --task "Build HTTP server" --provider openai

# Test with Claude
./scripts/test-llm --task "Create task database" --provider claude
```

---

## Manual Testing

### 1. Validation Testing

**Purpose:** Check syntax without execution

```bash
./cns-validate path/to/program.cns
```

**What it checks:**
- ✅ Story/Given/End structure
- ✅ Variable declarations
- ✅ Step numbering
- ✅ Because clauses
- ✅ Valid syntax
- ✅ No undeclared variables

**Example output:**
```
✓ VALID: examples/factorial.cns
```

### 2. Execution Testing

**Purpose:** Run program and verify output

```bash
./cns-run path/to/program.cns
```

**Options:**
```bash
# Set iteration limit (prevents infinite loops)
./cns-run --max-iterations 1000 program.cns

# Enable trace mode (debug)
./cns-run --trace program.cns

# Strict mode (fail on NIL)
./cns-run --strict program.cns
```

### 3. Trace Mode Debugging

**Purpose:** Step-by-step execution visualization

```bash
./cns-run --trace program.cns
```

**Output shows:**
- Current step number
- Variable states
- Expression evaluations
- Control flow decisions

**Example:**
```
[TRACE] Step 1: count becomes 1
[TRACE]   count = 1
[TRACE] Step 2: If count < 10
[TRACE]   Condition: TRUE
[TRACE]   Action: repeat from Step 1
```

---

## Automated Testing

### 1. Run Example Suite

**Test all examples:**
```bash
./test-all-examples.sh
```

**Output:**
```
Testing: examples/factorial.cns ... ✓ PASS
Testing: examples/fibonacci.cns ... ✓ PASS
Testing: examples/hello.cns ... ✓ PASS
...
Total: 35 tests, 35 passed
```

### 2. Run Regression Tests

**Test core functionality:**
```bash
./tests/run-all-tests.sh
```

Tests:
- Expression evaluation
- Control flow (if/otherwise, loops)
- Functions (calls, recursion)
- File I/O
- HTTP/JSON
- Database operations
- Process management
- String/list/map operations

---

## LLM Testing Framework

### Overview

Test language models' ability to generate valid CNS code.

**Architecture:**
```
Task → SYNTAX.md → llm-tester.py → LLM → Validate → Execute → Save
```

### Basic Usage

```bash
# Simple task
./scripts/test-llm --task "Sum numbers 1 to 100"

# Custom name
./scripts/test-llm --task "Calculate GCD" --name gcd-test

# Different provider
./scripts/test-llm --task "Build web server" --provider claude

# Specify model
./scripts/test-llm --task "Parse CSV" --provider openai --model gpt-4
```

### Configuration

**API Keys (in .env):**
```bash
# Grok (xAI)
GROK_API_KEY=your-key-here

# OpenAI
OPENAI_API_KEY=your-key-here

# Anthropic Claude
ANTHROPIC_API_KEY=your-key-here

# OpenRouter (any model)
OPENROUTER_API_KEY=your-key-here
```

### Test Levels

**Level 1: Basic Math**
```bash
./scripts/test-llm --task "Calculate factorial of 10"
./scripts/test-llm --task "Print first 20 Fibonacci numbers"
./scripts/test-llm --task "Find GCD of 48 and 18"
```

**Level 2: File I/O**
```bash
./scripts/test-llm --task "Count words in file input.txt"
./scripts/test-llm --task "Read CSV and print row count"
```

**Level 3: CLI Tools**
```bash
./scripts/test-llm --task "Build word counter with --lines flag"
./scripts/test-llm --task "Create grep tool for pattern matching"
```

**Level 4: HTTP Servers**
```bash
./scripts/test-llm --task "Build HTTP server with 3 routes"
./scripts/test-llm --task "Create JSON API for user data"
```

**Level 5: Database**
```bash
./scripts/test-llm --task "Create user database with CRUD"
./scripts/test-llm --task "Build task tracker with SQLite"
```

**Level 6: Process Management**
```bash
./scripts/test-llm --task "Launch 3 background jobs and wait"
./scripts/test-llm --task "Build job queue from file"
```

### Output Files

All results saved to `tests/llm-tests/`:

```
tests/llm-tests/
├── generated/                    # Generated CNS code
│   └── task-name_iter1_timestamp.cns
└── results/                      # Test results (JSON)
    └── task-name_timestamp.json
```

**Result JSON:**
```json
{
  "test_name": "factorial-10",
  "model": "grok-2-latest",
  "success": true,
  "total_attempts": 1,
  "attempts": [{
    "validation_passed": true,
    "execution_passed": true,
    "execution_output": "3628800\n"
  }]
}
```

### Batch Testing

**Test multiple tasks:**
```bash
cat > batch-test.sh << 'SCRIPT'
#!/bin/bash
TASKS=(
  "Calculate factorial of 10"
  "Sum numbers 1 to 100"
  "Build HTTP server on port 8080"
)

for task in "${TASKS[@]}"; do
  ./scripts/test-llm --task "$task"
done
SCRIPT

chmod +x batch-test.sh
./batch-test.sh
```

### Compare Models

**Same task, different models:**
```bash
# Test with Grok
./scripts/test-llm --task "Build HTTP server" --name server-grok

# Test with GPT-4
./scripts/test-llm --task "Build HTTP server" --name server-gpt4 --provider openai

# Test with Claude
./scripts/test-llm --task "Build HTTP server" --name server-claude --provider claude

# Compare results
cat tests/llm-tests/results/server-*.json | grep '"success"'
```

---

## Comprehensive Test Suite (Grok Iteration 4)

Located in `tests/grok-iterations/iteration-4/`:

### What It Tests

**3 progressive tests:**
1. **Word Counter** - File I/O, CLI args, flags
2. **Job Manager** - Process management, background jobs
3. **Task Runner API** - HTTP server, JSON, database, multi-route

### Running Tests

```bash
cd tests/grok-iterations/iteration-4
python3 run-grok-tests.py
```

**Output:**
```
Test 1: Word Counter
  ✓ Validation: PASS
  ✓ Execution: PASS

Test 2: Job Manager
  ✓ Validation: PASS
  ✓ Execution: PASS

Test 3: Task Runner API
  ✓ Validation: PASS
  ✓ Execution: PASS

SUCCESS: 3/3 tests passed (100%)
```

### Test Structure

Each test includes:
- **TEST-N-PROMPT.md** - Task description
- **TEST-N-FULL-PROMPT.md** - Complete prompt with SYNTAX.md (1361 lines)
- **test-N-reference.cns** - Hand-written reference implementation
- **results/** - Generated code and validation results

---

## Writing Tests

### Test Structure

```cns
Story: Test Description

Given:
  expected: Integer = 42
  actual: Integer = 0

Step 1 → Perform calculation
  Because: Calculate result
  Then: actual becomes some_expression

Step 2 → Verify result
  Because: Ensure correctness
  If: actual = expected
    Effect: Print "✓ PASS"
  Otherwise:
    Effect: Print "✗ FAIL: expected {expected}, got {actual}"

End: Return actual
```

### Test Best Practices

1. **Clear expectations** - Define expected outputs
2. **Descriptive names** - `test-factorial-positive.cns`
3. **Edge cases** - Test boundary conditions
4. **Error cases** - Test failure modes
5. **Documented** - Add comments explaining test purpose

### Example Test Suite

```bash
tests/
├── unit/
│   ├── test-math-operations.cns
│   ├── test-string-operations.cns
│   └── test-list-operations.cns
├── integration/
│   ├── test-http-json.cns
│   ├── test-database-crud.cns
│   └── test-file-processing.cns
└── llm-tests/
    ├── generated/
    └── results/
```

---

## Performance Testing

### Iteration Limits

**Test loop performance:**
```bash
# Default limit: 10000 iterations
./cns-run program.cns

# Custom limit
./cns-run --max-iterations 100000 program.cns
```

**Measure performance:**
```bash
time ./cns-run examples/fibonacci.cns
```

### Memory Testing

**Monitor memory usage:**
```bash
/usr/bin/time -v ./cns-run program.cns
```

---

## Troubleshooting Tests

### Validation Failures

**Common issues:**
- ❌ Missing `Given:` declarations
- ❌ Undeclared variables
- ❌ Missing `Because:` clauses
- ❌ Invalid syntax (e.g., `arr[0]` instead of `FIRST FROM arr`)

**Fix:**
```bash
# Read error message
./cns-validate program.cns

# Check SYNTAX.md for correct syntax
grep -A 5 "keyword" SYNTAX.md
```

### Execution Failures

**Common issues:**
- ❌ Infinite loops (hit iteration limit)
- ❌ NIL values in expressions
- ❌ File not found
- ❌ Network/database errors

**Debug:**
```bash
# Use trace mode
./cns-run --trace program.cns

# Check for NIL
./cns-run --strict program.cns

# Increase iteration limit
./cns-run --max-iterations 50000 program.cns
```

### LLM Test Failures

**If LLM generates invalid code:**
1. Check `tests/llm-tests/results/` for error details
2. LLM auto-retries with error feedback (up to 3 times)
3. Review generated code in `tests/llm-tests/generated/`
4. Check if SYNTAX.md needs clarification

---

## CI/CD Integration

### GitHub Actions Example

```yaml
name: CNS Tests

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Install SBCL
        run: sudo apt-get install -y sbcl
      - name: Run validation tests
        run: ./tests/run-validation-tests.sh
      - name: Run execution tests
        run: ./tests/run-all-tests.sh
      - name: Test examples
        run: ./test-all-examples.sh
```

---

## Documentation

**Full guides:**
- [LLM Testing Quick Start](QUICK-START-LLM-TESTING.md) - 30-second test
- [LLM Tester README](scripts/LLM-TESTER-README.md) - Complete reference
- [Trace Mode Guide](docs/guides/TRACE-MODE.md) - Debugging
- [Development Testing](docs/development/TESTING.md) - Test framework internals

---

## Summary

**Manual Testing:**
- `./cns-validate` - Syntax checking
- `./cns-run` - Execute program
- `./cns-run --trace` - Debug mode

**Automated Testing:**
- `./test-all-examples.sh` - Example suite
- `./tests/run-all-tests.sh` - Regression tests
- `./scripts/test-llm` - LLM code generation

**Result:** Comprehensive testing at every level, from syntax to LLM generation.
