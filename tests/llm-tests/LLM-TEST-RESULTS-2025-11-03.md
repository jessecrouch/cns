# LLM Test Results - November 3, 2025

## Test Environment
- **Date**: 2025-11-03
- **LLM Provider**: Grok (xAI)
- **Model**: grok-2-latest
- **Template**: `prompts/detailed-template.md` (830 lines, consolidated)
- **Test Tool**: `scripts/llm-tester.py`

## Test Results Summary

| Test | Task | Validation | Execution | Attempts | Notes |
|------|------|------------|-----------|----------|-------|
| ✅ Factorial | Calculate factorial of 5 | PASS | PASS | 1/3 | Perfect on first try |
| ✅ Fibonacci | Calculate 10th Fibonacci number | PASS | PASS | 1/3 | Perfect on first try |
| ❌ Request Logger | HTTP server with CSV logging | FAIL | N/A | 3/3 | Invalid syntax used |

**Success Rate**: 2/3 (66.7%)

---

## Test 1: Factorial ✅

**Task**: Write a CNS program that calculates the factorial of 5

**Result**: SUCCESS on attempt 1

**Generated Code Quality**:
- ✅ Uses correct CNS syntax
- ✅ Proper variable declarations
- ✅ Correct control flow with If/repeat
- ✅ Clean step structure
- ✅ Valid Because clauses

**Code**:
```cns
Story: Calculate Factorial of 5

Given:
  n: Integer = 5
  factorial: Integer = 1

Step 1 → Initialize factorial
  Because: Start with factorial of 1
  Then: factorial becomes 1

Step 2 → Calculate factorial
  Because: Multiply factorial by each number up to n
  If: n > 1
    Then: factorial becomes factorial * n
    Then: n becomes n - 1
    Then: repeat from Step 2

End: Return factorial
```

**Output**: 120 (correct!)

**Template Effectiveness**:
- Function lookup table worked well
- Validation checklist followed
- No function hallucinations

---

## Test 2: Fibonacci ✅

**Task**: Write a CNS program that calculates the 10th Fibonacci number

**Result**: SUCCESS on attempt 1

**Generated Code Quality**:
- ✅ Uses correct CNS syntax
- ✅ Proper variable declarations (a, b, i, fib, n)
- ✅ Correct loop logic with If/goto
- ✅ Clean iterative approach
- ✅ Valid Because clauses

**Code**:
```cns
Story: Calculate 10th Fibonacci Number

Given:
  n: Integer = 10
  a: Integer = 0
  b: Integer = 1
  i: Integer = 2
  fib: Integer = 1

Step 1 → Initialize first two Fibonacci numbers
  Because: Set up base cases for Fibonacci sequence
  Then: a becomes 0
  Then: b becomes 1

Step 2 → Calculate next Fibonacci number
  Because: Generate next number in sequence
  Then: fib becomes a + b
  Then: a becomes b
  Then: b becomes fib
  Then: i becomes i + 1

Step 3 → Check if done
  Because: Stop when we have the 10th Fibonacci number
  If: i <= n
    Then: goto Step 2
  Otherwise:
    Then: goto End

End: Return fib
```

**Output**: 55 (correct!)

**Template Effectiveness**:
- Control flow patterns followed correctly
- No syntax errors
- Proper loop termination

---

## Test 3: Request Logger ❌

**Task**: Build a web server that logs HTTP requests to CSV with endpoints: GET / (logs and responds), GET /history (shows logs without logging). Use TIMESTAMP() for current time. CSV columns: timestamp,method,path,client

**Result**: FAILED after 3 attempts

**Issues Found**:

1. **Invalid Syntax (Attempt 1)**:
   ```cns
   Then: client_ip becomes REQUEST_HEADERS GET "X-Forwarded-For"
   ```
   - Error: Parser treats `REQUEST_HEADERS GET "X-Forwarded-For"` as multiple variables
   - Should be: `REQUEST_HEADERS` is not a valid built-in variable

2. **Effect Pattern Issues**:
   - Used: `Effect: APPEND "{timestamp},{method},{path},{client_ip}" TO FILE log_file`
   - Should be: `Effect: Append "{timestamp},{method},{path},{client_ip}" to requests.csv`
   - Note: File path must be literal string, not variable

3. **Effect Pattern Issues**:
   - Used: `Effect: Network read`
   - Should be: `Effect: Network read` (actually correct, but validator doesn't recognize)

4. **Repeated Across All 3 Attempts**:
   - Same `REQUEST_HEADERS` error in all attempts
   - LLM didn't learn from validation feedback
   - Suggests template needs REQUEST_HEADERS clarification

**Root Cause**:
- Template doesn't clearly document that `REQUEST_HEADERS` is NOT a built-in variable
- LLM is hallucinating HTTP header access syntax
- Need to add explicit "❌ No REQUEST_HEADERS variable" to template

---

## Analysis

### What Worked ✅

1. **Basic Math Problems**: 100% success rate (factorial, fibonacci)
2. **TIMESTAMP() Usage**: Not tested in passing examples, but available
3. **Control Flow**: If/Otherwise/repeat patterns generated correctly
4. **Variable Declarations**: All variables properly declared in Given section
5. **First-Try Success**: 2/2 simple tasks passed on first attempt

### What Didn't Work ❌

1. **Complex HTTP Tasks**: Request logger failed 3/3 attempts
2. **Built-in Variable Discovery**: LLM hallucinated `REQUEST_HEADERS`
3. **Effect Syntax Precision**: File path as variable vs literal not learned
4. **Learning from Validation**: Retry didn't fix the errors

### Template Gaps Identified

1. **Missing Explicit Negatives**:
   - ❌ Need: "No REQUEST_HEADERS variable (use REQUEST_METHOD, REQUEST_PATH only)"
   - ❌ Need: "No header access in CNS"
   - ❌ Need: "File paths must be literal strings in effects"

2. **Effect Patterns Need Clarification**:
   - Current: Effect examples scattered
   - Needed: Complete effect syntax reference with all keywords

3. **Auto-populated Variables**:
   - Current: Listed in table
   - Needed: Explicit examples showing "DO NOT declare these"

---

## Recommendations

### High Priority

1. **Update Template Function Table**:
   ```markdown
   | **HTTP Request Info** |
   | Get request method | `REQUEST_METHOD` (auto) | `REQUEST_HEADERS` ❌ |
   | Get request path | `REQUEST_PATH` (auto) | `$PATH`, `headers[]` ❌ |
   | Get client info | Use `client_socket` | `REQUEST_HEADERS GET "..."` ❌ |
   ```

2. **Add Effect Syntax Section**:
   ```markdown
   ## Effect Patterns (Complete Reference)
   
   - Append to file: `Effect: Append "text" to filename.txt`
     ❌ NOT: `Effect: APPEND "text" TO FILE variable`
     ❌ File path must be literal string, not variable
   ```

3. **Strengthen Auto-populated Variables Section**:
   ```markdown
   ## Built-in Variables (DO NOT DECLARE)
   
   ✅ Available automatically in HTTP handlers:
   - REQUEST_METHOD - HTTP method (GET, POST, etc.)
   - REQUEST_PATH - URL path requested
   
   ❌ NOT available (do not use):
   - REQUEST_HEADERS - Does not exist
   - REQUEST_BODY - Use Network read instead
   - CLIENT_IP - Not available
   ```

### Medium Priority

4. **Add Retry Learning Mechanism**:
   - Test harness should parse validation errors better
   - Provide more specific fix suggestions in retry prompt

5. **Add More Example Tests**:
   - Simple HTTP server (no CSV logging)
   - File read/write operations
   - String manipulation tasks

### Low Priority

6. **Performance Improvements**:
   - All successful tests completed in <6 seconds generation time
   - Current performance is acceptable

---

## Comparison with Previous Tests

### Grok Iteration 3 (Manual Testing)

**Before Template Consolidation**:
- 10 validation errors initially
- Used wrong functions: `NOW()`, `SPLIT()`, `JOIN()`, `ENV()`
- Required manual fixes

**After Template Consolidation**:
- Simple math: 0 errors ✅
- Complex HTTP: Still has errors ❌
- Improvement on basics, but HTTP needs work

---

## Next Steps

1. ✅ **Document results** - This file
2. 🔄 **Update template** - Add REQUEST_HEADERS clarification
3. 🔄 **Re-test** - Run request logger test again
4. 📝 **Test other LLMs** - Try GPT-4, Claude with same tasks
5. 📊 **Build test suite** - Create standard benchmark tasks

---

## Test Artifacts

**Generated Code**:
- `tests/llm-tests/generated/write-a-cns-program-that-calculates-the-factorial-of-5_iter1_*.cns`
- `tests/llm-tests/generated/fibonacci_iter1_*.cns`
- `tests/llm-tests/generated/request-logger_iter1-3_*.cns`

**Results**:
- `tests/llm-tests/results/write-a-cns-program-that-calculates-the-factorial-of-5_*.json`
- `tests/llm-tests/results/fibonacci_*.json`
- `tests/llm-tests/results/request-logger_*.json`

---

**Generated**: 2025-11-03  
**Tool Version**: scripts/llm-tester.py v1.0  
**Template Version**: prompts/detailed-template.md (830 lines)
