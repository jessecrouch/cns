# LLM-First Improvements for CNS

## Core Principle
**CNS is designed for LLMs to write code, not humans.** Every improvement must make it easier for LLMs to generate correct CNS code on the first try.

---

## Session Summary: What We Learned

### Session Date: November 2, 2025
**Observer:** Human watching AI debug CNS code  
**Key Insight:** "I noticed you were having difficulty reading, writing, interpreting CNS"

### Problems Identified While AI Debugged CNS

1. **Silent expression failures** - `3 * n + 1` → NIL with no error
2. **Unclear control flow rules** - "repeat from Step" only works in If branches
3. **NIL propagation** - Variables become NIL, loops run 74,000+ iterations
4. **No iteration safety** - Infinite loops timeout instead of erroring
5. **Ambiguous syntax** - Indentation rules unclear

---

## Priority 1: Better Error Messages (LLM-Focused)

### Problem
LLMs generate code that looks syntactically correct but fails silently at runtime.

### Solution: Error Messages with Code Examples

**Before:**
```
ERROR: Could not evaluate 'n % 2'
```

**After (LLM-friendly):**
```
ERROR at Step 3, line 19: "Then: n becomes 3 * n + 1"
  Expression "3 * n + 1" evaluated to NIL
  
  CAUSE: CNS doesn't support literal-first expressions in complex operations
  
  FIX: Reorder or split the expression
  
  ✓ WORKING ALTERNATIVES:
    Then: temp becomes n * 3
    Then: n becomes temp + 1
  
  OR:
    Then: n becomes n * 3 + 1   (variable first)
```

**Why This Helps LLMs:**
- Shows exact line and context
- Provides working code examples to copy
- LLM can learn pattern from the error message itself

---

## Priority 2: Validation Mode (Pre-Runtime Checking)

### Problem
LLMs generate code that parses but won't run correctly. Runtime debugging is expensive.

### Solution: `cns-validate` command

**Usage:**
```bash
./cns-validate examples/my-program.cns

✓ Syntax valid
✓ All variables declared
✗ Control flow error at Step 3, line 14

  Step 3:
    Then: x becomes x + 1
    Then: repeat from Step 1    ← ERROR HERE
  
  INVALID: Control flow in regular step
  
  Control flow ("repeat from", "go to") only works inside If/Otherwise branches.
  
  FIX:
    Step 3:
      Then: x becomes x + 1
      If: x > 0
        Then: repeat from Step 1
      Otherwise:
        Then: go to End

⚠ Warning at Step 2, line 10
  Expression "result + count * 2" has multiple operators
  CNS evaluator may misparse this. Consider using parentheses or temporary variables.

Summary: 1 error, 1 warning
```

**Why This Helps LLMs:**
- Catches errors before execution
- Provides corrected code in error message
- LLM can incorporate fixes into next generation
- Warnings teach LLM about known limitations

---

## Priority 3: Strict Mode (Type Safety)

### Problem
Variables silently become NIL, causing infinite loops and confusing behavior.

### Solution: Optional strict mode in Story declaration

**Syntax:**
```cns
Story: My Program [strict]

Given:
  n: Integer = 5
  result: Integer = 0

Step 1:
  Then: result becomes 3 * n + 1    ← Would ERROR immediately if result is NIL
```

**Error Example:**
```
ERROR at Step 1, line 8: "result becomes 3 * n + 1"
  Variable 'result' was assigned NIL (expected Integer)
  Expression "3 * n + 1" evaluated to NIL
  Previous value: 0
  
  This is a STRICT MODE error. The program has stopped.
  
  Common causes:
  - Expression parser limitation (see docs/EXPRESSION-LIMITATIONS.md)
  - Type mismatch in operation
  - Undefined function call
```

**Why This Helps LLMs:**
- Fail fast instead of propagating errors
- Clear cause-and-effect (expression → NIL → error)
- LLM learns to avoid patterns that produce NIL

---

## Priority 4: Iteration Safety (Infinite Loop Protection)

### Problem
Infinite loops run until timeout (60+ seconds), wasting compute.

### Solution: Automatic iteration limit with helpful error

**Default Behavior:**
```bash
./cns-run examples/my-program.cns

# After 10,000 iterations:
ERROR: Iteration limit exceeded (10,000 iterations)
  Currently at: Step 3, line 18
  
  State snapshot:
    n = NIL
    steps = 9847
    temp = NIL
  
  LIKELY CAUSE: Infinite loop
  
  Common patterns:
  1. Variable became NIL, condition never met
  2. Loop condition always true
  3. Forgot to update loop counter
  
  To increase limit: ./cns-run --max-iterations 50000 examples/my-program.cns
  To debug: ./cns-run --trace examples/my-program.cns
```

**Why This Helps LLMs:**
- Fast feedback (10K iterations ~1 second vs 60 second timeout)
- Shows current state at failure point
- Teaches common error patterns
- Provides debugging commands

---

## Priority 5: Trace Mode (Execution Visibility)

### Problem
Hard to understand what code is actually doing, especially in loops.

### Solution: `--trace` flag with smart output

**Usage:**
```bash
./cns-run --trace examples/collatz.cns

=== Trace Mode: First 10 iterations, then every 10th ===

[Iter 1] Step 1 → n=27, steps=0
[Iter 1] Step 2 → steps=1
[Iter 1] Step 3 (Otherwise branch) → temp=81, n=82

[Iter 2] Step 1 → n=82, steps=1
[Iter 2] Step 2 → steps=2
[Iter 2] Step 3 (If branch) → n=41

...

[Iter 10] → n=5, steps=10
[Iter 20] → n=5, steps=20  ← LOOP DETECTED: Same state as iteration 10
[Iter 30] → n=5, steps=30  

WARNING: Variable 'n' hasn't changed in 20 iterations
Possible infinite loop detected.

Continue? [y/N]
```

**Why This Helps LLMs:**
- See actual execution flow
- Detect loops early
- Understand state changes
- Learn from execution patterns

---

## Priority 6: Expression Limitations Documentation

### Problem
LLMs don't know which expressions work and which don't.

### Solution: Machine-readable limitations file

**File: `docs/EXPRESSION-LIMITATIONS.md`**
```markdown
# CNS Expression Limitations (For LLM Code Generators)

## ✓ SUPPORTED Expressions

### Simple Binary Operations
- `a + b` ✓
- `a - b` ✓
- `a * b` ✓
- `a / b` ✓
- `a % b` ✓

### Comparisons
- `a = b` ✓
- `a > b` ✓
- `a < b` ✓

### Variable-First Complex
- `a + b + c` ✓
- `a * b + c` ✓
- `a + b * c` ✓ (left-to-right evaluation)

## ✗ UNSUPPORTED Patterns

### Literal-First Expressions
- `3 * n` → NIL ✗
- `5 + a` → NIL ✗

### Multi-Operator with Literals
- `n * 3 + 1` → NIL ✗
- `a + 5 * b` → NIL ✗

## ✓ WORKAROUNDS

### Pattern 1: Reorder to Variable-First
```cns
# WRONG:
Then: result becomes 3 * n + 1    ✗

# RIGHT:
Then: result becomes n * 3 + 1    ✓
```

### Pattern 2: Split Into Steps
```cns
# WRONG:
Then: result becomes n * 3 + 1    ✗

# RIGHT:
Then: temp becomes n * 3
Then: result becomes temp + 1     ✓
```

### Pattern 3: Reverse Operation
```cns
# WRONG:
Then: result becomes count / 2    (if 2 is literal)

# RIGHT:
Then: result becomes count / 2    ✓ (this actually works)
```

## LLM Code Generation Rules

1. **Always use variable-first order in expressions**
2. **Limit expressions to 2 variables max**
3. **Avoid mixing literals and variables in same expression**
4. **Use temporary variables for complex math**

## Testing Your Expression

Use this CNS snippet to test if an expression will work:

```cns
Story: Expression Test [strict]

Given:
  a: Integer = 5
  b: Integer = 3
  result: Integer = 0

Step 1 → Test
  Then: result becomes YOUR_EXPRESSION_HERE
  Effect: Print "Result: {result}"

End: Return result
```

If `result` is NIL, the expression is unsupported.
```

**Why This Helps LLMs:**
- Clear rules for code generation
- Examples of working patterns
- Self-service testing method
- Can be included in LLM context window

---

## Priority 7: Control Flow Rules Documentation

### File: `docs/CONTROL-FLOW-RULES.md`

```markdown
# CNS Control Flow Rules (For LLM Code Generators)

## Rule 1: Control Flow ONLY in If/Otherwise

### ✓ VALID
```cns
Step 1:
  If: x > 0
    Then: x becomes x - 1
    Then: repeat from Step 1    ✓
  Otherwise:
    Then: go to End              ✓
```

### ✗ INVALID
```cns
Step 1:
  Then: x becomes x - 1
  Then: repeat from Step 1       ✗ WILL BE IGNORED!
```

## Rule 2: Control Flow Must Be Last Then

```cns
# ✓ RIGHT: Control flow is last
If: x > 0
  Then: x becomes x - 1
  Then: y becomes y + 1
  Then: repeat from Step 1       ✓

# ✗ WRONG: Then after control flow
If: x > 0
  Then: repeat from Step 1
  Then: x becomes x - 1          ✗ NEVER EXECUTES!
```

## Rule 3: Control Flow Types

### repeat from Step N
- Jumps back to Step N
- Use for loops
- Example: Counting, iteration

### go to Step N
- Jumps forward to Step N
- Use for skip logic
- Example: Early exit from sequence

### go to End
- Exits program immediately
- Returns End value
- Example: Exit condition met

## LLM Code Generation Pattern

**Template for loops:**
```cns
Step N → Loop body
  If: CONTINUE_CONDITION
    Then: UPDATE_VARIABLES
    Then: repeat from Step N
  Otherwise:
    Then: go to End
```

**Template for conditional skip:**
```cns
Step N → Check
  If: SKIP_CONDITION
    Then: go to Step M
  
Step N+1 → Skippable work
  Then: ...
  
Step M → Continue
  Then: ...
```
```

---

## Implementation Plan

### Phase 1: Error Messages ✅ COMPLETE
- [x] Add context to all ERROR outputs (file, line, step)
- [x] Add "FIX:" section with code examples
- [x] Add "CAUSE:" explanation
- [x] Infrastructure exists via make-cns-error function

### Phase 2: Validation Mode ✅ COMPLETE
- [x] Create `cns-validate` command (already exists)
- [x] Add control flow validation (comprehensive)
- [x] Add expression complexity warnings (built-in)
- [x] Add undefined variable detection (working)
- [x] Output LLM-friendly format (structured errors)

### Phase 3: Strict Mode ✅ COMPLETE
- [x] Add `[strict]` flag parsing (working: `Story: Name [strict]`)
- [x] Add type checking on assignment (operational)
- [x] Add NIL detection with immediate error (check-strict-nil)
- [x] Add helpful context in errors (cns-error-nil-value)

### Phase 4: Safety Features ✅ COMPLETE
- [x] Add iteration counter to interpreter (*iteration-counter*)
- [x] Add `--max-iterations` flag (default: 10,000)
- [x] Add state snapshot on error (in cns-error-iteration-limit)
- [x] Fast detection: 1 sec vs 60 sec timeout (60x improvement)

### Phase 5: Trace Mode ⚠️ PARTIAL
- [x] Add `--trace` flag (flag parsing complete in cns-run)
- [ ] Smart output (first 10, then every Nth) - function exists, needs integration
- [ ] Loop detection warnings - needs integration
- [ ] Interactive continue prompt - future enhancement

### Phase 6: Documentation ✅ COMPLETE
- [x] Write EXPRESSION-LIMITATIONS.md (620 lines, comprehensive)
- [x] Write CONTROL-FLOW-RULES.md (469 lines, with templates)
- [x] Add examples for each limitation (working code samples)
- [x] Create self-test snippets (copy-paste ready)

### Phase 7: LLM Integration 🔄 IN PROGRESS
- [x] Create condensed rules for LLM context (in docs/language/)
- [ ] Test with Claude/GPT generating CNS (needs empirical testing)
- [ ] Iterate based on LLM errors (ongoing)
- [x] Build few-shot examples (42 working examples)

---

## Success Metrics

### For LLMs:
- **First-try success rate**: % of generated CNS that runs without errors
- **Error recovery**: % of errors fixed by reading error message
- **Validation pass rate**: % of code that passes cns-validate

### For Development:
- **Debug time**: Average time from error to fix
- **Iteration count**: How many test runs needed
- **Time to working code**: First idea to working program

**Target Improvements:**
- 60-70% reduction in debug time
- 80%+ first-try success for LLM-generated code
- 90%+ of errors caught by validation (not runtime)

---

## LLM-Friendly Error Format Specification

### Standard Error Template
```
ERROR at <location>: "<code>"
  <brief explanation>
  
  CAUSE: <why this failed>
  
  FIX: <how to fix it>
  
  ✓ WORKING EXAMPLE:
    <corrected code>
```

### Example Implementation
```lisp
(defun llm-error (location code cause fix example)
  (format t "ERROR at ~A: \"~A\"~%" location code)
  (format t "  ~A~%~%" cause)
  (format t "  CAUSE: ~A~%~%" cause)
  (format t "  FIX: ~A~%~%" fix)
  (format t "  ✓ WORKING EXAMPLE:~%")
  (format t "~{    ~A~%~}" (split-lines example)))
```

---

## Questions for Next Session

1. Should validation be **always-on** by default?
2. Should strict mode be **opt-out** instead of opt-in?
3. What iteration limit makes sense? (10K? 100K?)
4. Should trace mode pause on detected loops?
5. How verbose should error messages be?

---

**Next Session Goal:** Implement Priority 1-3 (Error Messages, Validation, Strict Mode)  
**Estimated Time:** 4-6 hours  
**Expected Impact:** 60% reduction in LLM debugging time
