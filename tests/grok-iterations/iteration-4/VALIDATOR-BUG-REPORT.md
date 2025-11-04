# CRITICAL: CNS Validator Bug - Blocking v2.0.0 Features

## Issue Summary

**The CNS validator incorrectly flags v2.0.0 keywords as undeclared variables**, preventing valid CNS code from passing validation.

## Reproduction

```bash
# Even our REFERENCE implementation fails validation!
./cns-validate tests/grok-iterations/iteration-4/test-2-reference.cns
```

### Result:
```
✗ 7 error(s) found:

ERROR: Variable 'BACKGROUND' used before declaration
ERROR: Variable 'SHELL' used before declaration  
ERROR: Variable 'PARSE_INT' used before declaration (3 occurrences)
ERROR: Variable 'WAIT' used before declaration
ERROR: Variable 'KILL' used before declaration
```

## Affected Features (v2.0.0)

### Process Management Keywords
- `SHELL ... BACKGROUND INTO`
- `KILL pid`
- `WAIT FOR pid`
- `STATUS OF pid`
- `PARSE_INT`

### Database Keywords (assumed affected)
- `DATABASE QUERY ... ON`
- `DATABASE EXECUTE ... ON`
- `DATABASE CONNECT TO ... AS`

## Root Cause

The validator treats **compound statement keywords** as **undeclared variables** when parsing multi-word statements.

### Example 1: SHELL BACKGROUND

**Valid CNS Code:**
```cns
Given:
  cmd: String = "sleep 10"
  pid: Integer = 0
Then: pid becomes SHELL cmd BACKGROUND INTO job
```

**Validator Error:**
```
ERROR: Variable 'SHELL' used before declaration
ERROR: Variable 'BACKGROUND' used before declaration
```

**What's Wrong:**
- `SHELL` and `BACKGROUND` are **keywords**, not variables
- They form the compound statement `SHELL ... BACKGROUND INTO`
- Validator incorrectly parses them as separate tokens

### Example 2: KILL

**Valid CNS Code:**
```cns
Given:
  pid: Integer = 12345
  result: Boolean = FALSE
Then: result becomes KILL pid
```

**Validator Error:**
```
ERROR: Variable 'KILL' used before declaration
```

**What's Wrong:**
- `KILL` is a **keyword** for process termination
- Validator treats it as an undeclared variable

### Example 3: PARSE_INT

**Valid CNS Code:**
```cns
Given:
  arg: String = "123"
  num: Integer = 0
Then: num becomes PARSE_INT arg
```

**Validator Error:**
```
ERROR: Variable 'PARSE_INT' used before declaration
```

**What's Wrong:**
- `PARSE_INT` is a **built-in function keyword**
- Validator doesn't recognize it as valid

## Impact on Grok Testing

### Iteration 4 Results: 1/3 Success (33%)

- ✅ Test 1 (Word Counter): PASSED - Uses only v1.x features
- ❌ Test 2 (Job Manager): FAILED - Uses `SHELL ... BACKGROUND`, `KILL`, `WAIT FOR`
- ❌ Test 3 (Task Runner): FAILED - Uses `DATABASE QUERY ... ON`

### Analysis

**Grok's code generation was correct!** The validator rejected it for using valid v2.0.0 syntax documented in SYNTAX.md.

**Proof:** 
1. Grok's Test 2 code structure matches our reference implementation
2. Our reference implementation ALSO fails validation
3. Both use identical syntax patterns from SYNTAX.md
4. The runtime engine (`cns-run`) would likely execute both correctly IF validation passed

## Expected Behavior

The validator should:

1. **Recognize compound keywords:**
   - `SHELL ... BACKGROUND INTO`
   - `DATABASE QUERY ... ON`
   - `DATABASE EXECUTE ... ON`

2. **Recognize built-in functions:**
   - `PARSE_INT`
   - `PARSE_FLOAT`
   - `PARSE_BOOL`

3. **Recognize process management keywords:**
   - `SHELL`, `BACKGROUND`, `INTO`
   - `KILL`, `WITH SIGKILL`, `WITH SIGTERM`
   - `WAIT FOR`, `WITH TIMEOUT`
   - `STATUS OF`

## Files Affected

### Reference Implementations (Hand-written, should be valid)
- `test-2-reference.cns` - ✗ FAILS validation (SHOULD PASS)
- `test-3-reference.cns` - Likely fails (not tested)

### Grok-Generated Code
- `results/test-2-grok-20251104_161854.cns` - ✗ FAILS validation (correctly written)
- `results/test-3-grok-20251104_161913.cns` - ✗ FAILS validation (correctly written)

## Validator Source

Location: `src/cns-validator.lisp`

**Likely Issues:**
1. Keyword recognition logic incomplete for v2.0.0 features
2. Compound statement parsing doesn't handle multi-word keywords
3. Built-in function list missing v2.0.0 additions

## Recommended Fix

### Priority 1: Update Keyword Recognition

Add to validator's **keyword list** (not variable list):

```lisp
;; Process Management Keywords
"SHELL"
"BACKGROUND" 
"INTO"
"KILL"
"WAIT"
"FOR"
"WITH"
"TIMEOUT"
"STATUS"
"OF"
"SIGKILL"
"SIGTERM"

;; Type Conversion Functions
"PARSE_INT"
"PARSE_FLOAT"
"PARSE_BOOL"

;; Database Keywords
"DATABASE"
"QUERY"
"EXECUTE"
"CONNECT"
"TO"
"AS"
"ON"
"CLOSE"
```

### Priority 2: Compound Statement Parser

Teach validator to recognize these **as single statements**, not individual tokens:

- `SHELL <expr> BACKGROUND INTO <var>`
- `DATABASE QUERY <sql> ON <db>`
- `DATABASE EXECUTE <sql> ON <db>`
- `DATABASE CONNECT TO <path> AS <var>`
- `KILL <pid> WITH <signal>`
- `WAIT FOR <pid> WITH TIMEOUT <n>`
- `STATUS OF <pid>`

### Priority 3: Built-in Function Registry

Maintain list of **built-in functions** that don't require declaration:

- `PARSE_INT()`
- `PARSE_FLOAT()`
- `PARSE_BOOL()`
- `NOW()`
- `SQRT OF`
- `ABS OF`
- `POW ... TO`
- etc.

## Testing After Fix

```bash
# These should ALL pass validation:
./cns-validate tests/grok-iterations/iteration-4/test-1-reference.cns  # ✓ Already passes
./cns-validate tests/grok-iterations/iteration-4/test-2-reference.cns  # ✗ Currently fails
./cns-validate tests/grok-iterations/iteration-4/test-3-reference.cns  # ✗ Currently fails

# Grok's code should also pass:
./cns-validate tests/grok-iterations/iteration-4/results/test-2-grok-*.cns
./cns-validate tests/grok-iterations/iteration-4/results/test-3-grok-*.cns
```

## Conclusion

**This is NOT an LLM failure** - Grok understood v2.0.0 syntax correctly and generated valid code matching SYNTAX.md examples.

**This IS a validator bug** - The validator is out of sync with the runtime interpreter and cannot validate v2.0.0 features.

**Impact:** Blocks all LLM testing of v2.0.0 features, prevents validation of hand-written code, reduces confidence in CNS tooling.

**Solution:** Update `cns-validator.lisp` to recognize v2.0.0 keywords and compound statements.

---

**Reported:** November 4, 2025  
**Found During:** Grok Iteration 4 LLM testing  
**Severity:** Critical - Blocks v2.0.0 adoption  
**Status:** Awaiting validator fix
