# CNS Validator Issues & Improvements

**Purpose**: Document known validator bugs and planned improvements

**Status**: Active development tracking

**Last Updated**: 2025-11-03

---

## Current Validator Issues

### High Priority (Blocking LLM Testing)

#### 1. False Positive: "Variable used before declaration" in Because Clauses

**Issue**:
```
ERROR: Variable 'Move' used before declaration in Step 4
  Context: Move to the next potential factor
```

**Root Cause**: Validator treats `Because:` clause text as code and tries to parse it for variable references

**Impact**: 
- Every program with descriptive Because clauses gets errors
- LLMs get confused by validation output
- Makes validator output untrustworthy

**Fix Required**:
```lisp
;; In validate-step function
(when (string-equal "BECAUSE:" (subseq trimmed 0 8))
  ;; Skip validation of Because clause - it's documentation
  (return-from validate-step))
```

**Priority**: 🔥🔥🔥 Critical

---

#### 2. Incomplete Effect Pattern Recognition

**Issue**:
```
WARNING: Unrecognized effect pattern: Network read
WARNING: Unrecognized effect pattern: Append "..." to file
```

**Root Cause**: Validator has hardcoded effect pattern list that doesn't match actual interpreter capabilities

**Impact**:
- Valid effects show as warnings
- Can't trust validator output
- LLMs might avoid correct patterns

**Fix Required**:
Build effect pattern library from actual interpreter:
```lisp
(defparameter *valid-effect-patterns*
  '("Print"
    "HTTP GET from"
    "HTTP POST to"
    "Create socket"
    "Accept connection"
    "Network read"
    "Send .* to client"
    "Close connection"
    "Write .* to .*"
    "Append .* to .*"
    ;; ... extracted from interpreter
    ))
```

**Priority**: 🔥🔥🔥 Critical

---

#### 3. No Documentation-Implementation Validation

**Issue**: Validator doesn't check if documented features are actually implemented

**Example**:
- Template says `<=` is supported
- Interpreter only had `≤` support
- Grok's correct code failed

**Impact**:
- LLMs generate "correct" code that doesn't work
- Documentation lies by omission
- Wastes debugging time

**Fix Required**:
Add `--strict` mode that validates against actual interpreter:
```lisp
(defun validate-operator (op)
  (unless (member op *implemented-operators*)
    (warn "Operator ~A is documented but not implemented" op)))

(defparameter *implemented-operators*
  ;; Extracted from actual interpreter eval-expr
  '("=" ">" "<" ">=" "<=" "+" "-" "*" "/" "%" "AND" "OR" "NOT"))
```

**Priority**: 🔥🔥 High

---

### Medium Priority (Quality of Life)

#### 4. Poor Error Messages

**Current**:
```
ERROR: Variable 'go to End' is not defined
```

**Better**:
```
ERROR: Control flow 'go to End' must be inside If/Otherwise block
LOCATION: Step 5, line 23
FIX: Wrap in conditional:
  If: condition
    Then: go to End
```

**Fix Required**:
- Context-aware error messages
- Suggested fixes for common errors
- Line number reporting
- Example corrections

**Priority**: 🔥🔥 High (helps LLMs correct themselves)

---

#### 5. No Auto-Fix Capability

**Desired**:
```bash
./cns-validate --fix-common program.cns

Found 3 fixable issues:
1. Line 5: True → TRUE [FIXED]
2. Line 12: == → = [FIXED]  
3. Line 18: >= → ≥ [FIXED]

Wrote corrected version to program.cns
```

**Fix Required**:
- Pattern matching for common mistakes
- Safe text replacement
- Backup original file
- Report changes made

**Priority**: 🔥 Medium (nice to have)

---

### Low Priority (Future Enhancements)

#### 6. Step Number Gap Detection

**Current**: Warns about gaps but doesn't understand `go to`

**Better**: Track control flow and understand when gaps are intentional

**Priority**: Low

---

#### 7. Dead Code Detection

**Example**:
```cns
Step 5 → Process
  Then: go to End

Step 6 → This never runs!
  Then: x becomes 5
```

**Fix**: Warn about unreachable steps

**Priority**: Low

---

## Validator Architecture Issues

### Current Design Problems

1. **Mixed Concerns**: Validator does both syntax checking and semantic analysis
2. **No AST**: Parses text directly without building intermediate representation
3. **Incomplete**: Doesn't validate all features (effects, functions, etc.)
4. **Regex-Heavy**: Uses regex for parsing instead of proper parser
5. **No Test Suite**: Validator itself has no automated tests

### Proposed Improvements

#### Phase 1: Fix Critical Issues (1-2 hours)
- [ ] Skip Because clause validation
- [ ] Build effect pattern library from interpreter
- [ ] Add `--strict` mode for doc-vs-impl checking

#### Phase 2: Better Error Messages (2-3 hours)
- [ ] Add error context (line numbers, step names)
- [ ] Create common error → fix mapping
- [ ] Include example corrections in messages
- [ ] Add "Did you mean X?" suggestions

#### Phase 3: Auto-Fix (3-4 hours)
- [ ] Pattern matching for common mistakes
- [ ] Safe text replacement engine
- [ ] `--fix-common` flag implementation
- [ ] Report of changes made

#### Phase 4: Structural Improvements (Future)
- [ ] Build proper AST
- [ ] Separate syntax from semantic validation
- [ ] Add control flow analysis
- [ ] Dead code detection
- [ ] Create validator test suite

---

## Testing Plan

### Validator Test Cases Needed

1. **Positive Tests** (should pass):
   - All examples in `examples/` directory
   - All Grok-generated code (corrected versions)
   - All LLM test cases

2. **Negative Tests** (should fail with good errors):
   - Undefined variables
   - Missing step numbers
   - Invalid operators
   - Type mismatches
   - Syntax errors

3. **Edge Cases**:
   - Empty stories
   - Very long variable names
   - Unicode in strings
   - Deeply nested conditionals

### Test Automation

```bash
# Run validator test suite
./tests/test-validator.sh

# Expected output:
# ✓ 45/45 positive tests pass
# ✓ 23/23 negative tests fail correctly
# ✓ 12/12 edge cases handled
```

---

## Immediate Action Items

### Week 1: Critical Fixes
1. ✅ Document issues (this file)
2. ⏳ Skip Because clause validation
3. ⏳ Build effect pattern library
4. ⏳ Add `--strict` mode flag

### Week 2: Error Messages
1. Add line number tracking
2. Create error → fix mapping
3. Implement suggestion system
4. Test with LLMs

### Week 3: Auto-Fix
1. Implement pattern matcher
2. Create safe replacer
3. Add `--fix-common` flag
4. Document supported fixes

---

## Success Metrics

**Current State**:
- ❌ False positives on every file
- ❌ Incomplete effect validation
- ❌ Poor error messages
- ❌ No auto-fix capability

**Target State (Phase 1)**:
- ✅ Zero false positives
- ✅ Complete effect validation
- ✅ All documented features validated
- ✅ `--strict` mode available

**Target State (Phase 2)**:
- ✅ Context-aware error messages
- ✅ Suggested fixes for common errors
- ✅ Line numbers in errors
- ✅ Example corrections

**Target State (Phase 3)**:
- ✅ Auto-fix for 80% of common mistakes
- ✅ Safe replacement engine
- ✅ Change reporting
- ✅ Backup mechanism

---

## Related Documents

- **LLM-COMMON-MISTAKES.md** - Errors LLMs make (validator should catch these)
- **TESTING.md** - Test framework documentation
- **ROADMAP.md** - Overall project roadmap
- **LLM-INTEGRATION.md** - How LLMs should use CNS

---

## Contributing

To fix validator issues:

1. Check this document for assigned issues
2. Create fix in `src/cns-validator.lisp`
3. Test with examples and LLM-generated code
4. Update this document with fix status
5. Add test case to validator test suite

---

**Remember**: The validator is the first thing LLMs encounter. Making it trustworthy and helpful is critical for LLM success rates!
