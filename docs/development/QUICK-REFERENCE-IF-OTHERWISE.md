# If/Otherwise Quick Reference

## Status: ✅ WORKING (Story Interpreter)

## Basic Syntax

### Simple Condition
```cns
Step 1 → Check value
  If: x > 10
    Then: result becomes "high"
  Otherwise:
    Then: result becomes "low"
```

### With Effects
```cns
Step 1 → Check user
  If: user IS "admin"
    Effect: Print "Admin logged in"
  Otherwise:
    Effect: Print "Regular user logged in"
```

### Control Flow
```cns
Step 1 → Route based on value
  If: x > 100
    Then: go to Step 5
  Otherwise:
    Then: go to Step 2
```

## What Works ✅

- ✅ Only one branch executes (If OR Otherwise, never both)
- ✅ Then clauses execute in correct branch
- ✅ Effects execute in correct branch
- ✅ Control flow (go to Step, repeat from Step)
- ✅ Variable assignments
- ✅ Multiple Then clauses in same branch
- ✅ Multiple Effects in same branch

## What Doesn't Work ❌

- ❌ Nested If/Otherwise (use waterfall pattern instead)
- ❌ If/Otherwise in functions (fix pending)
- ❌ ELSIF/ELSE IF (not implemented)

## Waterfall Pattern (Multiple Conditions)

**Instead of nesting:**
```cns
# DON'T DO THIS - NOT SUPPORTED
Step 1 → Check
  If: lang IS "rust"
    If: has_tests IS true  # ❌ Nested!
      Then: cmd becomes "cargo test"
```

**Use sequential steps:**
```cns
# DO THIS - Waterfall Pattern ✅
Step 1 → Check if Rust with tests
  If: lang IS "rust" AND has_tests IS true
    Then: cmd becomes "cargo test"
    Then: go to Step 10
    
Step 2 → Check if Rust without tests
  If: lang IS "rust"
    Then: cmd becomes "cargo build"
    Then: go to Step 10
    
Step 3 → Check if Go
  If: lang IS "go"
    Then: cmd becomes "go test"
    Then: go to Step 10
```

## Common Patterns

### Boolean Flag
```cns
Step 1 → Initialize
  If: condition
    Then: flag becomes true
  Otherwise:
    Then: flag becomes false
```

### Error Checking
```cns
Step 1 → Validate
  If: input IS empty
    Effect: Print "Error: input required"
    Then: go to End
  Otherwise:
    Then: continue
```

### Value Selection
```cns
Step 1 → Choose value
  If: mode IS "production"
    Then: url becomes "https://api.prod.com"
  Otherwise:
    Then: url becomes "https://api.dev.com"
```

## Debugging Tips

### Check Which Branch Executed
```cns
Step 1 → Debug conditional
  If: x > 10
    Effect: Print "IF branch executed"
    Then: result becomes "A"
  Otherwise:
    Effect: Print "OTHERWISE branch executed"
    Then: result becomes "B"
```

### Verify Condition Value
```cns
Step 1 → Show condition
  Effect: Print "x = {x}"
  If: x > 10
    Then: result becomes "high"
  Otherwise:
    Then: result becomes "low"
```

## Files to Check

- **Implementation:** `src/cns.lisp` lines 3553-3718
- **Tests:** `test-otherwise-simple.cns`, `test-otherwise-effects.cns`
- **Full Docs:** `IF-OTHERWISE-FIX-COMPLETE.md`
- **Debugging:** `docs/development/LISP-DEBUGGING-GUIDE.md`

## Known Issues

1. **Function Interpreter:** Not yet fixed (use If/Otherwise only in main stories)
2. **Nested If/Otherwise:** Not supported (use waterfall pattern)
3. **No error message:** For nested attempts (enhancement pending)

## Quick Test

```bash
# Create test file
cat > test-if.cns << 'EOF'
Story: Test If/Otherwise

Given:
  x: String = "hello"
  result: String = ""

Step 1 → Test
  If: x = "goodbye"
    Then: result becomes "A"
  Otherwise:
    Then: result becomes "B"

Step 2 → Show
  Effect: Print "Result: {result}"
  
End: 0
EOF

# Run test
./cns-run test-if.cns

# Expected output: "Result: B"
```

## Version Info

- **Implemented:** v1.7.0 (2025-11-02)
- **Status:** Story interpreter complete ✅
- **Next:** Function interpreter fix

---

*For detailed implementation, see IF-OTHERWISE-FIX-COMPLETE.md*  
*For debugging help, see docs/development/LISP-DEBUGGING-GUIDE.md*
