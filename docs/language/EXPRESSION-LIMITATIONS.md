# CNS Expression Limitations (For LLM Code Generators)

**Purpose:** This document explains which expressions work in CNS and which don't.  
**Audience:** LLMs generating CNS code, developers learning CNS  
**Last Updated:** 2025-11-02

---

## Quick Reference

✅ **ALWAYS SAFE:**
- Variable-first: `x + 5`, `result * 3`
- Simple operations: `a + b`, `x - y`
- Single operator: `count / 2`

❌ **NEVER WORKS:**
- Literal-first: `3 * n` → NIL
- Multi-operator with literals: `n * 3 + 1` → NIL
- Complex nested: `(a + b) * (c - d)` → NIL

⚠️ **USE WITH CARE:**
- Multi-variable: `a + b + c` (left-to-right evaluation)
- Chained operations: `x * y + z` (works if variable-first)

---

## ✓ SUPPORTED Expressions

### Simple Binary Operations
```cns
Then: result becomes a + b        ✓
Then: result becomes a - b        ✓
Then: result becomes a * b        ✓
Then: result becomes a / b        ✓
Then: result becomes a % b        ✓
```

### Comparisons
```cns
If: a = b           ✓
If: a > b           ✓
If: a < b           ✓
If: count = 0       ✓
If: x > 10          ✓
```

### Variable-First Complex Expressions
```cns
Then: result becomes a + b + c           ✓ (evaluates left-to-right)
Then: result becomes x * y + z           ✓ (if x and y exist)
Then: total becomes count + offset       ✓
Then: value becomes n / 2                ✓
```

### String Operations
```cns
Then: name becomes "John"                ✓
Then: greeting becomes "Hello " + name   ✓
Then: upper becomes UPPERCASE text       ✓
Then: lower becomes LOWERCASE text       ✓
Then: clean becomes TRIM input           ✓
Then: replaced becomes REPLACE "old" WITH "new" IN text  ✓
```

### List Operations
```cns
Then: items becomes [1, 2, 3]            ✓
Then: first becomes FIRST_OF items       ✓
Then: len becomes LENGTH_OF items        ✓
Then: joined becomes JOIN items WITH ", " ✓
```

---

## ✗ UNSUPPORTED Patterns

### Literal-First Expressions
```cns
❌ WRONG:
Then: result becomes 3 * n       # → NIL
Then: result becomes 5 + a       # → NIL
Then: value becomes 2 * x + 1    # → NIL

✓ RIGHT:
Then: result becomes n * 3
Then: result becomes a + 5
Then: temp becomes x * 2
Then: value becomes temp + 1
```

**Why it fails:** CNS expression parser expects variables first.

### Multi-Operator with Literals
```cns
❌ WRONG:
Then: result becomes n * 3 + 1          # → NIL
Then: value becomes count + 5 * 2       # → NIL
Then: answer becomes x / 2 - 10         # → NIL

✓ RIGHT - Split into steps:
Then: temp becomes n * 3
Then: result becomes temp + 1

# OR reorder:
Then: result becomes n * 3              # works
Then: result becomes result + 1         # then add
```

### Parentheses (Not Supported)
```cns
❌ WRONG:
Then: result becomes (a + b) * c        # → NIL
Then: value becomes n * (x + y)         # → NIL

✓ RIGHT:
Then: sum becomes a + b
Then: result becomes sum * c
```

### Complex Nested Expressions
```cns
❌ WRONG:
Then: result becomes a + b * c - d      # → NIL or wrong result
Then: value becomes (x + y) / (z - w)   # → NIL

✓ RIGHT:
Then: product becomes b * c
Then: sum becomes a + product
Then: result becomes sum - d
```

---

## ✓ WORKAROUNDS

### Pattern 1: Reorder to Variable-First
```cns
# WRONG:
Then: result becomes 3 * n + 1          ✗

# RIGHT:
Then: result becomes n * 3 + 1          ✓

# BEST (if still fails):
Then: temp becomes n * 3
Then: result becomes temp + 1           ✓
```

### Pattern 2: Split Into Multiple Steps
```cns
# WRONG:
Then: answer becomes n * 3 + 1          ✗

# RIGHT:
Then: temp becomes n * 3
Then: answer becomes temp + 1           ✓
```

### Pattern 3: Use Built-in Functions
```cns
# WRONG:
Then: result becomes (len + 5) * 2      ✗

# RIGHT - Use LENGTH_OF:
Then: len becomes LENGTH_OF items
Then: sum becomes len + 5
Then: result becomes sum * 2            ✓
```

### Pattern 4: Leverage Assignment
```cns
# WRONG:
If: (count + 1) > 10                    ✗
  Then: go to End

# RIGHT:
Then: next_count becomes count + 1
If: next_count > 10
  Then: go to End                       ✓
```

---

## LLM Code Generation Rules

When generating CNS code, follow these rules:

### Rule 1: Always Variable-First
```cns
✓ DO:    Then: result becomes x + 5
✗ DON'T: Then: result becomes 5 + x
```

### Rule 2: One Operator Per Expression (When in Doubt)
```cns
✓ DO:    Then: a becomes x + y
         Then: b becomes a + z
         
✗ DON'T: Then: b becomes x + y + z     (works but risky)
```

### Rule 3: No Literals in Multi-Operator Expressions
```cns
✓ DO:    Then: temp becomes n * 3
         Then: result becomes temp + 1
         
✗ DON'T: Then: result becomes n * 3 + 1
```

### Rule 4: Use Temporary Variables Liberally
```cns
✓ DO:    Then: step1 becomes a * b
         Then: step2 becomes c + d
         Then: result becomes step1 + step2
         
✗ DON'T: Then: result becomes a * b + c + d
```

### Rule 5: Test Complex Expressions
If unsure whether an expression will work, split it into steps.

---

## Testing Your Expression

Use this template to test if an expression works:

```cns
Story: Expression Test [strict]

Given:
  a: Integer = 5
  b: Integer = 3
  result: Integer = 0

Step 1 → Test expression
  Then: result becomes YOUR_EXPRESSION_HERE
  Effect: Print "Result: {result}"

End: Return result
```

**If `result` is NIL:** The expression is unsupported. Split it into multiple steps.

**Example test:**
```cns
Story: Test 3 * n

Given:
  n: Integer = 10
  result: Integer = 0

Step 1 → Test
  Then: result becomes 3 * n       # This will fail → NIL
  Effect: Print "Result: {result}"

End: Return result
```

**Output:** `Result: NIL` → Expression unsupported ❌

**Fixed version:**
```cns
Step 1 → Test
  Then: result becomes n * 3       # This works!
  Effect: Print "Result: {result}"
```

**Output:** `Result: 30` → Expression works ✅

---

## Common Pitfalls

### Pitfall 1: Forgetting Variable-First Rule
```cns
# Natural (but WRONG):
Then: area becomes 2 * width * height   ✗

# CNS way (RIGHT):
Then: temp becomes width * height
Then: area becomes temp * 2             ✓

# Or even better:
Then: area becomes width * height
Then: area becomes area * 2             ✓
```

### Pitfall 2: Trying to Use Parentheses
```cns
# Tempting (but WRONG):
Then: avg becomes (a + b) / 2           ✗

# CNS way (RIGHT):
Then: sum becomes a + b
Then: avg becomes sum / 2               ✓
```

### Pitfall 3: Complex Conditions
```cns
# Natural (but WRONG):
If: count + 1 > max                     ✗ (might work, but risky)
  Then: go to End

# CNS way (RIGHT):
Then: next becomes count + 1
If: next > max
  Then: go to End                       ✓
```

### Pitfall 4: Math in String Interpolation
```cns
# Tempting (but WRONG):
Effect: Print "Total: {count + 1}"      ✗ (won't evaluate)

# CNS way (RIGHT):
Then: total becomes count + 1
Effect: Print "Total: {total}"          ✓
```

---

## Expression Evaluation Order

CNS evaluates expressions **left-to-right** with **no operator precedence**:

```cns
# Math: 2 + 3 * 4 = 14 (multiplication first)
# CNS: 2 + 3 * 4 = 20 (left-to-right: (2+3)*4)

Then: result becomes a + b * c
# Evaluates as: (a + b) * c  ← NOT what you expect!

# To get math-like precedence, use separate steps:
Then: product becomes b * c
Then: result becomes a + product       # Now: a + (b*c)
```

**Key takeaway:** When order matters, split into multiple steps.

---

## Quick Diagnostic Guide

**Problem:** Expression evaluates to NIL

**Checklist:**
1. ☐ Is it literal-first? (e.g., `3 * n`) → Reorder to `n * 3`
2. ☐ Multiple operators? (e.g., `a + b * c`) → Split into steps
3. ☐ Has parentheses? (e.g., `(a + b) * c`) → Remove, use temp variables
4. ☐ All variables defined? → Check Given section or previous steps
5. ☐ Still NIL? → Enable strict mode to see where it fails

**Problem:** Expression gives wrong result

**Likely cause:** Left-to-right evaluation differs from math precedence
**Solution:** Split expression to control order

---

## Advanced: Why These Limitations Exist

CNS uses a simple expression evaluator that:
1. Parses left-to-right (no operator precedence)
2. Expects variables before literals in most contexts
3. Doesn't support parentheses for grouping
4. Evaluates eagerly without lookahead

This design keeps CNS simple and predictable, but requires:
- Explicit step-by-step calculation
- Temporary variables for complex math
- Careful ordering of operations

**Philosophy:** Write code like you would explain it to someone step-by-step.

---

## Summary: Golden Rules

1. **Variable-first:** `x + 5` not `5 + x`
2. **One operator at a time:** Split `a + b * c` into two steps
3. **No parentheses:** Use temp variables instead
4. **Test with strict mode:** Catch NIL early
5. **When in doubt, split it out:** More steps = fewer bugs

---

## Examples: Before & After

### Example 1: Quadratic Calculation
```cns
❌ WRONG:
Then: result becomes a * x * x + b * x + c

✓ RIGHT:
Then: x_squared becomes x * x
Then: term1 becomes a * x_squared
Then: term2 becomes b * x
Then: sum becomes term1 + term2
Then: result becomes sum + c
```

### Example 2: Average Calculation
```cns
❌ WRONG:
Then: avg becomes (a + b + c) / 3

✓ RIGHT:
Then: sum becomes a + b
Then: sum becomes sum + c
Then: avg becomes sum / 3
```

### Example 3: Distance Formula (√((x2-x1)² + (y2-y1)²))
```cns
❌ WRONG (completely unsupported):
Then: distance becomes SQRT((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))

✓ RIGHT:
Then: dx becomes x2 - x1
Then: dy becomes y2 - y1
Then: dx_sq becomes dx * dx
Then: dy_sq becomes dy * dy
Then: sum_sq becomes dx_sq + dy_sq
Then: distance becomes SQRT sum_sq
```

---

## For LLMs: Generation Template

When generating CNS arithmetic, use this pattern:

```cns
Story: [Your story name]

Given:
  [Declare all variables with types and initial values]

Step 1 → [Descriptive name]
  Because: [Why this calculation is needed]
  # Break complex expressions into simple steps:
  Then: temp1 becomes [simple expression]
  Then: temp2 becomes [simple expression]
  Then: result becomes [simple expression]
  Effect: Print "[Describe what was calculated]: {result}"

End: Return result
```

**Key:** Every `Then:` should have at most ONE operator and be variable-first.

