# CNS Quick Syntax Reference

## Variable Declaration
```
variable_name: Type = value
```

**Types:** `Integer`, `String`, `Boolean`, `List`, `Socket`

**Examples:**
```
n: Integer = 5
name: String = "Alice"
is_valid: Boolean = TRUE
items: List = []
```

## Boolean Literals
- ✅ `TRUE` and `FALSE` (uppercase)
- ❌ NOT: `True`, `true`, `False`, `false`

## Operators

**Comparison:**
- `=` (equals) — NOT `==`
- `>`, `<`
- `>=` or `≥` (greater than or equal)
- `<=` or `≤` (less than or equal)

**Arithmetic:**
- `+`, `-`, `*`, `/`, `%`

**Examples:**
```
If x = 0           (✅ correct)
If x == 0          (❌ wrong)
If count > 10      (✅ correct)
```

## Step Structure

```
Step N → [Action description]
  Because: [Explanation]
  Effect: [Side effect, if any]
  Then: [State change]
  Then: [Control flow]
```

**Key Points:**
- Each step must have `Because:`
- Multiple `Then:` lines allowed
- Each `Then:` should be ONE action

## Conditionals

```
Step N → If [condition]
  Because: [Explanation]
  Then: [action if true]
  Otherwise: [action if false]
```

**Examples:**
```
Step 1 → If x = 0
  Because: Check for zero
  Then: result becomes 1
  Otherwise: go to Step 2

Step 2 → If count > limit
  Because: Check bounds
  Then: go to End
  Otherwise: go to Step 3
```

## Control Flow

**Jump to End:**
```
Then: go to End
```

**Loop Back:**
```
Then: repeat from Step N
```

**Continue to Next:**
```
Otherwise: go to Step N
```

## State Changes

```
Then: variable becomes new_value
```

**Examples:**
```
Then: count becomes count + 1
Then: result becomes result * n
Then: is_valid becomes FALSE
```

## Effects (Side Effects)

Declare ALL I/O operations:

```
Effect: Print "message"
Effect: Read filename into variable
Effect: Write "data" to filename
Effect: Create socket socket_name on port
Effect: Accept connection on socket_name
Effect: Send "response" to client
Effect: Network read
Effect: Log "message"
```

## End Block

**Format:** Single line

```
End: Return result
```

❌ **WRONG:**
```
End:
  Return result
  Because: explanation
```

## Error Block

Use ONLY for exception handling (I/O failures, network errors):

```
Error:
  Return error_value
  Effect: Log "error message"
  Because: Handle unexpected failure
```

❌ **DON'T** use for input validation or logical branching
✅ **DO** use regular `If/Then/Otherwise` for validation

## Common Patterns

### Counter Loop
```
Given:
  i: Integer = 0
  limit: Integer = 10

Step 1 → If i >= limit
  Because: Reached the limit
  Then: go to End
  Otherwise: go to Step 2

Step 2 → Process current iteration
  Because: Do work for this iteration
  Then: i becomes i + 1
  Then: repeat from Step 1
```

### Flag-Based Early Exit
```
Given:
  is_valid: Boolean = TRUE

Step 1 → If some_condition
  Because: Invalidation check
  Then: is_valid becomes FALSE
  Otherwise: go to Step 2

Step 2 → If is_valid = FALSE
  Because: Early exit when invalid
  Then: go to End
  Otherwise: continue processing
```

### Divisibility Check
```
Step N → If num % divisor = 0
  Because: Check if divisor divides evenly
  Then: [action for divisible]
  Otherwise: [action for not divisible]
```

## Full Example: Factorial

```cns
Story: Compute factorial of a positive integer

Given:
  n: Integer = 5
  result: Integer = 1

Step 1 → Multiply result by n
  Because: n contributes to the factorial product
  Then: n becomes n - 1

Step 2 → If n > 1
  Because: We need to multiply all integers down to 1
  Then: repeat from Step 1
  Otherwise: go to End

End: Return result
```

## Syntax Checklist

Before submitting CNS code, verify:

- [ ] No semantic tags like `[≥ 1]` in variable declarations
- [ ] Boolean values are `TRUE` or `FALSE` (uppercase)
- [ ] Comparison uses `=` not `==`
- [ ] Each `Then:` has one action
- [ ] Every step has `Because:`
- [ ] All I/O has `Effect:` declarations
- [ ] `End:` is single-line format
- [ ] `Error:` used only for exceptions, not logic
- [ ] All variables declared in `Given:`
- [ ] Sequential step numbering (1, 2, 3...)
