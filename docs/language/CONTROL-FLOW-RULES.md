# CNS Control Flow Rules (For LLM Code Generators)

**Purpose:** This document explains how control flow works in CNS and common mistakes to avoid.  
**Audience:** LLMs generating CNS code, developers learning CNS  
**Last Updated:** 2025-11-02

---

## Quick Reference

✅ **ALWAYS VALID:**
- Control flow inside `If:` branches
- Control flow inside `Otherwise:` branches
- Sequential step execution (no explicit flow)

❌ **NEVER VALID:**
- Control flow in regular `Then:` clauses
- Control flow in `Effect:` clauses
- Control flow before branching logic

---

## Rule 1: Control Flow ONLY in If/Otherwise

Control flow statements can **only** appear inside `If:` or `Otherwise:` branches.

### Control Flow Statements
- `repeat from Step N` - Jump back to Step N (loop)
- `go to Step N` - Jump forward to Step N (skip ahead)
- `go to End` - Exit program immediately

### ✓ VALID Usage

```cns
Step 1 → Loop body
  If: x > 0
    Then: x becomes x - 1
    Then: repeat from Step 1    ✓ Inside If branch
  Otherwise:
    Then: go to End              ✓ Inside Otherwise branch
```

### ✗ INVALID Usage

```cns
Step 1 → Attempted loop
  Then: x becomes x - 1
  Then: repeat from Step 1       ✗ WILL BE IGNORED!
```

**Why it's invalid:** Control flow outside branches is silently ignored. CNS will just continue to the next step.

---

## Rule 2: Control Flow Must Be Last Then

When using control flow, it **must** be the last `Then:` in the branch.

### ✓ VALID: Control flow is last

```cns
If: x > 0
  Then: x becomes x - 1
  Then: y becomes y + 1
  Then: repeat from Step 1       ✓ Last Then
```

### ✗ INVALID: Then after control flow

```cns
If: x > 0
  Then: repeat from Step 1
  Then: x becomes x - 1          ✗ NEVER EXECUTES!
```

**Why it's invalid:** Once control flow happens, execution jumps. Anything after is unreachable.

---

## Rule 3: Control Flow Types

### `repeat from Step N` - Loop Back

**Use for:** Loops, iteration, retry logic

**Example: Countdown**
```cns
Story: Countdown from N

Given:
  n: Integer = 10

Step 1 → Check if done
  If: n = 0
    Then: go to End
    
Step 2 → Decrement and loop
  Then: n becomes n - 1
  Effect: Print "Count: {n}"
  If: n > 0
    Then: repeat from Step 1     ✓ Loop back

End: Return n
```

**Example: Sum range**
```cns
Story: Sum 1 to N

Given:
  n: Integer = 10
  sum: Integer = 0
  current: Integer = 1

Step 1 → Add current to sum
  Then: sum becomes sum + current
  Then: current becomes current + 1
  If: current < n
    Then: repeat from Step 1     ✓ Continue summing

End: Return sum
```

---

### `go to Step N` - Jump Forward

**Use for:** Skipping steps conditionally, early optimization

**Example: Skip processing**
```cns
Story: Conditional Processing

Given:
  skip_mode: Boolean = true
  result: Integer = 0

Step 1 → Check skip mode
  If: skip_mode
    Then: go to Step 3           ✓ Skip to end
    
Step 2 → Do expensive work
  Then: result becomes 100
  Effect: Print "Processing..."
  
Step 3 → Finish
  Effect: Print "Done: {result}"

End: Return result
```

**Example: Early exit on error**
```cns
Story: Validate Input

Given:
  value: Integer = -5
  valid: Boolean = false

Step 1 → Validate
  If: value < 0
    Then: go to Step 3           ✓ Skip to error handling
  Otherwise:
    Then: valid becomes true
    
Step 2 → Process valid input
  Effect: Print "Processing {value}"
  
Step 3 → Handle result
  If: valid
    Then: go to End
  Otherwise:
    Effect: Print "Invalid input!"

End: Return valid
```

---

### `go to End` - Exit Immediately

**Use for:** Early termination, error conditions, success exit

**Example: Find first match**
```cns
Story: Find First Even Number

Given:
  numbers: List = [1, 3, 5, 8, 9]
  current_index: Integer = 0
  found: Integer = -1

Step 1 → Check all numbers
  If: current_index = LENGTH_OF numbers
    Then: go to End              ✓ All checked, exit
    
Step 2 → Get current number
  Then: current becomes INDEX numbers AT current_index
  If: current % 2 = 0
    Then: found becomes current
    Then: go to End              ✓ Found it, exit early
    
Step 3 → Try next
  Then: current_index becomes current_index + 1
  If: current_index < LENGTH_OF numbers
    Then: repeat from Step 1

End: Return found
```

---

## LLM Code Generation Patterns

### Pattern 1: Simple Loop (Count Down)

```cns
Story: Loop Template - Count Down

Given:
  counter: Integer = 10

Step 1 → Loop condition
  If: counter = 0
    Then: go to End              ✓ Exit when done
    
Step 2 → Loop body
  Then: counter becomes counter - 1
  Effect: Print "Counter: {counter}"
  If: counter > 0
    Then: repeat from Step 1     ✓ Continue loop

End: Return counter
```

### Pattern 2: While-Style Loop

```cns
Story: Loop Template - While Style

Given:
  running: Boolean = true
  count: Integer = 0

Step 1 → Loop condition check
  If: running = false
    Then: go to End              ✓ Exit condition

Step 2 → Loop body
  Then: count becomes count + 1
  Effect: Print "Iteration {count}"
  
  # Update loop condition
  If: count > 5
    Then: running becomes false
    Then: repeat from Step 1     ✓ Check condition again

End: Return count
```

### Pattern 3: For-Each Style Loop (Manual)

```cns
Story: Loop Template - For Each

Given:
  items: List = ["a", "b", "c"]
  index: Integer = 0
  current: String = ""

Step 1 → Check if done
  If: index = LENGTH_OF items
    Then: go to End              ✓ All items processed
    
Step 2 → Process current item
  Then: current becomes INDEX items AT index
  Effect: Print "Item: {current}"
  
Step 3 → Advance to next
  Then: index becomes index + 1
  If: index < LENGTH_OF items
    Then: repeat from Step 1     ✓ Process next item

End: Return "Done"
```

### Pattern 4: Search with Early Exit

```cns
Story: Loop Template - Search

Given:
  target: Integer = 42
  items: List = [10, 20, 42, 50]
  index: Integer = 0
  found: Boolean = false

Step 1 → Check if done
  If: index = LENGTH_OF items
    Then: go to End              ✓ Not found, exit
    
Step 2 → Check current item
  Then: current becomes INDEX items AT index
  If: current = target
    Then: found becomes true
    Then: go to End              ✓ Found it, exit early
    
Step 3 → Try next
  Then: index becomes index + 1
  If: index < LENGTH_OF items
    Then: repeat from Step 1

End: Return found
```

### Pattern 5: Nested Loop Structure

```cns
Story: Nested Loops

Given:
  outer: Integer = 0
  inner: Integer = 0
  outer_max: Integer = 3
  inner_max: Integer = 2

Step 1 → Outer loop condition
  If: outer = outer_max
    Then: go to End              ✓ Outer loop done

Step 2 → Reset inner loop
  Then: inner becomes 0
  
Step 3 → Inner loop condition
  If: inner = inner_max
    Then: go to Step 5           ✓ Inner loop done, continue outer
    
Step 4 → Inner loop body
  Effect: Print "Outer: {outer}, Inner: {inner}"
  Then: inner becomes inner + 1
  If: inner < inner_max
    Then: repeat from Step 3     ✓ Continue inner loop
    
Step 5 → Advance outer loop
  Then: outer becomes outer + 1
  If: outer < outer_max
    Then: repeat from Step 1     ✓ Continue outer loop

End: Return "Done"
```

---

## Common Mistakes

### Mistake 1: Control Flow Outside Branches

```cns
❌ WRONG:
Step 1 → Loop
  Then: x becomes x - 1
  Then: repeat from Step 1       ✗ Silently ignored!

✓ RIGHT:
Step 1 → Loop
  Then: x becomes x - 1
  If: x > 0
    Then: repeat from Step 1     ✓ Inside If branch
```

### Mistake 2: Code After Control Flow

```cns
❌ WRONG:
If: count > 0
  Then: go to End
  Then: count becomes 0          ✗ Never runs!

✓ RIGHT:
If: count > 0
  Then: count becomes 0
  Then: go to End                ✓ Control flow last
```

### Mistake 3: Multiple Control Flow in Same Branch

```cns
❌ WRONG:
If: x > 0
  Then: repeat from Step 1
  Then: go to End                ✗ Second never runs!

✓ RIGHT:
If: x > 0
  Then: repeat from Step 1       ✓ Only one control flow
Otherwise:
  Then: go to End                ✓ In separate branch
```

### Mistake 4: Forgetting Loop Termination

```cns
❌ WRONG (Infinite Loop):
Step 1 → Loop forever
  Then: count becomes count + 1
  If: count > 0
    Then: repeat from Step 1     ✗ Always true!

✓ RIGHT:
Given:
  count: Integer = 0
  max: Integer = 10              ✓ Add limit

Step 1 → Loop with limit
  Then: count becomes count + 1
  If: count < max
    Then: repeat from Step 1     ✓ Will terminate
```

### Mistake 5: Jumping to Non-Existent Step

```cns
❌ WRONG:
Step 1:
  If: error
    Then: go to Step 99          ✗ Step 99 doesn't exist!

✓ RIGHT:
Step 1:
  If: error
    Then: go to End              ✓ End always exists

# Or:
Step 1:
  If: error
    Then: go to Step 5           ✓ Ensure Step 5 exists
    
Step 5 → Error handler
  Effect: Print "Error!"
```

---

## Loop Safety

### Always Include Exit Condition

Every loop **must** have a way to exit:

```cns
✓ SAFE - Has exit condition:
Step 1:
  If: count = 0
    Then: go to End              ✓ Exit condition
  Then: count becomes count - 1
  If: count > 0
    Then: repeat from Step 1

✗ UNSAFE - No exit:
Step 1:
  Then: count becomes count + 1
  If: count > 0                  ✗ Always true = infinite loop!
    Then: repeat from Step 1
```

### Use Iteration Limits

For complex loops, use the `--max-iterations` flag:

```bash
./cns-run --max-iterations 1000 program.cns
```

Default limit: 10,000 iterations

---

## Control Flow Decision Tree

**Question 1:** Do you need to repeat steps?  
→ **YES:** Use `repeat from Step N`  
→ **NO:** Continue to Question 2

**Question 2:** Do you need to skip ahead?  
→ **YES:** Use `go to Step N`  
→ **NO:** Continue to Question 3

**Question 3:** Do you need to exit early?  
→ **YES:** Use `go to End`  
→ **NO:** No control flow needed, just sequential steps

**Question 4:** Is your control flow inside an `If:` or `Otherwise:` branch?  
→ **YES:** Valid! ✓  
→ **NO:** Invalid! Move it inside a branch ✗

---

## Debugging Control Flow

### Problem: Loop Never Terminates

**Symptoms:**
- Program runs until iteration limit (10,000)
- Same step repeats forever

**Diagnosis:**
```bash
./cns-run --max-iterations 100 program.cns
```

Check the state snapshot in error:
```
State snapshot:
    counter = NIL    ← Problem: Variable became NIL!
```

**Fix:** Ensure loop variables update correctly

---

### Problem: Jump to Wrong Step

**Symptoms:**
- Unexpected behavior
- Steps execute out of order

**Diagnosis:** Add verbose logging:
```cns
Effect: Print "At Step 1, counter={counter}"
```

**Fix:** Verify step numbers match your jumps

---

### Problem: Control Flow Ignored

**Symptoms:**
- `repeat from` doesn't loop
- `go to` doesn't jump

**Diagnosis:** Check if control flow is inside a branch:
```cns
❌ NOT inside If/Otherwise - ignored!
Step 1:
  Then: repeat from Step 1

✓ Inside If/Otherwise - works!
Step 1:
  If: true
    Then: repeat from Step 1
```

---

## Summary: Control Flow Checklist

Before committing control flow code, verify:

- ☐ Control flow is inside `If:` or `Otherwise:` branch
- ☐ Control flow is the **last** `Then:` in the branch
- ☐ Loop has an exit condition
- ☐ Target step exists (or use `go to End`)
- ☐ Loop variables update correctly
- ☐ Not mixing multiple control flows in same branch

---

## Advanced: Step Execution Order

Understanding how CNS executes steps:

1. **Sequential by default:** Steps run 1 → 2 → 3 → ...
2. **Control flow changes PC:** `repeat from` or `go to` sets program counter
3. **If branches select path:** Only one branch executes
4. **Otherwise is else:** Runs when If condition is false
5. **End terminates:** No implicit looping

**Example trace:**
```cns
Step 1: counter=3
  If: counter > 0
    Then: counter becomes 2
    Then: repeat from Step 1   ← PC = 1
    
Step 1: counter=2              ← Jumped back
  If: counter > 0
    Then: counter becomes 1
    Then: repeat from Step 1   ← PC = 1
    
Step 1: counter=1              ← Jumped back
  If: counter > 0
    Then: counter becomes 0
    Then: repeat from Step 1   ← PC = 1
    
Step 1: counter=0              ← Jumped back
  If: counter > 0              ← False!
  Otherwise:
    Then: go to End            ← PC = END
    
End: Return 0
```

---

## For LLMs: Generation Guidelines

When generating CNS with control flow:

1. **Always wrap control flow in If/Otherwise**
2. **Make control flow the last Then in branch**
3. **Provide clear exit conditions for loops**
4. **Use descriptive step names for jump targets**
5. **Test with `--max-iterations 100` during development**

**Template:**
```cns
Story: [Name]

Given:
  [loop variable]: Integer = [initial]
  [condition variable]: Boolean = true

Step 1 → [Check exit condition]
  If: [exit condition]
    Then: go to End              ✓ Always have exit
    
Step 2 → [Loop body]
  Then: [update variables]
  If: [continue condition]
    Then: repeat from Step 1     ✓ Loop back
  Otherwise:
    Then: go to End              ✓ Explicit exit

End: Return [result]
```

