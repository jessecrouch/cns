# CNS Trace Mode Guide

**Purpose:** Visualize program execution for debugging and understanding control flow

---

## Quick Start

```bash
# Enable trace mode with --trace flag
./cns-run --trace examples/core/factorial.cns

# Combine with other flags
./cns-run --trace --strict --max-iterations 1000 program.cns
```

**Output:**
```
[Iter 1] Step 1 → Check if done
  n = 5
  result = 1

[Iter 2] Step 1 → Check if done
  n = 4
  result = 5

[Iter 3] Step 1 → Check if done
  n = 3
  result = 20
...
```

---

## Features

### Smart Output Strategy

Trace mode uses an intelligent output strategy to avoid spam:

- **Iterations 1-10:** Show every step
- **Iterations 11+:** Show every 10th iteration (10, 20, 30, ...)

This allows you to:
- See initial execution in detail
- Detect infinite loops quickly
- Avoid overwhelming output for long-running programs

### Variable Display

Each trace line shows:
- **Iteration count:** `[Iter N]`
- **Step number:** `Step N`
- **Step label:** `→ Description` (if present)
- **Up to 5 key variables** with their current values

**Example:**
```
[Iter 1] Step 3 → Calculate result
  x = 10
  y = 20
  result = 0
  counter = 1
  temp = 5
```

### What Gets Traced

Trace mode shows:
- Every step execution (with smart filtering)
- All variable values at each step
- Control flow (jumps, loops, conditionals)
- Function calls and returns

---

## Use Cases

### 1. Debugging Infinite Loops

**Problem:** Program runs forever, you don't know why

**Solution:** Use trace mode to see what's repeating

```bash
./cns-run --trace my-program.cns
```

**What to look for:**
- Same step repeating with same variable values
- Variables becoming NIL (shown as "NIL")
- Loop counter not changing

**Example Output:**
```
[Iter 10] Step 2 → Loop check
  counter = NIL    ← Problem: counter is NIL!
  result = 42

[Iter 20] Step 2 → Loop check
  counter = NIL    ← Still NIL, loop will never exit
  result = 42
```

### 2. Understanding Control Flow

**Problem:** Not sure which branches your code is taking

**Solution:** Watch the step numbers jump

```bash
./cns-run --trace my-program.cns
```

**Example Output:**
```
[Iter 1] Step 1 → Check condition
  x = 5

[Iter 2] Step 3 → True branch    ← Skipped Step 2!
  x = 5

[Iter 3] Step 1 → Check condition ← Jumped back
  x = 4
```

### 3. Variable Value Tracking

**Problem:** Variable has wrong value, don't know when it changed

**Solution:** See every value change in trace

```bash
./cns-run --trace my-program.cns
```

**Example Output:**
```
[Iter 1] Step 1 → Initialize
  result = 0    ← Starts at 0

[Iter 2] Step 2 → Calculate
  result = 10   ← Changed to 10

[Iter 3] Step 3 → Update
  result = NIL  ← Became NIL! Expression failed
```

### 4. Performance Analysis

**Problem:** Program is slow, not sure where

**Solution:** Count iterations between steps

```bash
./cns-run --trace my-program.cns
```

**What to look for:**
- Large iteration counts
- Steps that execute many times
- Unexpected loops

**Example Output:**
```
[Iter 10] Step 2 → Process item
  items_processed = 10

[Iter 20] Step 2 → Process item   ← Still in loop
  items_processed = 20

[Iter 100] Step 2 → Process item  ← 100 iterations in one step!
  items_processed = 100
```

---

## Combining with Other Flags

### Trace + Strict Mode

Catch NIL values immediately while tracing

```bash
./cns-run --trace --strict program.cns
```

**Benefits:**
- See exactly when NIL appears
- Get immediate error before continuing
- Understand what caused the NIL

**Example:**
```
[Iter 1] Step 1
  x = 10

[Iter 2] Step 2
  result = NIL    ← NIL detected!

=== CNS ERROR ===
TYPE: NIL-VALUE
ERROR: Variable 'result' is NIL (strict mode)
```

### Trace + Max Iterations

Set a custom iteration limit while tracing

```bash
./cns-run --trace --max-iterations 100 program.cns
```

**Benefits:**
- Stop earlier if you suspect infinite loop
- See first 10 iterations, then every 10th up to limit
- Useful for testing with large iteration counts

### All Flags Combined

```bash
./cns-run --trace --strict --max-iterations 1000 program.cns
```

**Use when:**
- Debugging complex issues
- Need maximum visibility
- Want to catch errors early
- Testing with custom limits

---

## Reading Trace Output

### Basic Format

```
[Iter N] Step M → Label
  var1 = value1
  var2 = value2
  ...
```

- `[Iter N]` - Iteration counter (how many steps executed total)
- `Step M` - Current step number being executed
- `→ Label` - Step description (if present)
- Variables listed with current values (max 5 shown)

### Iteration Numbers

**Consecutive iterations (1-10):**
```
[Iter 1] Step 1
[Iter 2] Step 1  ← Loop back to Step 1
[Iter 3] Step 1
...
[Iter 10] Step 1
```

**Sparse iterations (11+):**
```
[Iter 10] Step 1
[Iter 20] Step 1  ← Skipped 11-19
[Iter 30] Step 1  ← Skipped 21-29
```

### Variable Values

- **Numbers:** `counter = 42`
- **Strings:** `name = "John"`
- **NIL:** `result = NIL` (failed expression or uninitialized)
- **Lists:** Shown as list representation
- **Maps:** Shown as hash table representation

### Jump Patterns

**Normal progression:**
```
[Iter 1] Step 1
[Iter 2] Step 2
[Iter 3] Step 3
```

**Loop back:**
```
[Iter 3] Step 3
[Iter 4] Step 1  ← Jumped back
```

**Skip forward:**
```
[Iter 2] Step 2
[Iter 3] Step 5  ← Skipped 3-4
```

---

## Common Patterns

### Pattern 1: Successful Loop

Variables change each iteration, loop eventually exits:

```
[Iter 1] Step 1 → Count down
  counter = 5

[Iter 2] Step 1 → Count down
  counter = 4

[Iter 3] Step 1 → Count down
  counter = 3

[Iter 4] Step 1 → Count down
  counter = 2

[Iter 5] Step 1 → Count down
  counter = 1

[Iter 6] Step 1 → Count down
  counter = 0
  
(Program ends)
```

### Pattern 2: Infinite Loop (NIL Variable)

Variable becomes NIL, condition never met:

```
[Iter 5] Step 1 → Check
  counter = 1

[Iter 6] Step 2 → Update
  counter = NIL    ← Expression failed!

[Iter 7] Step 1 → Check
  counter = NIL    ← Still NIL

[Iter 10] Step 1 → Check
  counter = NIL    ← Will loop forever

[Iter 20] Step 1 → Check
  counter = NIL
```

### Pattern 3: Infinite Loop (No Change)

Variables don't change, loop condition always true:

```
[Iter 10] Step 2 → Loop
  x = 5
  y = 10

[Iter 20] Step 2 → Loop
  x = 5    ← No change
  y = 10   ← No change

[Iter 30] Step 2 → Loop
  x = 5    ← Still no change
  y = 10
```

### Pattern 4: Nested Loops

Inner loop completes many times:

```
[Iter 1] Step 1 → Outer loop
  i = 0

[Iter 2] Step 2 → Inner loop
  i = 0
  j = 0

[Iter 10] Step 2 → Inner loop
  i = 0
  j = 8   ← j increments

[Iter 11] Step 1 → Outer loop
  i = 1   ← i finally increments
```

---

## Performance Considerations

### Output Volume

- **Small programs (<50 iterations):** Minimal overhead
- **Medium programs (50-1000 iterations):** ~10-100 trace lines
- **Large programs (1000+ iterations):** ~100+ trace lines (every 10th)

### Memory Usage

Trace mode uses minimal memory:
- No history storage
- No state snapshots
- Just immediate output
- Same memory as without trace

### Execution Speed

Trace mode adds:
- Format string generation
- Console output time
- ~5-10% overhead for traced programs
- Negligible for most use cases

---

## Tips and Best Practices

### 1. Start Without Trace

Run your program normally first:

```bash
./cns-run program.cns
```

If it works, you probably don't need trace.

### 2. Use Trace for Debugging

Enable trace when something goes wrong:

```bash
# Program hangs?
./cns-run --trace --max-iterations 100 program.cns

# Wrong result?
./cns-run --trace program.cns > trace.log
```

### 3. Combine with Grep

Filter trace output for specific variables:

```bash
./cns-run --trace program.cns 2>&1 | grep "counter"
```

### 4. Save Trace to File

Capture full trace for analysis:

```bash
./cns-run --trace program.cns > trace.log 2>&1
```

### 5. Compare Iterations

Look for differences between early and late iterations:

```bash
./cns-run --trace program.cns 2>&1 | grep -E "Iter (1|100)"
```

---

## Troubleshooting

### "Too much output"

**Problem:** Trace generates thousands of lines

**Solution:** Use iteration limit

```bash
./cns-run --trace --max-iterations 50 program.cns
```

### "Can't find the problem"

**Problem:** Need to see more detail

**Solution:** Combine with verbose mode (automatic) and strict mode

```bash
./cns-run --trace --strict program.cns
```

### "Trace is confusing"

**Problem:** Hard to follow execution flow

**Solution:** Focus on iteration numbers and step numbers

Look for:
- Iteration count going up = execution continuing
- Step numbers jumping = control flow
- Variables changing = computation happening
- Variables stuck = potential infinite loop

---

## Examples

### Example 1: Factorial with Trace

**File:** `examples/core/factorial.cns`

```bash
./cns-run --trace examples/core/factorial.cns
```

**Output:**
```
[Iter 1] Step 1 → Check if done
  n = 5
  result = 1

[Iter 2] Step 1 → Check if done
  n = 4
  result = 5

[Iter 3] Step 1 → Check if done
  n = 3
  result = 20

[Iter 4] Step 1 → Check if done
  n = 2
  result = 60

[Iter 5] Step 1 → Check if done
  n = 1
  result = 120

[Iter 6] Step 1 → Check if done
  n = 0
  result = 120

Return: 120
```

### Example 2: Collatz Sequence

**File:** `examples/core/collatz.cns`

```bash
./cns-run --trace examples/core/collatz.cns
```

Shows the 3n+1 sequence with each step visible.

### Example 3: Debugging Loop

**Problem File:**
```cns
Story: Broken Loop

Given:
  counter: Integer = 10
  result: Integer = 0

Step 1 → Loop
  Then: result becomes result + counter
  Then: counter becomes counter - 1
  If: counter > 0
    Then: repeat from Step 1

End: Return result
```

**Trace Output:**
```bash
./cns-run --trace broken-loop.cns

[Iter 1] Step 1 → Loop
  counter = 10
  result = 0

[Iter 2] Step 1 → Loop
  counter = 9
  result = 10

[Iter 3] Step 1 → Loop
  counter = 8
  result = 19

...

[Iter 10] Step 1 → Loop
  counter = 1
  result = 54

Return: 54  ← Success!
```

---

## Summary

**Trace mode (`--trace`) provides:**
- ✅ Real-time execution visibility
- ✅ Smart output (first 10, then every 10th)
- ✅ Variable value tracking
- ✅ Control flow visualization
- ✅ Infinite loop detection aid
- ✅ Minimal performance overhead
- ✅ Works with strict mode and iteration limits

**Use trace mode when:**
- Debugging loops
- Understanding control flow
- Tracking variable changes
- Finding infinite loops
- Learning how CNS executes

**Don't use trace mode when:**
- Program works correctly
- Production deployments
- Performance benchmarking
- Output needs to be clean

---

**See Also:**
- [Strict Mode](../language/SYNTAX.md#strict-mode) - Immediate NIL error detection
- [Expression Limitations](../language/EXPRESSION-LIMITATIONS.md) - Why expressions return NIL
- [Control Flow Rules](../language/CONTROL-FLOW-RULES.md) - Understanding loops and jumps
