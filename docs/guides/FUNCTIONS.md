# Functions in CNS

## Overview

Functions in CNS allow code reuse and modularity while maintaining the explicit, narrative structure of the language. Functions are simply **Stories that can be called by other Stories**.

## Design Philosophy

Functions follow the "Simplicity First" principle:

✅ **Reuses existing keywords**: Story, Given, Step, End  
✅ **No new concepts**: Functions ARE stories, just marked as callable  
✅ **Explicit parameters**: First N variables in Given: become parameters  
✅ **Explicit returns**: End: Return clearly states what's returned  
✅ **Consistent structure**: Same Story/Given/Step/End pattern  

## Basic Syntax

### Defining a Function

```cns
Story: FunctionName (function)
Given:
  param1: Type
  param2: Type
  result: Type = initialValue

Step 1 → Do something
  Because: explanation
  Then: result becomes param1 + param2

End: Return result
```

**Key points**:
- Add `(function)` tag after function name in Story line
- Variables in `Given:` section become parameters (in order)
- `End: Return <variable>` specifies what value is returned
- Everything else is standard CNS

### Calling a Function

```cns
Story: Main Program
Given:
  myResult: Integer

Step 1 → Call the function
  Because: need to compute something
  Then: myResult becomes FunctionName(10, 20)

End: Return myResult
```

**Key points**:
- Call syntax: `FunctionName(arg1, arg2, ...)`
- Arguments passed positionally (match parameter order)
- Result can be assigned to a variable

## Complete Example

```cns
Story: Multiply (function)
Given:
  a: Integer
  b: Integer
  product: Integer = 0

Step 1 → Calculate product
  Because: multiply a by b
  Then: product becomes a * b

End: Return product

---

Story: Add (function)
Given:
  x: Integer
  y: Integer
  sum: Integer = 0

Step 1 → Calculate sum
  Because: add x and y
  Then: sum becomes x + y

End: Return sum

---

Story: Calculate Total
Given:
  first: Integer = 5
  second: Integer = 10
  multiplied: Integer
  final: Integer

Step 1 → Multiply the numbers
  Because: need product before addition
  Then: multiplied becomes Multiply(first, second)

Step 2 → Add to get final result
  Because: combining multiplied value with first number
  Then: final becomes Add(multiplied, first)

End: Return final
```

**Output**: 55 (because 5 * 10 = 50, then 50 + 5 = 55)

## File Structure

### Multiple Stories Per File

Use `---` (three dashes) to separate multiple stories in one file:

```cns
Story: Function1 (function)
Given:
  ...
End: Return ...

---

Story: Function2 (function)
Given:
  ...
End: Return ...

---

Story: Main
Given:
  ...
End: Return ...
```

**Rules**:
- The story WITHOUT `(function)` tag is the entry point
- If all stories have `(function)` tag, the FIRST one is the entry point
- Function order doesn't matter (can be called before defined)

## CNSC (Compact) Format

Functions work in CNSC too:

```cnsc
Story: Multiply (function)
G: a:I, b:I, product:I=0
S1→ product=a*b
E: product

---

Story: Add (function)
G: x:I, y:I, sum:I=0
S1→ sum=x+y
E: sum

---

Story: Main
G: first:I=5, second:I=10, multiplied:I, final:I
S1→ multiplied=Multiply(first, second)
S2→ final=Add(multiplied, first)
E: final
```

Same 70-76% token reduction applies!

## Parameter Passing

### Positional Parameters

Parameters are matched by position in the `Given:` section:

```cns
Story: Divide (function)
Given:
  numerator: Integer    # First parameter
  denominator: Integer  # Second parameter
  result: Integer = 0

...

Story: Main
Given:
  answer: Integer

Step 1 → Call divide
  Then: answer becomes Divide(100, 5)  # 100 → numerator, 5 → denominator
```

### Local Variables

Variables declared AFTER parameters are local to the function:

```cns
Story: Factorial (function)
Given:
  n: Integer              # Parameter
  result: Integer = 1     # Local variable
  counter: Integer = 1    # Local variable

...
```

When calling: `Factorial(5)` only passes `n`, other variables are initialized locally.

## Return Values

### Single Return Value

```cns
End: Return result
```

Returns the value of the `result` variable.

### Return Expressions

```cns
End: Return x + y * 2
```

Can return calculated expressions (evaluated before returning).

### Return Literals

```cns
End: Return 42
End: Return "success"
End: Return [1, 2, 3]
```

Can return literal values directly.

## Scope Rules

1. **Function parameters** are local to the function
2. **Local variables** declared in function's Given: are local
3. **No global variables** - functions cannot access caller's variables
4. **No side effects** (except Effects) - functions should compute and return

### Example of Scope

```cns
Story: AddOne (function)
Given:
  value: Integer
  result: Integer = 0

Step 1 → Add one
  Because: increment by one
  Then: result becomes value + 1

End: Return result

---

Story: Main
Given:
  x: Integer = 5
  y: Integer

Step 1 → Call function
  Because: need to increment x
  Then: y becomes AddOne(x)

# x is still 5 here (not modified)
# y is now 6

End: Return y
```

**Important**: `x` in Main is NOT modified by the function. The function receives a COPY of x's value.

## Recursion

Functions can call themselves:

```cns
Story: Factorial (function)
Given:
  n: Integer
  result: Integer = 1

Step 1 → If n <= 1
  Because: base case for recursion
  Then: go to End
  Otherwise: go to Step 2

Step 2 → Recursive calculation
  Because: factorial(n) = n * factorial(n-1)
  Then: result becomes n * Factorial(n - 1)

End: Return result

---

Story: Main
Given:
  answer: Integer

Step 1 → Calculate factorial
  Because: compute 5!
  Then: answer becomes Factorial(5)

End: Return answer
```

**Output**: 120

## Functions with Effects

Functions can have side effects (printing, logging, file I/O):

```cns
Story: LogAndAdd (function)
Given:
  a: Integer
  b: Integer
  sum: Integer = 0

Step 1 → Calculate sum
  Because: add the two numbers
  Then: sum becomes a + b
  Effect: Log "Computing {a} + {b} = {sum}"

End: Return sum
```

Effects execute during function execution (visible in traces).

## Advanced Examples

### Example 1: Math Library

```cns
Story: GCD (function)
Given:
  a: Integer
  b: Integer
  remainder: Integer

Step 1 → If b = 0
  Because: found the GCD
  Then: go to End
  Otherwise: go to Step 2

Step 2 → Calculate remainder
  Because: Euclidean algorithm step
  Then: remainder becomes a % b

Step 3 → Recursive call
  Because: continue until b = 0
  Then: a becomes GCD(b, remainder)
  Then: go to End

End: Return a

---

Story: LCM (function)
Given:
  x: Integer
  y: Integer
  gcd: Integer
  lcm: Integer = 0

Step 1 → Find GCD
  Because: LCM = (x * y) / GCD(x, y)
  Then: gcd becomes GCD(x, y)

Step 2 → Calculate LCM
  Because: apply the formula
  Then: lcm becomes (x * y) / gcd

End: Return lcm

---

Story: Main
Given:
  result: Integer

Step 1 → Calculate LCM of 12 and 18
  Because: demonstrate math library
  Then: result becomes LCM(12, 18)
  Effect: Print "LCM(12, 18) = {result}"

End: Return result
```

### Example 2: Text Processing

```cns
Story: CountWords (function)
Given:
  text: String
  count: Integer = 1
  i: Integer = 0
  length: Integer

Step 1 → Get text length
  Because: need to iterate through string
  Then: length becomes length of text

Step 2 → If i >= length
  Because: reached end of string
  Then: go to End
  Otherwise: go to Step 3

Step 3 → Check for space
  Because: spaces separate words
  Then: if text at i = " " then count becomes count + 1

Step 4 → Move to next character
  Because: continue scanning
  Then: i becomes i + 1
  Then: repeat from Step 2

End: Return count

---

Story: Main
Given:
  sentence: String = "Hello world from CNS"
  wordCount: Integer

Step 1 → Count the words
  Because: analyze the sentence
  Then: wordCount becomes CountWords(sentence)
  Effect: Print "Word count: {wordCount}"

End: Return wordCount
```

### Example 3: Validation Function

```cns
Story: IsValidPort (function)
Given:
  port: Integer
  valid: Integer = 0

Step 1 → If port >= 1024 AND port <= 65535
  Because: valid port range for user applications
  Then: valid becomes 1
  Otherwise: valid becomes 0

End: Return valid

---

Story: StartServer
Given:
  port: Integer = 8080
  isValid: Integer
  server_socket: Socket

Step 1 → Validate port
  Because: ensure port is in valid range
  Then: isValid becomes IsValidPort(port)

Step 2 → If isValid = 0
  Because: invalid port number
  Then: go to End
  Effect: Print "Error: Invalid port {port}"
  Otherwise: go to Step 3

Step 3 → Create server
  Because: port is valid, can start server
  Effect: Create socket server_socket on port
  Effect: Print "Server started on port {port}"

End: Return isValid
```

## Error Handling in Functions

Functions can have Error sections:

```cns
Story: SafeDivide (function)
Given:
  numerator: Integer
  denominator: Integer
  result: Integer = 0

Step 1 → If denominator = 0
  Because: cannot divide by zero
  Then: go to Error
  Otherwise: go to Step 2

Step 2 → Perform division
  Because: denominator is non-zero
  Then: result becomes numerator / denominator

End: Return result

Error:
  Return -1
  Effect: Log "Division by zero attempted"
  Because: Return sentinel value for error case
```

## Best Practices

### 1. Keep Functions Small

**Good**:
```cns
Story: Add (function)
Given:
  a: Integer
  b: Integer
  sum: Integer = 0

Step 1 → Calculate sum
  Because: add the two numbers
  Then: sum becomes a + b

End: Return sum
```

**Avoid**:
```cns
Story: DoEverything (function)
# 50 steps doing multiple unrelated things
```

### 2. Use Descriptive Names

**Good**: `CalculateInterest`, `ValidateEmail`, `FormatAddress`  
**Avoid**: `Calc`, `Check`, `DoIt`

### 3. Document Parameters in Story

```cns
Story: CalculateInterest (function) - Computes compound interest
Given:
  principal: Integer    # Initial amount
  rate: Integer         # Interest rate (percentage)
  years: Integer        # Number of years
  result: Integer = 0
```

### 4. Minimize Side Effects

Functions SHOULD:
- ✅ Calculate and return values
- ✅ Use parameters and local variables

Functions SHOULD MINIMIZE:
- ⚠️ Printing (except debugging)
- ⚠️ File I/O (except when that's the purpose)
- ⚠️ Modifying global state

### 5. Test Functions Independently

Each function should be testable on its own:

```bash
# Create test file for single function
cat > test-multiply.cns <<EOF
Story: Multiply (function)
...
End: Return product

---

Story: TestMultiply
Given:
  result: Integer

Step 1 → Test case 1
  Then: result becomes Multiply(5, 10)
  Effect: Print "5 * 10 = {result}"

End: Return result
EOF

./cns-run test-multiply.cns
```

## Limitations (Current Version)

1. **No default parameters** - all parameters must be provided
2. **No variable arguments** - fixed parameter count
3. **No named parameters** - must pass arguments in order
4. **No closures** - functions cannot capture outer scope
5. **No higher-order functions** - cannot pass functions as arguments (yet)

These may be added in future versions if they pass the Development Checklist!

## CNSC Quick Reference

```cnsc
# Function definition
Story: FuncName (function)
G: param1:I, param2:I, result:I=0
S1→ result=param1+param2
E: result

---

# Function call
Story: Main
G: x:I
S1→ x=FuncName(10, 20)
E: x
```

## See Also

- [Development Checklist](../development/DEVELOPMENT-CHECKLIST.md) - Why functions were designed this way
- [LLM Integration](../guides/LLM-INTEGRATION.md) - Using functions with LLMs
- [CNSC Compact Format](../guides/CNSC-COMPACT.md) - Token-efficient syntax

---

**Functions make CNS powerful while staying true to its LLM-friendly design!**
