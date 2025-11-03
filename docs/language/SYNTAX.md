# CNS Language Syntax Reference

**Purpose**: Complete CNS syntax reference optimized for LLM code generation.

**Audience**: LLMs generating CNS code, developers learning CNS.

**Philosophy**: Pattern-focused with ❌ Wrong / ✅ Right examples for common mistakes.

---

## Quick Reference

```cns
Story: Program Name

Given:
  variable: Type = value

Step N → Description
  Because: Reason for this step
  Then: variable becomes expression
  Effect: Action (Print, HTTP, etc.)
  If: condition
    Then: action
  Otherwise:
    Then: action

End: Return value
```

---

## Table of Contents

1. [Story Structure](#story-structure)
2. [Variables & Types](#variables--types)
3. [Expressions](#expressions)
4. [Conditionals](#conditionals)
5. [Loops & Control Flow](#loops--control-flow)
6. [Effects](#effects)
7. [Functions](#functions)
8. [Error Handling](#error-handling)
9. [CNSC Compact Format](#cnsc-compact-format)
10. [Limitations & Workarounds](#limitations--workarounds)

---

## Story Structure

### Minimal Story

```cns
Story: Hello World

Given:
  message: String = "Hello, CNS!"

Step 1 → Print message
  Effect: Print "{message}"

End: Return 0
```

**Required sections:**
- `Story: Name` - Program title
- `Given:` - Variable declarations
- `Step N → Description` - At least one step
- `End: Return value` - Program exit

### Story with Causality

```cns
Story: Calculate Factorial
Because: Demonstrate recursive multiplication

Given:
  n: Integer = 5
  result: Integer = 1

Step 1 → Multiply by current n
  Because: Accumulate factorial product
  Then: result becomes result * n
  Then: n becomes n - 1

Step 2 → Check if done
  Because: Stop when n reaches 1
  If: n > 1
    Then: repeat from Step 1

End: Return result
```

**Best practices:**
- Add `Because:` to explain step purpose (improves LLM understanding)
- Use descriptive step names (not just "Step 1")
- Include story-level `Because:` for context

---

## Variables & Types

### Declaration Syntax

```cns
Given:
  name: String = "Alice"
  age: Integer = 30
  score: Integer = 0
  items: List = []
  settings: Map = {}
```

**Available types:**
- `Integer` - Whole numbers
- `String` - Text (quoted)
- `List` - Ordered collection
- `Map` - Key-value pairs
- `Socket` - Network socket (special)

### ❌ Wrong / ✅ Right

```cns
# ❌ Wrong - missing type annotation
Given:
  count = 0

# ✅ Right - type explicitly declared
Given:
  count: Integer = 0
```

```cns
# ❌ Wrong - string without quotes
Given:
  name: String = Alice

# ✅ Right - string with quotes
Given:
  name: String = "Alice"
```

```cns
# ❌ Wrong - multiline value (parser doesn't support)
Given:
  config: String = "line1
  line2"

# ✅ Right - single line or build dynamically
Given:
  config: String = "line1"
  
Step 1:
  Then: config becomes config + "\nline2"
```

### Variable Assignment

```cns
Step 1 → Update variables
  Then: count becomes count + 1
  Then: name becomes "Bob"
  Then: total becomes sum + tax
```

**Syntax:** `Then: variable becomes expression`

**Common mistake:**
```cns
# ❌ Wrong - "Set" syntax (inconsistent support)
Step 1:
  Set count to 5

# ✅ Right - "becomes" syntax (universal)
Step 1:
  Then: count becomes 5
```

---

## Expressions

### Supported Operators

**Arithmetic:**
- `a + b` - Addition
- `a - b` - Subtraction
- `a * b` - Multiplication
- `a / b` - Division
- `a % b` - Modulo

**Comparison:**
- `a = b` or `a == b` - Equality
- `a > b` - Greater than
- `a < b` - Less than
- `a >= b` - Greater than or equal
- `a <= b` - Less than or equal
- `a != b` - Not equal

**Logical:**
- `a AND b` - Logical AND
- `a OR b` - Logical OR
- `NOT a` - Logical NOT

**String:**
- `"text" + variable + "more"` - Concatenation

### Expression Evaluation

```cns
# Simple expressions (always work)
Then: sum becomes a + b
Then: product becomes x * y
Then: is_valid becomes x > 10
```

### ❌ CRITICAL LIMITATIONS ❌

**These expression patterns FAIL silently (return NIL):**

```cns
# ❌ Literal-first multiplication
Then: result becomes 3 * n
# Result: NIL (not 3n!)

# ❌ Multi-operator expressions
Then: result becomes n * 3 + 1
# Result: NIL (not 3n+1!)

# ❌ Complex nested expressions
Then: result becomes (a + b) * (c - d)
# Result: NIL (no parentheses support)
```

### ✅ Workarounds that WORK

**Pattern 1: Variable-first order**
```cns
# ❌ Wrong
Then: result becomes 3 * n

# ✅ Right - swap order
Then: result becomes n * 3
```

**Pattern 2: Split multi-operator into steps**
```cns
# ❌ Wrong
Then: result becomes n * 3 + 1

# ✅ Right - use temporary variable
Then: temp becomes n * 3
Then: result becomes temp + 1
```

**Pattern 3: Simplify**
```cns
# ❌ Wrong
Then: area becomes (width + margin) * (height + margin)

# ✅ Right - step by step
Then: w becomes width + margin
Then: h becomes height + margin
Then: area becomes w * h
```

### String Concatenation

```cns
# Multi-part concatenation with +
Then: message becomes "Count: " + count + " items"

# Variable interpolation in Print
Effect: Print "Found {count} items"

# Mixed types (auto-converted)
Then: label becomes name + " is " + age + " years old"
```

---

## Conditionals

### Basic If/Otherwise

```cns
Step 1 → Check value
  If: x > 10
    Then: status becomes "high"
  Otherwise:
    Then: status becomes "low"
```

**Important rules:**
1. Only ONE branch executes (If OR Otherwise, never both)
2. Indentation matters (2 spaces for branch content)
3. Control flow (`repeat from`, `go to`) ONLY works inside If/Otherwise

### Multiple Conditions (Waterfall Pattern)

**❌ Nested If/Otherwise NOT supported:**
```cns
# ❌ Wrong - nested If (doesn't work!)
Step 1:
  If: lang == "rust"
    If: has_tests == true
      Then: cmd becomes "cargo test"
```

**✅ Use waterfall pattern (sequential steps):**
```cns
# ✅ Right - waterfall with early exit
Step 1 → Check Rust with tests
  If: lang == "rust" AND has_tests == true
    Then: cmd becomes "cargo test"
    Then: go to Step 10

Step 2 → Check Rust without tests
  If: lang == "rust"
    Then: cmd becomes "cargo build"
    Then: go to Step 10

Step 3 → Check Go
  If: lang == "go"
    Then: cmd becomes "go test"
    Then: go to Step 10

Step 10 → Execute command
  Effect: SHELL "{cmd}"
```

### If/Otherwise with Effects

```cns
Step 1 → Log user type
  If: user == "admin"
    Effect: Print "Admin logged in"
    Then: permissions becomes "all"
  Otherwise:
    Effect: Print "Regular user logged in"
    Then: permissions becomes "limited"
```

### If/Otherwise with Control Flow

```cns
Step 1 → Route based on condition
  If: count < 100
    Then: count becomes count + 1
    Then: repeat from Step 1
  Otherwise:
    Then: go to End
```

**Key insight:** This is how you build loops in CNS!

---

## Loops & Control Flow

### repeat from Step N

**Pattern: Loop until condition**

```cns
Story: Count to 10

Given:
  counter: Integer = 0

Step 1 → Increment counter
  Then: counter becomes counter + 1

Step 2 → Check if should continue
  If: counter < 10
    Then: repeat from Step 1
  Otherwise:
    Then: go to End

End: Return counter
```

**Key:** Control flow must be inside If/Otherwise!

### go to Step N

**Pattern: Skip forward conditionally**

```cns
Step 1 → Check early exit
  If: value == 0
    Then: go to Step 5

Step 2 → Do work
  Then: result becomes calculate(value)

Step 5 → Finish
  Effect: Print "Done"
```

### go to End

**Pattern: Early termination**

```cns
Step 1 → Validate input
  If: input == ""
    Effect: Print "Error: Input required"
    Then: go to End

Step 2 → Process valid input
  Then: result becomes process(input)

End: Return result
```

### ❌ Wrong / ✅ Right

```cns
# ❌ Wrong - control flow in regular step
Step 1:
  Then: counter becomes counter + 1
  Then: repeat from Step 1
# This will be IGNORED!

# ✅ Right - control flow in If branch
Step 1:
  Then: counter becomes counter + 1
  If: counter < 10
    Then: repeat from Step 1
```

### For-Each Loops

```cns
Given:
  numbers: List = [10, 20, 30, 40, 50]
  sum: Integer = 0

Step 1 → Sum all numbers
  For each num in numbers:
    Then: sum becomes sum + num

End: Return sum
```

**Syntax variations:**
- `For each item in list:`
- `FOREACH item IN list:`
- Both work identically

---

## Effects

Effects perform actions with side effects (I/O, network, etc.).

### Print

```cns
Effect: Print "Hello, World!"
Effect: Print "Count: {count}"
Effect: Print "Name: " + name + ", Age: " + age
```

**Variable interpolation:** Use `{varname}` or concatenation.

### HTTP GET

```cns
Given:
  url: String = "https://api.example.com/data"
  response: String = ""
  status: Integer = 0

Step 1 → Call API
  Effect: HTTP GET from url into response
  
Step 2 → With status code
  Effect: HTTP GET from url into response with status code status
  
Step 3 → Check response
  If: status == 200
    Then: data becomes PARSE JSON response GET "result"
```

**Patterns:**
- `HTTP GET from url into variable`
- `HTTP GET from url into variable with status code status_var`

### HTTP POST

```cns
Given:
  url: String = "https://api.example.com/submit"
  body: String = "{\"name\": \"Alice\"}"
  response: String = ""

Step 1 → POST data
  Effect: HTTP POST to url with body body into response
```

### File I/O

```cns
# Write file
Effect: WRITE "Hello, file!" TO FILE "/tmp/test.txt"

# Append to file
Effect: APPEND "More text\n" TO FILE "/tmp/test.txt"

# Read file
Then: content becomes READ FROM FILE "/tmp/test.txt"
```

### Shell Execution

```cns
Given:
  output: String = ""
  exit_code: Integer = 0

Step 1 → Run command
  Effect: SHELL "ls -la" INTO output WITH EXIT_CODE exit_code

Step 2 → Check success
  If: exit_code == 0
    Effect: Print "Success!"
```

**Syntax:**
- `SHELL "command"`
- `SHELL "command" INTO variable`
- `SHELL "command" INTO variable WITH EXIT_CODE code_var`

### File/Content Search

```cns
# Find files by pattern
Then: cns_files becomes FIND "*.cns" IN "examples"

# With count
Then: cns_files becomes FIND "*.cns" IN "examples" WITH COUNT count

# Search file contents
Then: matches becomes GREP "TODO" IN "src/code.lisp"

# GREP on multiple files
Then: files becomes FIND "*.lisp" IN "src"
Then: todos becomes GREP "TODO" IN files
```

### Git Operations

```cns
# Clone repository
Effect: GIT CLONE "https://github.com/user/repo" TO "/tmp/repo"

# Check status
Then: status becomes GIT STATUS IN "/tmp/repo"

# Get diff
Then: changes becomes GIT DIFF IN "/tmp/repo"

# Commit changes
Effect: GIT ADD "file.txt" IN "/tmp/repo"
Effect: GIT COMMIT "Fix bug" IN "/tmp/repo"
```

### Database (SQLite)

```cns
# Connect to database
Effect: DATABASE CONNECT TO "/tmp/data.db" AS db

# Execute DDL/DML
Effect: DATABASE EXECUTE "CREATE TABLE users (id INT, name TEXT)" ON db

# Query data
Then: results becomes DATABASE QUERY "SELECT * FROM users" ON db
```

### DateTime

```cns
# Current timestamp
Then: now becomes NOW()

# Format timestamp
Then: formatted becomes FORMAT TIME now AS "YYYY-MM-DD HH:mm:ss"

# Time arithmetic
Then: tomorrow becomes ADD 1 DAYS TO now
Then: next_week becomes ADD 7 DAYS TO now
```

---

## Functions

### Function Definition

```cns
Function: power(base: Integer, exponent: Integer) returns Integer
  Because: Calculate base^exponent using recursion

  Given:
    result: Integer = 1

  Step 1 → Base case
    If: exponent = 0
      Then: go to End

  Step 2 → Recursive case
    Then: result becomes base * power(base, exponent - 1)

  End: Return result
```

**Syntax:**
- `Function: name(param: Type, ...) returns Type`
- Function body has same structure as Story
- Recursion is supported

### Calling Functions

```cns
Story: Use Power Function

Given:
  x: Integer = 2
  y: Integer = 8
  result: Integer = 0

Step 1 → Call function
  Then: result becomes power(x, y)

End: Return result
```

---

## Error Handling

### Error Section

```cns
Story: Safe Division

Given:
  numerator: Integer = 10
  denominator: Integer = 0
  result: Integer = 0

Step 1 → Divide
  Then: result becomes numerator / denominator

Error:
  Effect: Print "Division by zero!"
  Return -1

End: Return result
```

**Behavior:** If any step errors, jumps to `Error:` section.

---

## CNSC Compact Format

CNS Compact (CNSC) is a 62% smaller format for LLM efficiency.

### Comparison

**Verbose CNS (150 lines):**
```cns
Story: Calculate Factorial

Given:
  n: Integer = 5
  result: Integer = 1

Step 1 → Multiply
  Because: Accumulate factorial
  Then: result becomes result * n
  Then: n becomes n - 1

Step 2 → Check done
  Because: Stop at 1
  If: n > 1
    Then: repeat from Step 1

End: Return result
```

**Compact CNSC (60 lines):**
```cnsc
Story: Calculate Factorial

G: n:I=5, result:I=1

S1→ Multiply
  Because: Accumulate factorial
  → result = result * n
  → n = n - 1

S2→ Check done
  Because: Stop at 1
  If n>1: repeat from S1

E: result
```

### CNSC Syntax Rules

**Variable declarations:**
- `G:` = Given
- `var:I=5` = Integer
- `var:S="text"` = String
- `var:L=[]` = List

**Steps:**
- `S1→` = Step 1
- `→` = Then
- `=` instead of `becomes`

**Control:**
- `If condition:` = If condition
- `Otherwise:` = Otherwise
- `E:` = End

**See:** `examples/*.cnsc` files for more examples.

---

## Limitations & Workarounds

### 1. Expression Parser Limitations

**Problem:** Literal-first and multi-operator expressions fail.

**Workarounds:**
```cns
# ❌ Wrong: 3 * n
# ✅ Right: n * 3

# ❌ Wrong: n * 3 + 1
# ✅ Right:
Then: temp becomes n * 3
Then: result becomes temp + 1
```

### 2. Control Flow Location

**Problem:** `repeat from` and `go to` only work in If/Otherwise.

**Workarounds:**
```cns
# ❌ Wrong:
Step 1:
  Then: counter becomes counter + 1
  Then: repeat from Step 1

# ✅ Right:
Step 1:
  Then: counter becomes counter + 1
  
Step 2:
  If: counter < 10
    Then: repeat from Step 1
```

### 3. No Nested Conditionals

**Problem:** Nested If/Otherwise not supported.

**Workaround:** Use waterfall pattern (see Conditionals section).

### 4. No Multiline Strings

**Problem:** Parser doesn't handle multiline strings in declarations.

**Workaround:** Build strings step-by-step or use single line.

### 5. List/Map Access

**Working patterns:**
```cns
# List indexing
Then: first becomes items[0]
Then: second becomes items[1]

# List length
Then: count becomes LENGTH OF items

# Map key access
Then: value becomes map["key"]
```

---

## Best Practices for LLMs

### 1. Always Declare Types

```cns
# ✅ Good
Given:
  count: Integer = 0
  name: String = "default"
```

### 2. Use Simple Expressions

```cns
# ✅ Good - one operation per line
Then: temp becomes a * 3
Then: result becomes temp + b

# ❌ Avoid - complex expression
Then: result becomes a * 3 + b
```

### 3. Put Control Flow in Conditionals

```cns
# ✅ Good
Step 2:
  If: counter < 10
    Then: repeat from Step 1
```

### 4. Add Because Clauses

```cns
# ✅ Good - explains reasoning
Step 1 → Increment counter
  Because: Track loop iterations
  Then: counter becomes counter + 1
```

### 5. Use Waterfall Pattern for Multiple Conditions

```cns
# ✅ Good - sequential checks with early exit
Step 1 → Check condition A
  If: a == true
    Then: go to Step 10

Step 2 → Check condition B
  If: b == true
    Then: go to Step 10
```

---

## Complete Working Examples

### Example 1: HTTP API Call

```cns
Story: Fetch User Data

Given:
  url: String = "https://jsonplaceholder.typicode.com/users/1"
  response: String = ""
  status: Integer = 0
  name: String = ""

Step 1 → Call API
  Because: Get user data from server
  Effect: HTTP GET from url into response with status code status

Step 2 → Parse response
  Because: Extract name from JSON
  If: status == 200
    Then: name becomes PARSE JSON response GET "name"
    Effect: Print "User: {name}"
  Otherwise:
    Effect: Print "API call failed with status {status}"

End: Return name
```

### Example 2: File Processing Loop

```cns
Story: Count Lines in File

Given:
  filename: String = "/tmp/data.txt"
  content: String = ""
  lines: List = []
  count: Integer = 0

Step 1 → Read file
  Because: Load file contents
  Then: content becomes READ FROM FILE filename

Step 2 → Split into lines
  Because: Separate lines for counting
  Then: lines becomes SPLIT content BY "\n"

Step 3 → Get count
  Because: Count total lines
  Then: count becomes LENGTH OF lines
  Effect: Print "File has {count} lines"

End: Return count
```

### Example 3: Loop with Accumulator

```cns
Story: Calculate Sum of Range

Given:
  current: Integer = 1
  limit: Integer = 100
  sum: Integer = 0

Step 1 → Add current to sum
  Because: Accumulate total
  Then: sum becomes sum + current
  Then: current becomes current + 1

Step 2 → Check if done
  Because: Stop when limit reached
  If: current <= limit
    Then: repeat from Step 1

End: Return sum
```

---

## See Also

- **examples/README.md** - Pattern guide with working examples
- **examples/core/** - Fundamental CNS patterns
- **examples/features/** - Feature-specific examples
- **docs/development/QUICK-REFERENCE-IF-OTHERWISE.md** - If/Otherwise quick reference
- **docs/development/TEST-RESULTS-2025-11-02.md** - Known working features

---

*Last Updated: 2025-11-02*  
*Status: Complete syntax reference*  
*Audience: LLMs and developers*
