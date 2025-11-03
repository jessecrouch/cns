# LLM Common Mistakes & Fixes

**Purpose**: Document common errors LLMs make when generating CNS code and how to fix them.

**Based on**: Real testing with Grok-2, GPT-4, and other LLMs

**Last Updated**: 2025-11-03

---

## Quick Fix Checklist

Before submitting CNS code, verify:

- [ ] Comparisons use `=` not `==`
- [ ] Booleans are `TRUE` / `FALSE` (uppercase)
- [ ] Each `Then:` has exactly one action
- [ ] No semantic tags in variable declarations (no `[≥ 2]`)
- [ ] Error blocks only for exceptions, not validation
- [ ] All variables declared in `Given:` section
- [ ] Control flow (`go to`, `repeat from`) in If/Otherwise blocks only

---

## The Big 5 Mistakes

### 1. Using `==` for Comparison ❌

**Wrong:**
```cns
If: x == 5
  Then: print "five"
```

**Right:**
```cns
If: x = 5
  Then: go to Step 10
```

**Why**: CNS uses single `=` for both assignment (in `Then:` context) and comparison (in `If:` context)

**How to Remember**: Context matters! 
- `Then: x becomes 5` → assignment
- `If: x = 5` → comparison

---

### 2. Boolean Casing ❌

**Wrong:**
```cns
Given:
  is_prime: Boolean = True
  found: Boolean = false
```

**Right:**
```cns
Given:
  is_prime: Boolean = TRUE
  found: Boolean = FALSE
```

**Why**: CNS uses uppercase keywords consistently

**Common variants to avoid:**
- `True` / `False` (Python style)
- `true` / `false` (JavaScript style)
- `1` / `0` (C style) ← Actually this might work, but use TRUE/FALSE

---

### 3. Multi-Action Then: Clause ❌

**Wrong:**
```cns
Step 5 → Check divisor
  If: num % i = 0
    Then: is_prime becomes FALSE
         go to End
```

**Right - Option A** (Separate steps):
```cns
Step 5 → Check divisor
  If: num % i = 0
    Then: is_prime becomes FALSE

Step 6 → Early exit if not prime
  If: is_prime = FALSE
    Then: go to End
```

**Right - Option B** (Multiple Then: lines):
```cns
Step 5 → Check divisor
  If: num % i = 0
    Then: is_prime becomes FALSE
    Then: go to End
```

**Why**: Each `Then:` should be a single logical action. Multiple `Then:` lines are allowed in CNS, but mixing state change + control flow on "one conceptual line" confuses the parser.

**Best Practice**: Use Option A for clarity

---

### 4. Semantic Tags in Declarations ❌

**Wrong:**
```cns
Given:
  num: Integer [≥ 2] = 17
  count: Integer [0..100] = 0
```

**Right:**
```cns
Given:
  num: Integer = 17
  count: Integer = 0
```

**Why**: Current CNS parser doesn't support semantic tags (though they're documented as "future feature")

**Alternative**: Put constraints in comments
```cns
Given:
  num: Integer = 17  # Must be >= 2
  count: Integer = 0  # Range: 0-100
```

---

### 5. Error Blocks for Validation ❌

**Wrong:**
```cns
Given:
  num: Integer = 17

Step 1 → Process number
  Then: result becomes num * 2

Error:
  Return FALSE
  Because: Handle case where num < 2
```

**Right:**
```cns
Given:
  num: Integer = 17
  result: Integer = 0

Step 1 → Validate input
  If: num < 2
    Then: result becomes -1
    Then: go to End
  Otherwise: go to Step 2

Step 2 → Process valid number
  Then: result becomes num * 2

End: Return result
```

**Why**: Error blocks are for **runtime exceptions** (network failures, file I/O errors), not logical flow or input validation

**When to use Error blocks**:
- File not found
- Network timeout
- Division by zero (automatic)
- JSON parse failure

**When NOT to use Error blocks**:
- Input validation
- Logical branches
- Early termination conditions

---

## Operator Mistakes

### Comparison Operators

**Wrong:**
```cns
If: x != 5      # Wrong operator
If: x <> 5      # Wrong operator  
If: x == 5      # Wrong operator
```

**Right:**
```cns
If: x = 5       # Equality
If: NOT (x = 5) # Inequality (verbose but works)
```

**CNS Comparison Operators:**
- `=` - Equal
- `>` - Greater than
- `<` - Less than
- `>=` or `≥` - Greater than or equal
- `<=` or `≤` - Less than or equal
- `NOT (x = y)` - Not equal (workaround)

**Note**: `!=` is not currently supported, use `NOT (x = y)` instead

---

### Arithmetic Operators

**All work as expected:**
```cns
Then: x becomes x + 1   # Addition
Then: x becomes x - 1   # Subtraction  
Then: x becomes x * 2   # Multiplication
Then: x becomes x / 2   # Division
Then: x becomes x % 3   # Modulo
```

**Watch out for:**
- Integer division: `5 / 2` might be `2` not `2.5` (depends on types)
- Modulo with negatives: behavior may vary

---

## Control Flow Mistakes

### 1. Control Flow Outside If/Otherwise ❌

**Wrong:**
```cns
Step 5 → Update counter
  Then: i becomes i + 1
  Then: go to End  # ← Parser ignores this!
```

**Right:**
```cns
Step 5 → Update counter
  Then: i becomes i + 1
  If: i > 10
    Then: go to End
```

**Why**: `go to` and `repeat from` are control flow statements that MUST be inside If/Otherwise blocks

---

### 2. Infinite Loop Protection ❌

**Wrong (will timeout):**
```cns
Step 1 → Loop forever
  Because: Demonstrating infinite loop
  Then: count becomes count + 1
  If: TRUE
    Then: repeat from Step 1
```

**Right:**
```cns
Step 1 → Loop with limit
  Because: Process until done
  Then: count becomes count + 1
  If: count < 100
    Then: repeat from Step 1
  Otherwise: go to End
```

**Why**: CNS has max iteration limits (default 10000) to prevent infinite loops

---

### 3. Go To Non-Existent Step ❌

**Wrong:**
```cns
Step 1 → Start
  If: done = TRUE
    Then: go to Step 5  # Step 5 doesn't exist!

End: Return result
```

**Right:**
```cns
Step 1 → Start
  If: done = TRUE
    Then: go to End

End: Return result
```

**Valid go to targets:**
- `go to Step N` where N exists
- `go to End`
- `go to Error` (if Error block exists)

---

## Variable Mistakes

### 1. Using Undeclared Variables ❌

**Wrong:**
```cns
Given:
  x: Integer = 5

Step 1 → Process
  Then: result becomes x * 2  # result not declared!
```

**Right:**
```cns
Given:
  x: Integer = 5
  result: Integer = 0

Step 1 → Process
  Then: result becomes x * 2
```

**Why**: All variables must be declared in `Given:` section before use

---

### 2. Type Mismatches ❌

**Wrong:**
```cns
Given:
  count: Integer = "five"  # String in Integer variable
  name: String = 42        # Number in String variable
```

**Right:**
```cns
Given:
  count: Integer = 5
  name: String = "Bob"
```

**Type coercion**: CNS does some automatic string conversion, but be explicit!

---

## String Mistakes

### 1. String Concatenation ❌

**Wrong:**
```cns
Then: message becomes "Hello" + name  # Works but...
Then: message becomes "Count: " + count  # Might not work for integers
```

**Right:**
```cns
Then: message becomes "Hello " + name
Effect: Print "Count: {count}"  # Use interpolation for mixed types
```

**Best Practice**: Use `{variable}` interpolation in Effect: Print statements

---

## Effect Pattern Mistakes

### 1. HTTP Effects

**Wrong:**
```cns
Effect: HTTP GET url into response  # Missing "from"
Effect: GET from url into response  # Missing "HTTP"
```

**Right:**
```cns
Effect: HTTP GET from url into response
Effect: HTTP POST to url with body body into response
```

---

### 2. File Effects

**Wrong:**
```cns
Effect: Write to file data  # Backwards
Effect: Append data file   # Missing "to"
Effect: WRITE "data" TO FILE variable  # Variable not supported
```

**Right:**
```cns
Effect: Write "data" to /tmp/file.txt
Effect: Append "more data" to /tmp/file.txt
Then: content becomes READ FROM FILE "/tmp/file.txt"
```

**Note**: File paths must be literal strings, not variables (current limitation)

---

### 3. Socket Effects

**Wrong:**
```cns
Effect: Create socket on port  # Missing socket variable name
Effect: Accept connection      # Missing "on server_socket"
```

**Right:**
```cns
Effect: Create socket server_socket on port
Effect: Accept connection on server_socket
Effect: Network read
Effect: Send response to client
Effect: Close connection client_socket
```

---

## Network Programming Mistakes

### 1. Special Network Variables

**Wrong:**
```cns
Given:
  REQUEST_METHOD: String = ""  # Don't declare these!
  REQUEST_PATH: String = ""

Step 1 → Read request
  Effect: Network read
  Then: method becomes REQUEST_METHOD  # This won't work if you declared it
```

**Right:**
```cns
Given:
  method: String = ""
  path: String = ""

Step 1 → Read request
  Effect: Network read
  Then: method becomes REQUEST_METHOD  # Auto-populated by Network read
  Then: path becomes REQUEST_PATH
```

**Auto-populated variables** (after `Network read`):
- `REQUEST_METHOD` - GET, POST, etc.
- `REQUEST_PATH` - The URL path
- `REQUEST_BODY` - POST data
- `REQUEST_HEADERS` - Header dict
- `REQUEST_QUERY` - Query string

**Don't declare these** - they're automatically created!

---

## JSON Mistakes

### 1. JSON Parsing

**Wrong:**
```cns
Then: data becomes PARSE JSON response  # Missing GET
Then: value becomes JSON GET response "key"  # Missing PARSE
```

**Right:**
```cns
Then: data becomes PARSE JSON response GET "key"
Then: nested becomes PARSE JSON response GET "user.name"  # Dot notation
```

---

### 2. JSON Generation

**Wrong (not yet supported):**
```cns
Then: json becomes JSON FROM dict  # Not implemented
```

**Right (manual):**
```cns
Then: json becomes "{\"name\":\"" + name + "\",\"age\":" + age + "}"
```

**Or use template:**
```cns
Given:
  json_template: String = "{\"name\":\"NAME\",\"age\":AGE}"

Step 1 → Build JSON
  Then: json becomes REPLACE json_template WITH "NAME" TO name
  Then: json becomes REPLACE json WITH "AGE" TO age
```

---

## List Mistakes

### 1. List Operations

**Wrong:**
```cns
Then: list becomes list + item  # Doesn't append
Then: first becomes list[0]     # No bracket notation
```

**Right:**
```cns
Effect: ADD item TO LIST list
Then: first becomes FIRST FROM list WHERE TRUE  # Get first item
Then: size becomes LENGTH OF list
```

---

## Advanced Mistakes

### 1. Function Definitions

**Wrong:**
```cns
Story: MyFunction
Given:
  param: Integer
  
# Missing (function) marker!
```

**Right:**
```cns
Story: MyFunction (function)
Given:
  param: Integer
  result: Integer = 0

Step 1 → Calculate
  Then: result becomes param * 2

End: Return result

---

Story: Main
Given:
  x: Integer = 5
  y: Integer

Step 1 → Call function
  Then: y becomes MyFunction(x)

End: Return y
```

**Key**: Functions need `(function)` marker in Story line

---

## Validation Warnings (False Positives)

**Current validator has known bugs** - these warnings can be ignored:

### 1. "Variable used before declaration" in Because clause

```
WARNING: Variable 'Move' used before declaration in Step 4
  Context: Move to the next potential factor
```

**Ignore this**: Validator treats "Because:" text as code. This is a validator bug.

### 2. "Unrecognized effect pattern" for valid effects

```
WARNING: Unrecognized effect pattern: Network read
```

**Ignore this**: Validator's effect pattern library is incomplete. If it runs, it's fine.

### 3. Missing step numbers

```
WARNING: Step 10 found, expected Step 9
```

**This might be real**: Check if you skipped a step number or used `go to Step 10` correctly.

---

## Testing Your CNS Code

### Validation

```bash
./cns-validate your-program.cns
```

**Ignore**:
- "Variable used before declaration" in Because clauses
- "Unrecognized effect pattern" for documented effects

**Fix**:
- Actual syntax errors
- Undefined variables in expressions
- Missing step numbers (if you didn't use go to)

### Execution

```bash
./cns-run your-program.cns
```

**Common runtime errors:**
- "Variable X is not defined" → Add to Given section
- "go to Step N" errors → Check step exists
- Infinite loop timeout → Add iteration limit check
- Network/File errors → Check paths and permissions

---

## Quick Reference: CNS vs Other Languages

| Feature | Python | JavaScript | CNS |
|---------|--------|------------|-----|
| **Comparison** | `==` | `===` | `=` |
| **Not Equal** | `!=` | `!==` | `NOT (x = y)` |
| **Boolean** | `True` | `true` | `TRUE` |
| **Assignment** | `x = 5` | `x = 5` | `x becomes 5` |
| **If/Else** | `if:/else:` | `if {}/else {}` | `If:/Otherwise:` |
| **While** | `while:` | `while {}` | `If: repeat from` |
| **Print** | `print()` | `console.log()` | `Effect: Print` |
| **Return** | `return x` | `return x` | `End: Return x` |

---

## Summary: Top 10 Mistakes

1. ❌ Using `==` instead of `=`
2. ❌ Using `True` instead of `TRUE`
3. ❌ Multi-action Then clauses
4. ❌ Semantic tags in declarations
5. ❌ Error blocks for validation
6. ❌ Undeclared variables
7. ❌ Control flow outside If blocks
8. ❌ Declaring auto-populated network variables
9. ❌ Using `!=` instead of `NOT (x = y)`
10. ❌ File paths as variables instead of literals

---

## Need Help?

- **Syntax Reference**: `docs/language/SYNTAX.md`
- **Common Patterns**: `docs/language/COMMON-PATTERNS.md`
- **Expression Rules**: `docs/language/EXPRESSION-LIMITATIONS.md`
- **Control Flow**: `docs/language/CONTROL-FLOW-RULES.md`
- **LLM Guide**: `docs/guides/LLM-INTEGRATION.md`

---

**Remember**: When in doubt, check the examples! The `examples/` directory has 90+ working CNS programs.
