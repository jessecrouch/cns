# CNS Quick Start Guide

Welcome to Causal Narrative Script! Here's how to get started in 5 minutes.

## 1. Check Installation

Make sure you have SBCL (Steel Bank Common Lisp):
```bash
sbcl --version
```

## 2. Run Your First Program

```bash
./cns-run examples/factorial.cns
```

You should see:
```
=== Executing Story: Compute factorial of a positive integer ===
...
Return: 120
```

## 3. Try More Examples

See what's available:
```bash
./cns-run --list
```

Run them all:
```bash
./cns-run --all
```

## 4. Write Your Own Program

### Example 1: Simple Math
Create a file `my-program.cns`:

```cns
Story: Double a number

Given:
  x: Integer = 21

Step 1 → x becomes x + x
  Because: we want to double the value

End: Return x
```

Run it:
```bash
./cns-run my-program.cns
```

### Example 2: Using Print and Strings
```cns
Story: Greet a user

Given:
  name: String = "Alice"
  age: Integer = 25

Step 1 → Show greeting
  Because: we want to welcome the user
  Effect: Print "Hello, {name}! You are {age} years old."

End: Return name
```

### Example 3: Working with Lists
```cns
Story: Count items in a shopping list

Given:
  items: List = ["milk", "bread", "eggs"]
  count: Integer = 0

Step 1 → count becomes length of items
  Because: we need to know how many items there are
  Effect: Print "You have {count} items in your cart"

End: Return count
```

### Example 4: File Output
```cns
Story: Generate a simple report

Given:
  total: Integer = 150
  status: String = "complete"

Step 1 → Write report header
  Because: we need to document the results
  Effect: Write "Report Status: {status}" to report.txt
  Effect: Append "Total Amount: {total}" to report.txt

End: Return total
```

## 5. Use the Interactive REPL

Start the REPL:
```bash
./cns-run --repl
```

Type your program, end with `.` on a line by itself, then see it run!

## Common Commands

| Command | What It Does |
|---------|-------------|
| `./cns-run file.cns` | Run a CNS file |
| `./cns-run --list` | List all examples |
| `./cns-run --all` | Run all examples |
| `./cns-run --repl` | Interactive mode |
| `./cns-run --test` | Run test suite |
| `./cns-run --help` | Show help |

## CNS Syntax Cheat Sheet

```cns
Story: Brief description of what this program does

Given:
  variable_name: Type = initial_value
  text_var: String = "hello"
  list_var: List = [1, 2, 3]

Step 1 → Action description
  Because: Why this step is necessary
  Then: variable becomes new_value
  Then: another_var becomes variable + 10
  Effect: Print "Current value: {variable}"

Step 2 → If condition AND other_condition
  Because: Why we check this condition
  Then: repeat from Step 1
  Otherwise: go to Step 3

Step 3 → Final processing
  Because: we need to complete the task
  Effect: Write "Result: {variable}" to output.txt

End: Return variable_name
```

## Key Features

**Core Elements:**
- **Because:** - Every step explains WHY (mandatory!)
- **Then:** - Shows what changes (can have multiple)
- **If/Otherwise:** - Branching logic with boolean operators
- **repeat from Step N** - Loops
- **Arrow (→)** - Separates step number from action

**Data Types:**
- **Integer** - Whole numbers
- **String** - Text in quotes: `"hello"`
- **List** - Arrays: `[1, 2, 3]`

**Operators:**
- Math: `+`, `-`, `*`, `/`, `%`
- Compare: `<`, `>`, `=`, `≤` (or `<=`), `≥` (or `>=`), `≠`
- Logic: `AND`, `OR`, `NOT`
- Lists: `length of list`, `list at 0`

**Effects (I/O):**
- `Effect: Print "text with {variable}"`
- `Effect: Write "data" to file.txt`
- `Effect: Append "more" to file.txt`

## Tips

1. **Start Simple** - Begin with the examples
2. **Be Explicit** - CNS values clarity over brevity
3. **Explain Causality** - The "Because:" is what makes CNS special
4. **Watch It Run** - The verbose output helps you understand execution
5. **Think Narratively** - Write code like you're telling a story

## Common Mistakes

### ❌ Wrong: Multi-line End block
```cns
End:
  Return result
  Because: Done
```

### ✅ Correct: Single-line End
```cns
End: Return result
```

### ❌ Wrong: Inline conditional action
```cns
Step 2 → If n > 1, repeat from Step 1
```

### ✅ Correct: Separate Then clause
```cns
Step 2 → If n > 1
  Then: repeat from Step 1
```

## Success Metrics

CNS has been validated with LLM testing:
- ✅ 100% parse success rate (10/10 tests)
- ✅ 100% execution success (5/5 tested programs)
- ✅ Average 1.2 iterations to working code
- ✅ Clear, traceable causality in all generated code

See `tests/llm-tests/TEST-RESULTS.md` for detailed analysis.

## Next Steps

1. Read `examples/README.md` for detailed explanations
2. Modify existing examples to see how they work
3. Check out `AGENTS.md` for the full language design
4. Write your own algorithms!

## Getting Help

- Run `./cns-run --help` for command options
- Check the examples directory for patterns
- Read the main README.md for complete documentation

Happy coding! Remember: In CNS, every line tells a story. 📖✨
