# CNS: Causal Narrative Script

**A programming language optimized for Large Language Model comprehension**

CNS (Causal Narrative Script) is a novel programming language designed to make code more comprehensible for LLMs than traditional languages. It emphasizes explicit causality, narrative flow, and self-documenting structures to align with LLM strengths in structured reasoning and pattern matching.

## Core Philosophy

- **Narrative Flow**: Code reads like a story with `Story:`, `Given:`, `Step:`, and `End:` sections
- **Causality First**: Every transformation includes mandatory `Because:` clauses explaining the reasoning
- **Explicit Everything**: State transitions, effects, and conditions are declared upfront—no hidden magic
- **LLM-Friendly**: Linear structure, semantic tags on variables, and pattern-based logic mirror LLM thought processes

## Example

Here's a factorial computation in CNS:

```cns
Story: Compute factorial of a positive integer

Given:
  n: Integer = 5
  result: Integer = 1

Step 1 → Multiply result by n
  Because: n contributes to the product
  Then: n becomes n - 1

Step 2 → If n > 1
  Because: we need to include all integers down to 1
  Then: repeat from Step 1
  Otherwise: go to End

End: Return result
```

This executes to compute 5! = 120, with traceable reasoning at every step.

## Why CNS?

LLMs excel at:
- ✅ Causal, step-by-step reasoning
- ✅ Pattern matching and structured text
- ✅ Explaining "why" something happens

LLMs struggle with:
- ❌ Implicit control flow
- ❌ Hidden side effects
- ❌ Ambiguous logic

CNS is designed around LLM strengths, making it ideal for:
- LLM-generated code
- Automated debugging and reasoning
- Collaborative human-AI programming
- Educational tools for AI reasoning

## Installation

### Prerequisites

- Common Lisp implementation (SBCL, CCL, or similar)
- Git

### Quick Start

```bash
# Clone the repository
git clone <repository-url>
cd cns

# Load the interpreter in your Lisp REPL
sbcl --load cns.lisp

# Or run tests
sbcl --load test-runner.lisp --eval "(run-all-tests)" --quit
```

## Usage

### Quick Start (Recommended)

Use the included `cns-run` script for the easiest experience:

```bash
# Run a specific example
./cns-run examples/factorial.cns

# List all available examples
./cns-run --list

# Run all examples
./cns-run --all

# Start interactive REPL
./cns-run --repl

# Run test suite
./cns-run --test

# Get help
./cns-run --help
```

### Available Examples

```bash
$ ./cns-run --list

Available CNS examples:

  Basic Algorithms:
    collatz.cns          Count steps in the Collatz conjecture sequence
    digit-sum.cns        Calculate the sum of digits in a number
    factorial.cns        Compute factorial of a positive integer
    fibonacci.cns        Calculate the nth Fibonacci number
    gcd.cns              Find the greatest common divisor
    is-prime.cns         Check if a number is prime
    power.cns            Calculate base raised to exponent
    simple-counter.cns   Count from 1 to 5
    sum-numbers.cns      Calculate sum of numbers
    sum-range.cns        Sum all integers from start to end

  Feature Demonstrations:
    boolean-demo.cns     Boolean logic operators (AND, OR, NOT)
    combined-features.cns All features in one example
    file-demo.cns        File I/O operations
    halve.cns            Division operator demonstration
    is-even.cns          Modulo operator for even/odd checking
    list-demo.cns        List operations (create, length, index)
    print-demo.cns       Print with variable interpolation
  
  Real-World Applications:
    filter-numbers.cns   Filter list by threshold (data processing)
    sales-report.cns     Generate quarterly sales report (file I/O)
    simple-webserver.cns Basic HTTP server with socket operations
    webserver-routing.cns Webserver with multiple routes and 404 handling
    word-stats.cns       Analyze test scores with statistics
```

### Advanced Usage

#### Option 1: From Lisp REPL

```lisp
(load "cns.lisp")

;; Parse and run CNS code directly
(let* ((code "Story: Hello CNS
Given:
  x: Integer = 42
End: Return x")
       (ast (parse-cns code)))
  (interpret-cns ast))

;; Load and run a CNS file
(load-cns-file "examples/factorial.cns")
```

#### Option 2: Interactive REPL

```bash
./cns-run --repl
```

Or manually:
```lisp
(load "cns.lisp")
(cns-repl)
```

Then enter CNS code, ending multi-line input with a single `.` on its own line.

### Running Tests

```bash
./cns-run --test
```

Or manually:
```bash
sbcl --load test-runner.lisp --eval "(run-all-tests)" --quit
```

## Project Structure

```
cns/
├── AGENTS.md           # Detailed guide for AI agents developing CNS
├── README.md           # This file
├── cns.lisp            # Core interpreter and parser
├── test-runner.lisp    # Testing framework
└── examples/           # Example CNS programs
    ├── factorial.cns
    ├── simple-counter.cns
    └── sum-numbers.cns
```

## Language Features

### Current Features

**Core Structure:**
- ✅ Narrative structure (`Story:`, `Given:`, `Step:`, `End:`)
- ✅ Variable declarations with types and semantic tags
- ✅ Explicit causality (mandatory `Because:` clauses)
- ✅ State transformations (`Then:` clauses, including multiple per step)
- ✅ Conditional logic (`If`, `Otherwise`)
- ✅ Loops via step jumps (`repeat from Step N`)
- ✅ Execution tracing with causality explanations
- ✅ Enhanced error messages with context

**Data Types:**
- ✅ Integers with full arithmetic (`+`, `-`, `*`, `/`, `%`)
- ✅ Strings with literal support (`"text"`)
- ✅ Lists/arrays (`[1, 2, 3]`)
- ✅ Booleans (implicit in conditions)

**Operators:**
- ✅ Arithmetic: `+`, `-`, `*`, `/` (floor division), `%` (modulo)
- ✅ Comparisons: `<`, `>`, `=`, `≤`, `≥`, `≠`
- ✅ Boolean logic: `AND`, `OR`, `NOT`
- ✅ List operations: `length of`, `at index`

**I/O and Effects:**
- ✅ Print with variable interpolation (`Effect: Print "Value: {x}"`)
- ✅ File writing (`Effect: Write "data" to file.txt`)
- ✅ File appending (`Effect: Append "data" to file.txt`)
- ✅ Variable substitution in effects (`{varname}`)
- ✅ Socket operations (`Create socket`, `Accept connection`, `Send`, `Close socket`)
- ✅ Network effects (`Network read`, `Network write`)
- ✅ Logging (`Effect: Log "message"`)

**Control Flow:**
- ✅ Conditional jumps to specific steps (`Otherwise: go to Step 5`)
- ✅ Loop controls (`repeat from Step N`)
- ✅ Direct step navigation
- ✅ Error handling blocks (`Error:` section)
- ✅ Automatic error recovery with error effects

### Planned Features

- 🔜 Functions/reusable stories
- 🔜 Read from files (not just write)
- 🔜 More list operations (append, filter, map)
- 🔜 Full type validation for semantic tags
- 🔜 Module/story composition
- 🔜 Pattern matching for conditionals
- 🔜 Parentheses for expression grouping
- 🔜 LLM-based causality validation
- 🔜 Compilation to native Lisp

See `AGENTS.md` for the complete roadmap.

## Language Syntax

### Story Header
```cns
Story: <narrative description of what this code does>
```

### Variable Declarations
```cns
Given:
  <name>: <Type> [<semantic-tag>] = <initial-value>

Examples:
  count: Integer = 0
  name: String = "Alice"
  items: List = [1, 2, 3]
  threshold: Integer [≥ 0] = 10
```

### Steps
```cns
Step N → <action description>
  Because: <causal explanation>
  Then: <state transformation>
  Then: <another transformation>  # Multiple Then clauses supported
  Effect: <side effect description>

Examples:
  Step 1 → Calculate total
    Because: we need the sum of all values
    Then: total becomes x + y + z
    Effect: Print "Total is {total}"
```

### Conditionals
```cns
Step N → If <condition>
  Because: <reason for this check>
  Then: <action if true>
  Otherwise: <action if false>

Conditions support:
  - Comparisons: <, >, =, ≤, ≥, ≠
  - Boolean logic: AND, OR, NOT
  - Examples: "x > 5", "x = 0 OR y < 10", "NOT done"
```

### Control Flow
```cns
# Loop back to earlier step
Then: repeat from Step N

# Jump to specific step
Otherwise: go to Step N

# Exit to end
Otherwise: go to End
```

### Effects (I/O and Side Effects)
```cns
# Print to console with variable interpolation
Effect: Print "The value of x is {x}"

# Write to file
Effect: Write "Some data: {result}" to output.txt

# Append to file
Effect: Append "Log entry: {message}" to log.txt
```

### Operators

**Arithmetic:**
- `+` Addition
- `-` Subtraction
- `*` Multiplication
- `/` Floor division
- `%` Modulo (remainder)

**Comparisons:**
- `<` Less than
- `>` Greater than
- `=` Equal to
- `≤` Less than or equal
- `≥` Greater than or equal
- `≠` Not equal

**Boolean Logic:**
- `AND` Logical AND
- `OR` Logical OR
- `NOT` Logical NOT

**List Operations:**
- `length of <list>` Get list size
- `<list> at <index>` Get element at index (0-based)

### Error Handling
```cns
Error:
  Return <error-value>
  Effect: <error-side-effect>
  Because: <error-reasoning>

Example:
Error:
  Return "Server error"
  Effect: Log "Error in webserver"
  Because: Handle unexpected failures gracefully
```

The Error: block is optional and executes if any step throws an error during execution.

### End Section
```cns
End: Return <value>
```

## Contributing

We welcome contributions! Key areas:

1. **Parser improvements**: Better error messages, support for more syntax
2. **Interpreter features**: Enhanced expression evaluation, effects system
3. **Language features**: Modules, pattern matching, type validation
4. **Documentation**: More examples, tutorials, language specification
5. **Testing**: Expanded test suite, edge cases

See `AGENTS.md` for detailed guidance on extending CNS.

## Design Principles

1. **Clarity over Performance**: Prioritize readability and LLM comprehension
2. **Explicit Causality**: Every transformation must explain "why"
3. **Narrative Structure**: Code should read like a story
4. **Traceable Execution**: Full execution traces with reasoning
5. **LLM-First**: Optimize for LLM generation and understanding, not human handwriting

## Use Cases

- **LLM Code Generation**: Models can generate CNS more reliably than Python/Java
- **Automated Debugging**: Causal traces make debugging transparent
- **AI-Driven Development**: Intermediate representation for AI coding assistants
- **Educational Tools**: Teaching AI reasoning and program logic
- **Collaborative Coding**: Bridge between human intent and machine execution

## License

[To be determined - suggest MIT or Apache 2.0]

## Acknowledgments

CNS was designed as a thought experiment in making programming languages more comprehensible to Large Language Models, prioritizing explicit causality and narrative structure over traditional programming paradigms.

---

**Let's build the future of LLM-friendly programming!**
