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

- ✅ Narrative structure (`Story:`, `Given:`, `Step:`, `End:`)
- ✅ Variable declarations with types and semantic tags
- ✅ Explicit causality (`Because:` clauses)
- ✅ State transformations (`Then:` clauses)
- ✅ Conditional logic (`If`, `Otherwise`)
- ✅ Loops via step jumps (`repeat from Step N`)
- ✅ Basic arithmetic and comparisons
- ✅ Execution tracing with causality explanations

### Planned Features

- 🔜 Enhanced error reporting with line numbers
- 🔜 Causal arrow operators in actions (`A → B`)
- 🔜 Full type validation for semantic tags
- 🔜 Effect tracking for side effects
- 🔜 Module/story composition
- 🔜 Pattern matching for conditionals
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
```

### Steps
```cns
Step N → <action description>
  Because: <causal explanation>
  Then: <state transformation>
  Effect: <side effect description>
```

### Conditionals
```cns
Step N → If <condition>
  Because: <reason for this check>
  Then: <action if true>
  Otherwise: <action if false>
```

### Loops
```cns
Then: repeat from Step N
```

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
