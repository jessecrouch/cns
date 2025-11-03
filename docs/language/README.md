# CNS Language Documentation

This directory contains human-readable CNS language documentation.

## For LLMs

**LLMs should use [prompts/detailed-template.md](../../prompts/detailed-template.md)** instead.

That file is the single comprehensive reference optimized for machine code generation.

## For Humans

These files provide detailed explanations of CNS language features:

### Core Language

- **[SYNTAX.md](SYNTAX.md)** - Complete syntax reference
  - Story structure, variables, expressions, control flow
  - Effects (I/O operations)
  - Functions and error handling
  - Limitations and workarounds

### Specific Topics

- **[EXPRESSION-LIMITATIONS.md](EXPRESSION-LIMITATIONS.md)** - What works and what doesn't
  - Variable-first rule (`x + 5` not `5 + x`)
  - Single operator per expression
  - No parentheses for grouping
  - Workarounds and patterns

- **[CONTROL-FLOW-RULES.md](CONTROL-FLOW-RULES.md)** - Loops, jumps, and conditionals
  - If/Otherwise patterns
  - Repeat from loops
  - Go to jumps
  - Control flow must be in If blocks

- **[COMMON-PATTERNS.md](COMMON-PATTERNS.md)** - Reusable code patterns
  - Read-Process-Write
  - API-Parse-Store
  - Validate-Process-Return
  - Loop-Accumulate
  - HTTP servers with routing
  - Database operations

- **[LLM-COMMON-MISTAKES.md](LLM-COMMON-MISTAKES.md)** - Known LLM errors
  - Using `==` instead of `=`
  - Using `True`/`False` instead of `TRUE`/`FALSE`
  - Control flow outside If blocks
  - Functions that don't exist
  - Expression mistakes

## Quick Links

- **Examples**: [examples/](../../examples/) - 30+ working programs
- **LLM Template**: [prompts/detailed-template.md](../../prompts/detailed-template.md)
- **Quick Start**: [QUICKSTART.md](../../QUICKSTART.md)

---

**Philosophy**: CNS is LLM-first. These docs are supplementary for human readers.
