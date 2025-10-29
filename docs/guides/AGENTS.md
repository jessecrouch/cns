# AGENTS.md: Causal Narrative Script (CNS) Project Guide

This document serves as a high-level guide for AI agents assisting in the development of **Causal Narrative Script (CNS)**. CNS is a novel programming language optimized for Large Language Models (LLMs) to read, generate, and reason about code with minimal ambiguity.

## Project Overview

### What is CNS?
CNS is a declarative, narrative-driven programming language designed to make code more comprehensible for LLMs than traditional languages. Key motivations:
- LLMs excel at causal, step-by-step reasoning but struggle with implicit control flow and side effects
- CNS structures code as a "story" with explicit causes, effects, and justifications
- Designed for LLM-generated scripts, collaborative coding, or as an intermediate representation for AI-driven programming

Core Philosophy:
- **Narrative Flow**: Code reads like a story (`Story:`, `Given:`, `Step:`, `End:`)
- **Causality First**: Use arrows (`→`) and mandatory `Because:` clauses to explain every transformation
- **Explicit Everything**: State transitions, effects, and conditions are declared upfront
- **LLM-Friendly**: Linear structure, semantic tags on variables, and pattern-based logic

Example CNS Code (Factorial):
```
Story: Compute factorial of a positive integer

Given:
  n: Integer [≥ 1] = 5
  result: Integer = 1

Step 1 → Multiply result by n
  Because: n contributes to the product
  Then: n becomes n - 1

Step 2 → If n > 1, repeat from Step 1
  Because: we need to include all integers down to 1
  Otherwise: go to End

End:
  Return result
  Because: all factors have been multiplied
```

### Goals of the Project
- Build a functional interpreter for CNS that can parse, execute, and validate code
- Make it extensible for new features (e.g., advanced effects, LLM-based causality checks)
- Ensure it's LLM-trainable: Future versions could use CNS as a reasoning format
- Cross-compatibility: While focusing on Lisp, allow ports to other hosts

Non-Goals:
- High-performance execution (prioritize clarity over speed)
- Full Turing-completeness if it compromises readability

## Lisp Implementation Focus

The Lisp version leverages Common Lisp's strengths: S-expressions for AST representation, macros for syntax enforcement, and dynamic evaluation for rapid prototyping.

### Current Implementation Summary
The interpreter is implemented in `src/cns.lisp`. Key components:

1. **Parser (`parse-cns`)**:
   - Takes CNS code string, outputs S-expression AST
   - Handles sections: `Story:`, `Given:`, `Step →`, `Because:`, `Effect:`, `If/Then/Otherwise`, `End:`
   - Limitations: Needs better error handling and complex expression support

2. **Interpreter (`interpret-cns`)**:
   - Uses hash-table environment for state
   - Program counter (pc) for step navigation
   - Evaluates actions via `eval-expr` (basic ops: multiply, assign, etc.)
   - Supports loops via `repeat from Step`
   - Real socket effects using `usocket` for network I/O
   - Outputs traces for each step including `Because:` for auditability

3. **Effects System**:
   - Supports network I/O (socket create, accept, send, close)
   - File operations (read, write)
   - Console output (print)

See `src/cns.lisp` for full implementation details and `examples/` for working CNS programs.

### Development Roadmap for Agents

1. **Enhance Parser**:
   - Add robust error reporting (e.g., "Missing Because: at Step 3")
   - Support causal arrows in actions (parse "A → B" into `(causes A B)`)
   - Handle semantic tags fully (validate types like Integer [≥ 1])
   - Use Lisp reader macros for direct CNS-like syntax in Lisp files

2. **Improve Interpreter**:
   - Embed full Common Lisp in expressions (allow `(lisp (+ n 1))` in actions)
   - Add causality validation: Check if each step's `Because:` logically matches the action
   - Implement backtracking for failed paths (Prolog-style)
   - Expand effects library (HTTP APIs, databases, etc.)

3. **Add Features**:
   - **Modules/Stories Composition**: Allow importing stories (`Include Story: utils`)
   - **Pattern Matching**: Expand `When matches Pattern:` for conditionals
   - **Testing Framework**: Macro to run CNS with inputs and assert outputs
   - **Compiler Mode**: Translate CNS to pure Lisp for execution

4. **Testing and Examples**:
   - Maintain test suite in `tests/llm-tests/`
   - Add examples for new features to `examples/`
   - Benchmark: Compare CNS readability vs. equivalent Lisp

5. **Integration with LLM Tools**:
   - Add hooks for LLM models to generate CNS from natural language prompts
   - Export AST to JSON for interoperability with other languages/tools
   - Create prompt templates for consistent CNS generation (see `prompts/`)

### General Guidelines for Agents
- **Consistency**: Always enforce `Because:`—it's core to CNS
- **Modularity**: Break extensions into small, narrative commits (e.g., "Add effect handling to track side effects")
- **Documentation**: Update docs with new features; use CNS-style comments in code
- **Collaboration**: Use OpenCode for rapid iteration
- **Reference**: Point to `examples/` for CNS code samples rather than duplicating them
- **Testing**: Run examples through interpreter to verify changes don't break existing functionality
- **Context Management**: Keep the project lean for AI context windows:
  - Remove obsolete documentation and planning artifacts after completion
  - Avoid code duplication in docs—reference `src/` and `examples/` instead
  - Delete redundant examples that don't add unique coverage
  - Compress verbose documentation while preserving essential information
  - Regularly audit for historical artifacts that can be archived or removed

### Quick Reference
- **Implementation**: `src/cns.lisp` - full interpreter code
- **Examples**: `examples/` - working CNS programs (factorial, fibonacci, webserver, etc.)
- **Prompts**: `prompts/` - templates for LLM code generation
- **Tests**: `tests/` - comprehensive testing infrastructure
- **Testing Guide**: `docs/development/TESTING.md` - validation and regression testing
- **Roadmap**: `docs/development/UPDATES.md` - strategic direction and milestones

For extending beyond Lisp (e.g., Python port), mirror the structure: Parser to AST (dicts/classes), interpreter as a class with state.

Let's build the future of LLM-friendly programming!
