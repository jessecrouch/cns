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

## Agent Architecture & Best Practices

### Core Principle: CNS as Orchestration Language

**CNS is the conductor, not the orchestra.**

Like Kubernetes YAML orchestrates containers without implementing them, CNS orchestrates intelligent components without implementing complex algorithms. This keeps CNS:
- **Token-efficient**: Compact representation for LLM context windows
- **LLM-readable**: Clear causal narrative of agent behavior  
- **Focused**: Does one thing well - orchestration

### What CNS Excels At

✅ **Narrative Flow** - "Here's what happens and why" with explicit causality  
✅ **Tool Orchestration** - SHELL, FIND, GREP, HTTP, File I/O  
✅ **Conditional Logic** - If/Otherwise with clear branching  
✅ **State Management** - Variables, lists, file persistence  
✅ **Story Composition** - Reusable functions via multi-story files

### What External Tools Should Handle

❌ **Complex Algorithms** - Use Python/Rust/Go for performance-critical logic  
❌ **LLM Inference** - Call Python scripts with OpenAI/Anthropic APIs  
❌ **Language-specific Parsing** - Use tree-sitter, rustc, go vet  
❌ **Heavy Computation** - Delegate to compiled languages

### Story Composition Pattern

CNS supports **multi-story files** with `---` separator. Stories can call each other like functions:

```cns
Story: Square (function)
Given:
  Num: Integer = 0
  Result: Integer = 0

Step 1 → Calculate square
  Because: Multiply number by itself
  Then: Result becomes Num * Num

End: Result

---

Story: Main
Given:
  x: Integer = 5
  result: Integer = 0

Step 1 → Calculate
  Then: result becomes Square(x)

End: result
```

**Key Features:**
- `---` separates Stories in same file
- Stories can call each other: `Square(x)`
- Return value via `End: variable`
- Registered as "functions" at parse time

### Modular Agent Design

**Recommended Structure:**
```
cns-agents/
├── core/
│   ├── language-detector.cns    # Atomic: Detect repo language
│   ├── test-runner.cns          # Atomic: Execute tests
│   ├── code-searcher.cns        # Atomic: FIND + GREP wrapper
│   └── patch-applier.cns        # Atomic: Git operations
├── workflows/
│   ├── swe-bench-rust.cns       # Compose: Rust workflow
│   ├── swe-bench-go.cns         # Compose: Go workflow
│   └── swe-bench-multi.cns      # Compose: Multi-language
└── lib/
    └── cns-stdlib.cns           # Reusable utilities
```

**Each Core Component:**
- Single responsibility
- Returns simple data (string, number, list)
- Stateless (except via ENV or file state)
- Callable from other Stories
- Uses SHELL for external tools

**Example: Modular Language Detector**
```cns
Story: DetectLanguage (function)
Given:
  repo_path: String
  result: String = ""

Step 1 → Check Rust
  Effect: SHELL "test -f {repo_path}/Cargo.toml && echo rust || echo ''" INTO result WITH EXIT_CODE code

Step 2 → Check Go if not found
  If: result = ""
    Effect: SHELL "test -f {repo_path}/go.mod && echo go || echo ''" INTO result WITH EXIT_CODE code

Step 3 → Check JavaScript if not found
  If: result = ""
    Effect: SHELL "test -f {repo_path}/package.json && echo javascript || echo ''" INTO result WITH EXIT_CODE code

End: result

---

Story: Main
Given:
  repo: String = "test-repos/rust-example"
  lang: String = ""

Step 1 → Detect language
  Then: lang becomes DetectLanguage(repo)
  Effect: Print "Detected: {lang}"

End: lang
```

### Data Interchange Between Components

Since CNS has limited data structures, use **JSON** for complex data:

```cns
Story: GetConfig (function)
Given:
  repo: String
  config_json: String = ""

Step 1 → Build JSON config
  Then: lang becomes DetectLanguage(repo)
  Then: config_json becomes "{\"language\":\"" + lang + "\",\"test_cmd\":\"cargo test\"}"

End: config_json

---

Story: UseConfig
Given:
  config: String = ""
  language: String = ""

Step 1 → Get config
  Then: config becomes GetConfig("my-repo")

Step 2 → Parse language
  Then: language becomes PARSE JSON config GET "language"

End: language
```

### Available CNS Features

**Effects:**
- `Print "text {var}"` - Output with variable substitution
- `SHELL "cmd" INTO var WITH EXIT_CODE code` - Execute command
- `FIND "pattern" IN "path" INTO var WITH COUNT count` - File discovery
- `GREP "pattern" IN var INTO result` - Code search
- `READ FROM FILE "path"` - Read file content
- `Write "content" to /path/to/file` - Write file
- `ADD item TO LIST list` - Append to list
- `CSV WRITE data TO file WITH HEADERS headers` - Write CSV
- `HTTP GET "url" INTO var` - Fetch HTTP

**Data Types:**
- `String` - Text values
- `Integer` / `Number` - Numeric values
- `Boolean` - TRUE/FALSE
- `List` - Arrays (use `ADD`, `REMOVE`, `LENGTH OF`)

**Expressions:**
- Arithmetic: `+`, `-`, `*`, `/`
- Comparison: `=`, `<`, `>`, `<=`, `>=`
- String concat: `+` (e.g., `"Hello " + name`)
- JSON parsing: `PARSE JSON json GET "key.nested[0]"`
- ENV vars: `ENV("VAR_NAME", "default")`

**Control Flow:**
- `If: condition` / `Otherwise:` - Branching
- `repeat from Step N` - Loops
- `go to End` - Early exit

### Multi-Step Agent Example

Complete agent that orchestrates external tools:

```cns
Story: SWE-bench Agent
Given:
  repo_path: String = "test-repos/rust-example"
  language: String = ""
  test_output: String = ""
  exit_code: Number = 0
  patch_needed: Boolean = FALSE

Step 1 → Detect language
  Effect: SHELL "test -f {repo_path}/Cargo.toml && echo rust || echo unknown" INTO language WITH EXIT_CODE code
  Effect: Print "Language: {language}"

Step 2 → Run tests
  If: language = "rust"
    Effect: SHELL "cd {repo_path} && cargo test --color=never" INTO test_output WITH EXIT_CODE exit_code

Step 3 → Analyze results
  If: exit_code = 0
    Effect: Print "Tests passing - no fix needed"
    Then: patch_needed becomes FALSE
  Otherwise:
    Effect: Print "Tests failing - generating patch"
    Then: patch_needed becomes TRUE

Step 4 → Generate patch if needed
  If: patch_needed = TRUE
    Effect: SHELL "python3 llm-patcher.py --repo {repo_path} --output patch.txt" INTO result WITH EXIT_CODE code
    Effect: SHELL "cd {repo_path} && git apply ../patch.txt" INTO output WITH EXIT_CODE status

End: patch_needed
```

### Future Enhancements (Roadmap)

**Short-term:**
1. **Map/Dictionary Type** - Key-value data structures
2. **Include Directive** - Load external CNS files
3. **State Persistence** - Auto-save/load variables between runs

**Medium-term:**
4. **CNS Package Manager** - `cns-pm install language-detector`
5. **Story Registry** - Discover reusable components
6. **Native JSON Objects** - First-class object syntax

**Long-term:**
7. **LLM Integration** - Built-in OpenAI/Anthropic effects
8. **Streaming Execution** - Real-time agent monitoring
9. **Multi-agent Coordination** - Agent-to-agent messaging

### Quick Reference
- **Implementation**: `src/cns.lisp` - full interpreter code
- **Examples**: `examples/` - working CNS programs (factorial, fibonacci, webserver, etc.)
- **Prompts**: `prompts/` - templates for LLM code generation
- **Tests**: `tests/` - comprehensive testing infrastructure
- **Testing Guide**: `docs/development/TESTING.md` - validation and regression testing
- **Roadmap**: `docs/development/ROADMAP.md` - strategic direction and milestones
- **Agent Examples**: `examples/swe-bench-agent.cns`, `examples/language-detect-simple.cns`

For extending beyond Lisp (e.g., Python port), mirror the structure: Parser to AST (dicts/classes), interpreter as a class with state.

Let's build the future of LLM-friendly programming!
