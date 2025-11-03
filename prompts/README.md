# CNS LLM Prompts

This directory contains the **canonical CNS reference for LLM code generation**.

## Primary Template

**[detailed-template.md](detailed-template.md)** - Complete CNS reference optimized for LLMs

This single file contains:
- Quick reference function lookup table (what exists, what doesn't)
- Validation self-check checklist (catch errors before submission)
- Complete syntax reference with ✅/❌ examples
- Built-in variables and functions (REQUEST_METHOD, TIMESTAMP, etc.)
- Expression rules (variable-first, no multi-operator, etc.)
- Control flow patterns (If/Otherwise, loops, jumps)
- All effects (Print, HTTP, Files, Network, Database)
- Common patterns (loops, validation, API calls, servers)
- Complete worked examples

## Usage

Replace `{TASK}` with your task description and send the entire template to your LLM.

```markdown
Task: Create a web server on port 8080 that responds "Hello"

[Rest of detailed-template.md content...]
```

The LLM will generate syntactically correct CNS code.

## Design Philosophy

**LLM-First**: This template is optimized for machine comprehension, not humans.

Key principles:
1. **Single source of truth** - Everything in one file
2. **Show, don't tell** - ✅/❌ examples over prose
3. **Negative warnings** - Explicitly state what DON'T exist
4. **Self-validation** - Checklist for LLMs to verify their output
5. **Quick lookup** - Function table at top for instant reference

## Success Metrics

Using this template, Grok-2 achieved:
- **100% validation pass rate** on complex network programming
- **0 syntax errors** on first generation attempt
- **0 false positive warnings** from validator

## For Humans

If you're a human developer, see:
- [examples/](../examples/) - 30+ working CNS programs
- [docs/language/SYNTAX.md](../docs/language/SYNTAX.md) - Human-friendly syntax guide
- [QUICKSTART.md](../QUICKSTART.md) - Getting started tutorial

## Files

- **detailed-template.md** - Main LLM template (use this!)
- **quick-template.md** - Deprecated (use detailed-template.md instead)
- **README.md** - This file

---

Last updated: 2025-11-03
