# CNS Repository Structure

## Directory Layout

```
cns/
├── README.md                   # Main project documentation
├── QUICKSTART.md              # Quick start guide
├── STRUCTURE.md               # This file - explains repository layout
├── cns-run                    # Wrapper script to run CNS files
├── test-input.txt             # Sample test data
│
├── src/                       # Core Implementation
│   ├── cns.lisp              # Main CNS interpreter (Lisp implementation)
│   └── cns-run               # Actual runner script
│
├── examples/                  # Example CNS Programs
│   ├── README.md             # Example documentation
│   ├── factorial.cns         # Math examples
│   ├── fibonacci.cns
│   ├── is-prime.cns
│   ├── file-demo.cns         # File I/O examples
│   ├── simple-webserver.cns  # Network examples
│   └── ... (27 total examples)
│
├── dataset/                   # Training Datasets for LLMs
│   ├── README.md             # Dataset documentation
│   ├── cns-examples.json     # General examples (26 entries)
│   └── webserver-examples.json  # Webserver examples (62 entries)
│
├── prompts/                   # LLM Generation Prompts
│   ├── cns-generation-template.md     # Main generation template
│   ├── quick-syntax-reference.md      # Quick reference guide
│   ├── webserver-generation-prompt.md # Specialized webserver template
│   └── ... (5 total prompt files)
│
├── tests/                     # All Testing Files
│   ├── llm-tests/            # LLM-Generated Test Cases
│   │   ├── 01-factorial.cns  # Numbered test cases (10 total)
│   │   ├── 02-prime.cns
│   │   ├── ...
│   │   ├── run-tests.sh      # Structure validation script
│   │   ├── run-execution-tests.sh  # Execution testing script
│   │   ├── TEST-RESULTS.md   # Test analysis & results
│   │   └── test-input.txt    # Test data file
│   │
│   └── grok-iterations/      # Grok Iteration Testing
│       ├── iteration-1/      # First iteration (failed)
│       │   ├── grok-prime-test.cns
│       │   ├── grok-prime-corrected.cns
│       │   └── grok-prime-test-composite.cns
│       ├── iteration-2/      # Second iteration (success)
│       │   └── grok-sum-range.cns
│       ├── GROK-FEEDBACK.md  # Issue analysis from iteration 1
│       ├── ITERATION-SUMMARY.md  # Comparison of both iterations
│       └── GROK-ITERATION-2-SUCCESS.md  # Success report
│
├── scripts/                   # Build & Generation Scripts
│   ├── generate-dataset.lisp           # Generate training JSON
│   ├── generate-extended-webservers.lisp
│   ├── generate-more-webservers.lisp
│   └── simple-webserver-gen.lisp
│
└── docs/                      # Documentation
    ├── guides/                # User Guides
    │   ├── AGENTS.md          # Guide for AI agents developing CNS
    │   ├── LLM-INTEGRATION.md # How to integrate CNS with LLMs
    │   ├── LLM-TRAINING-READY.md  # Training preparation guide
    │   ├── TEST-WITH-LLM.md   # Testing CNS with LLMs
    │   └── NEXT-TEST-PROMPT.md  # Next testing instructions
    │
    ├── development/           # Development Documentation
    │   ├── UPDATES.md         # Change log / update history
    │   ├── REAL-SOCKETS.md    # Socket implementation notes
    │   └── DEMO.md            # Demo/presentation notes
    │
    └── archive/               # Historical Documents
        ├── SESSION_SUMMARY.md # Old session notes
        └── SUMMARY.md         # Old project summary
```

## Quick Navigation

### I want to...

**Run a CNS program:**
```bash
./cns-run examples/factorial.cns
```

**See available examples:**
```bash
./cns-run --list
```

**Run all tests:**
```bash
cd tests/llm-tests
./run-tests.sh
```

**Generate training data:**
```bash
sbcl --script scripts/generate-dataset.lisp
```

**Learn about CNS:**
- Start: `README.md`
- Quick start: `QUICKSTART.md`
- For AI agents: `docs/guides/AGENTS.md`

**Integrate with an LLM:**
- Read: `docs/guides/LLM-INTEGRATION.md`
- Use template: `prompts/cns-generation-template.md`
- Quick ref: `prompts/quick-syntax-reference.md`

**Contribute:**
- Read: `docs/development/UPDATES.md`
- Check: `tests/llm-tests/TEST-RESULTS.md`

## File Counts

- **Examples:** 27 CNS programs
- **Dataset:** 88 training examples (JSON)
- **Tests:** 10 LLM-generated + 4 Grok iteration tests
- **Prompts:** 5 generation templates
- **Documentation:** 14 markdown files
- **Scripts:** 4 generation utilities

## Key Files

| File | Purpose |
|------|---------|
| `src/cns.lisp` | Main CNS interpreter (1200+ lines) |
| `cns-run` | Command-line runner |
| `prompts/cns-generation-template.md` | LLM code generation template |
| `tests/llm-tests/run-tests.sh` | Validation test harness |
| `docs/guides/AGENTS.md` | AI agent development guide |
| `QUICKSTART.md` | 5-minute getting started guide |

## Recent Changes

This structure was created on 2025-10-28 to:
1. Clean up root directory (was 15+ markdown files)
2. Logically group related files
3. Separate source, tests, docs, and scripts
4. Make navigation easier for new contributors
5. Support scalable project growth

Previous structure had everything in root - now organized by purpose.

## Maintenance

When adding new files, follow these guidelines:

- **New examples:** → `examples/`
- **Test cases:** → `tests/llm-tests/` (numbered: 11-xxx.cns)
- **Prompts:** → `prompts/`
- **Documentation:** → `docs/guides/` or `docs/development/`
- **Scripts:** → `scripts/`
- **Core code:** → `src/`

Keep root directory minimal: only README, QUICKSTART, and essential files.
