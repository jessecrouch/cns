# Repository Reorganization Plan

## Current Issues
- 15+ markdown files in root directory
- Multiple similar documentation files
- Generator scripts mixed with main code
- Test files spread across multiple locations

## New Structure

```
cns/
├── README.md                          # Main project readme (keep)
├── QUICKSTART.md                      # Quick start guide (keep)
├── .gitignore                         # Git config (keep)
│
├── src/                               # Core source code
│   ├── cns.lisp                       # Main interpreter
│   └── cns-run                        # Execution script
│
├── examples/                          # Example CNS programs (keep as-is)
│   └── *.cns
│
├── dataset/                           # Training datasets (keep as-is)
│   └── *.json
│
├── prompts/                           # LLM generation prompts (keep as-is)
│   └── *.md
│
├── tests/                             # All test-related files
│   ├── llm-tests/                     # LLM-generated test cases
│   │   ├── 01-factorial.cns           # Numbered test cases
│   │   ├── ...
│   │   ├── run-tests.sh
│   │   ├── run-execution-tests.sh
│   │   └── TEST-RESULTS.md
│   │
│   └── grok-iterations/               # Grok-specific test iterations
│       ├── iteration-1/
│       │   ├── grok-prime-test.cns           # Original (failed)
│       │   └── grok-prime-corrected.cns      # Corrected version
│       ├── iteration-2/
│       │   └── grok-sum-range.cns            # Second attempt (success)
│       ├── GROK-FEEDBACK.md           # Issue analysis
│       ├── ITERATION-SUMMARY.md       # Comparison
│       └── GROK-ITERATION-2-SUCCESS.md # Success report
│
├── scripts/                           # Build and generation scripts
│   ├── generate-dataset.lisp
│   ├── generate-extended-webservers.lisp
│   ├── generate-more-webservers.lisp
│   └── simple-webserver-gen.lisp
│
├── docs/                              # Documentation
│   ├── guides/                        # User guides
│   │   ├── AGENTS.md                  # Guide for AI agents
│   │   ├── LLM-INTEGRATION.md        # How to integrate with LLMs
│   │   ├── LLM-TRAINING-READY.md     # Training preparation
│   │   ├── TEST-WITH-LLM.md          # Testing with LLMs
│   │   └── NEXT-TEST-PROMPT.md       # Next test instructions
│   │
│   ├── development/                   # Development docs
│   │   ├── UPDATES.md                 # Change log
│   │   ├── REAL-SOCKETS.md           # Socket implementation notes
│   │   └── DEMO.md                    # Demo/presentation notes
│   │
│   └── archive/                       # Historical/redundant docs
│       ├── SESSION_SUMMARY.md         # Old session notes
│       └── SUMMARY.md                 # Old summary (redundant)
│
└── test-input.txt                     # Test data file (keep in root for easy access)
```

## Files to Move

### To `src/`
- cns.lisp
- cns-run

### To `scripts/`
- generate-dataset.lisp
- generate-extended-webservers.lisp
- generate-more-webservers.lisp
- simple-webserver-gen.lisp

### To `docs/guides/`
- AGENTS.md
- LLM-INTEGRATION.md
- LLM-TRAINING-READY.md
- TEST-WITH-LLM.md
- NEXT-TEST-PROMPT.md

### To `docs/development/`
- UPDATES.md
- REAL-SOCKETS.md
- DEMO.md

### To `docs/archive/`
- SESSION_SUMMARY.md
- SUMMARY.md (potentially redundant with README)

### To `tests/grok-iterations/`
- GROK-FEEDBACK.md
- ITERATION-SUMMARY.md
- GROK-ITERATION-2-SUCCESS.md
- llm-tests/grok-*.cns files

## Benefits

1. **Cleaner root** - Only essential files (README, QUICKSTART, .gitignore)
2. **Logical grouping** - Related files together
3. **Easier navigation** - Clear purpose for each directory
4. **Scalability** - Easy to add new tests, docs, scripts
5. **Professional** - Standard project structure

## What NOT to change

- Keep examples/ as-is (good structure)
- Keep dataset/ as-is (good structure)
- Keep prompts/ as-is (good structure)
- Keep numbered test files in llm-tests/ (good naming)
