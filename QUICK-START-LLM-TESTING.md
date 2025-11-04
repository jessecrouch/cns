# Quick Start: LLM Testing with CNS

## What Just Got Fixed

✅ **llm-tester.py now uses SYNTAX.md as single source of truth**
- No duplicate documentation needed
- No template files required
- No separate system prompts
- Just works!

## Quick Test (30 seconds)

```bash
# From project root:
./scripts/test-llm --task "Calculate factorial of 10"
```

That's it! The tester will:
1. Read SYNTAX.md (1390 lines)
2. Replace `{TASK}` with your task
3. Call Grok API (configured in .env)
4. Validate generated code
5. Execute it
6. Save results

## What You Get

```
tests/llm-tests/
├── generated/
│   └── calculate-factorial-of-10_iter1_20251104_164109.cns
└── results/
    └── calculate-factorial-of-10_20251104_164109.json
```

## Try Different Complexity Levels

```bash
# Level 1: Math (always works)
./scripts/test-llm --task "Sum numbers 1 to 100"

# Level 2: File I/O
./scripts/test-llm --task "Count words in file /tmp/input.txt"

# Level 3: HTTP Server
./scripts/test-llm --task "Build HTTP server on port 8080 with 3 routes"

# Level 4: Database
./scripts/test-llm --task "Create SQLite user database with INSERT and SELECT"

# Level 5: Processes
./scripts/test-llm --task "Launch 3 background jobs and wait for all"
```

## Test Results from Today

**Test 1: Factorial**
- ✅ Generated valid code (first attempt)
- ✅ Passed validation
- ✅ Executed correctly
- Result: Proper factorial calculator with loop

**Test 2: Sum Range**
- ✅ Generated valid code (first attempt)
- ✅ Passed validation
- ✅ Executed correctly
- Result: 1275 (sum of 1 to 50)

## What Changed

**Before:**
- ❌ Needed to create `prompts/quick-template.md`
- ❌ Needed to create `prompts/cns-system-prompt.md`
- ❌ 3 files to maintain
- ❌ Duplicate documentation

**After:**
- ✅ Just SYNTAX.md
- ✅ Single source of truth
- ✅ Already working
- ✅ Zero duplication

## More Info

See `scripts/LLM-TESTER-README.md` for:
- Complete API documentation
- All provider options (Grok, GPT-4, Claude)
- Batch testing examples
- Architecture details
- Troubleshooting guide

## Architecture

```
Task → SYNTAX.md → llm-tester.py → Grok → cns-validate → cns-run → ✅
       (1390 lines)  (replaces {TASK})
```

**That's it. Simple. Clean. Works.** 🎉
