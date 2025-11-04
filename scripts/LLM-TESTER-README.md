# CNS LLM Tester - Usage Guide

## Overview

The CNS LLM Tester automatically tests language models' ability to generate valid CNS code using **SYNTAX.md as the single source of truth**. No duplicate documentation needed!

## Quick Start

```bash
# Test Grok on a simple task
./scripts/test-llm --task "Calculate factorial of 10"

# Test with custom name
./scripts/test-llm --task "Sum numbers 1 to 100" --name sum-range

# Test with different provider
./scripts/test-llm --task "Build HTTP server" --provider claude
```

## How It Works

1. **Reads SYNTAX.md** - Uses the complete CNS reference (already has `{TASK}` placeholder)
2. **Fills in task** - Replaces `{TASK}` with your task description
3. **Calls LLM** - Sends prompt to chosen provider (Grok, GPT-4, Claude, etc.)
4. **Validates** - Runs `cns-validate` on generated code
5. **Executes** - Runs `cns-run` to test execution
6. **Retries** - If validation/execution fails, retries with error feedback (up to 3 times)
7. **Saves results** - Stores generated code and test results

## Command Options

```
--task TASK         Task description (REQUIRED)
--name NAME         Test name (default: task in kebab-case)
--provider NAME     LLM provider: grok, openai, claude, openrouter (default: grok)
--model MODEL       Specific model (default: provider's best model)
--retries N         Max retry attempts (default: 3)
--timeout N         Execution timeout in seconds (default: 5)
--quiet             Minimal output
```

## Supported Providers

### Grok (xAI)
```bash
# Set in .env:
GROK_API_KEY=your-key-here

# Use:
./scripts/test-llm --task "Your task" --provider grok
```

### OpenAI (GPT-4)
```bash
# Set in .env:
OPENAI_API_KEY=your-key-here

# Use:
./scripts/test-llm --task "Your task" --provider openai --model gpt-4
```

### Anthropic (Claude)
```bash
# Set in .env:
ANTHROPIC_API_KEY=your-key-here

# Use:
./scripts/test-llm --task "Your task" --provider claude
```

### OpenRouter (Any Model)
```bash
# Set in .env:
OPENROUTER_API_KEY=your-key-here

# Use:
./scripts/test-llm --task "Your task" --provider openrouter --model anthropic/claude-3-opus
```

## Example Tasks by Complexity

### Level 1 - Simple (Always passes)
```bash
./scripts/test-llm --task "Calculate factorial of 10"
./scripts/test-llm --task "Print first 20 Fibonacci numbers"
./scripts/test-llm --task "Find GCD of 48 and 18"
./scripts/test-llm --task "Check if 17 is prime"
```

### Level 2 - File I/O
```bash
./scripts/test-llm --task "Count words in file input.txt"
./scripts/test-llm --task "Read file and convert to uppercase"
./scripts/test-llm --task "Count lines starting with # in file"
```

### Level 3 - CLI Tools
```bash
./scripts/test-llm --task "Build word counter with --lines and --chars flags"
./scripts/test-llm --task "Create grep-like tool that searches for pattern in file"
./scripts/test-llm --task "Build file concatenator that accepts multiple files"
```

### Level 4 - HTTP Servers
```bash
./scripts/test-llm --task "Build HTTP server on port 8080 with / and /about routes"
./scripts/test-llm --task "Create request logger that logs method and path to file"
./scripts/test-llm --task "Build JSON API that returns current timestamp"
```

### Level 5 - Database Operations
```bash
./scripts/test-llm --task "Create user database with INSERT and SELECT"
./scripts/test-llm --task "Build task tracker with CREATE, READ, UPDATE operations"
./scripts/test-llm --task "Make inventory system with database and HTTP API"
```

### Level 6 - Process Management
```bash
./scripts/test-llm --task "Launch 3 background jobs and wait for completion"
./scripts/test-llm --task "Build job queue that runs shell commands from file"
./scripts/test-llm --task "Create process monitor that checks status every second"
```

### Level 7 - Complex Systems
```bash
./scripts/test-llm --task "Build multi-route API with database backend"
./scripts/test-llm --task "Create CSV to JSON converter with validation"
./scripts/test-llm --task "Build microservice coordinator with 3 background servers"
```

## Output Files

All results are saved to `tests/llm-tests/`:

```
tests/llm-tests/
├── generated/              # Generated CNS code
│   └── task-name_iter1_timestamp.cns
└── results/                # Test results (JSON)
    └── task-name_timestamp.json
```

### Result JSON Structure

```json
{
  "test_name": "factorial-10",
  "model": "grok-2-latest",
  "timestamp": "2025-11-04T12:34:56",
  "success": true,
  "total_attempts": 1,
  "attempts": [
    {
      "attempt_number": 1,
      "generation_time": 2.5,
      "validation_passed": true,
      "execution_passed": true,
      "code_file": "tests/llm-tests/generated/factorial-10_iter1_20251104_123456.cns",
      "execution_output": "3628800\n"
    }
  ]
}
```

## Single Source of Truth: SYNTAX.md

**No duplicate documentation!** The system uses `SYNTAX.md` directly:

1. **SYNTAX.md** contains complete CNS reference with `{TASK}` placeholder (line 3)
2. **llm-tester.py** reads SYNTAX.md and replaces `{TASK}` with your task
3. **No templates needed** - SYNTAX.md is comprehensive (1391 lines)
4. **Always up-to-date** - Any SYNTAX.md update automatically applies to LLM testing

## Advanced Usage

### Compare Multiple Models

```bash
# Test same task with 3 different models
./scripts/test-llm --task "Build HTTP server" --provider grok --name server-grok
./scripts/test-llm --task "Build HTTP server" --provider openai --name server-gpt4
./scripts/test-llm --task "Build HTTP server" --provider claude --name server-claude

# Compare results
ls -lh tests/llm-tests/results/server-*
```

### Batch Testing

```bash
# Create test batch script
cat > run-batch-tests.sh << 'EOF'
#!/bin/bash
TASKS=(
  "Calculate factorial of 10"
  "Find GCD of 48 and 18"
  "Sum numbers 1 to 100"
  "Build HTTP server on port 8080"
)

for task in "${TASKS[@]}"; do
  echo "Testing: $task"
  ./scripts/test-llm --task "$task"
done
EOF

chmod +x run-batch-tests.sh
./run-batch-tests.sh
```

### Custom Timeouts for Long-Running Programs

```bash
# Web server needs longer timeout
./scripts/test-llm --task "Build HTTP server" --timeout 10

# Quick math task needs less
./scripts/test-llm --task "Calculate factorial" --timeout 2
```

## Troubleshooting

### "ERROR: No API key provided"
```bash
# Add to .env file:
echo "GROK_API_KEY=your-key-here" >> .env
```

### "SYNTAX.md not found"
```bash
# Make sure you're running from project root:
cd /home/bolt/Documents/cns
./scripts/test-llm --task "Your task"
```

### Validation Failures
- Check `tests/llm-tests/results/` for detailed error messages
- LLM will auto-retry with error feedback (up to 3 times)
- Common issues: missing `Given:`, wrong syntax, undeclared variables

### Execution Failures
- Check if task requires external files (create them first)
- Increase timeout for complex programs
- Review execution output in results JSON

## Architecture

```
┌─────────────────┐
│   SYNTAX.md     │  ← Single source of truth (1391 lines)
│   (with {TASK}) │
└────────┬────────┘
         │
         ↓
┌─────────────────┐
│  llm-tester.py  │  ← Replaces {TASK}, calls LLM
└────────┬────────┘
         │
         ↓
┌─────────────────┐
│   LLM API       │  ← Grok/GPT-4/Claude generates code
│ (Grok/GPT/etc)  │
└────────┬────────┘
         │
         ↓
┌─────────────────┐
│  cns-validate   │  ← Validates syntax
└────────┬────────┘
         │
         ↓
┌─────────────────┐
│    cns-run      │  ← Executes code
└────────┬────────┘
         │
         ↓
┌─────────────────┐
│  Save Results   │  ← JSON + generated code
└─────────────────┘
```

## What Changed (Cleanup)

**Removed:**
- ❌ `prompts/quick-template.md` (not needed)
- ❌ `prompts/cns-system-prompt.md` (not needed)
- ❌ `load_prompt_template()` function (replaced)
- ❌ `load_system_prompt()` function (replaced)
- ❌ `--template` CLI argument (removed)
- ❌ `--system-prompt` CLI argument (removed)

**Added:**
- ✅ `build_prompt_from_syntax()` - uses SYNTAX.md directly
- ✅ Simplified CLI (no template/prompt args)
- ✅ Better help text with examples
- ✅ This README

**Result:** Cleaner, simpler, single source of truth!
