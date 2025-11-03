# CNS Scripts

Utilities for CNS development, testing, and deployment.

---

## llm-tester.py

**Test LLM code generation capabilities with CNS**

Supports multiple LLM providers:
- **xAI Grok** (grok-2-latest, grok-beta)
- **OpenAI** (gpt-4, gpt-3.5-turbo, gpt-4-turbo)
- **Anthropic Claude** (claude-3-opus, claude-3-sonnet, claude-3-haiku)
- **OpenRouter** (any model via unified API)

### Setup

1. **Install dependencies:**
```bash
pip install requests
```

2. **Configure API key:**

Create `.env` file in project root:
```bash
# For xAI Grok
GROK_API_KEY=your-key-here
# or
XAI_API_KEY=your-key-here

# For OpenAI
OPENAI_API_KEY=your-key-here

# For Anthropic Claude
ANTHROPIC_API_KEY=your-key-here
# or
CLAUDE_API_KEY=your-key-here

# For OpenRouter
OPENROUTER_API_KEY=your-key-here
```

### Usage

**Basic test (Grok):**
```bash
./scripts/llm-tester.py --task "Write a factorial function"
```

**Specify provider and model:**
```bash
# OpenAI GPT-4
./scripts/llm-tester.py \
  --provider openai \
  --model gpt-4 \
  --task "Calculate fibonacci numbers"

# Anthropic Claude
./scripts/llm-tester.py \
  --provider claude \
  --model claude-3-opus-20240229 \
  --task "Parse CSV file"

# OpenRouter (any model)
./scripts/llm-tester.py \
  --provider openrouter \
  --model anthropic/claude-3-sonnet \
  --task "Build a web server"
```

**Use detailed prompt template:**
```bash
./scripts/llm-tester.py \
  --task "Write a function to check if a number is prime" \
  --template prompts/detailed-template.md
```

**Custom test name and retries:**
```bash
./scripts/llm-tester.py \
  --task "Sort a list of integers" \
  --name prime-checker \
  --retries 5
```

**Quiet mode (minimal output):**
```bash
./scripts/llm-tester.py \
  --task "Hello world" \
  --quiet
```

### Command-Line Options

| Option | Description | Default |
|--------|-------------|---------|
| `--task` | Task description (required) | - |
| `--name` | Test name for output files | Derived from task |
| `--provider` | LLM provider (grok, openai, claude, openrouter) | `grok` |
| `--model` | Model name (provider-specific) | Provider default |
| `--template` | Prompt template file | `prompts/quick-template.md` |
| `--system-prompt` | System prompt file | `prompts/cns-system-prompt.md` |
| `--retries` | Max retry attempts | `3` |
| `--timeout` | Execution timeout (seconds) | `5` |
| `--api-key` | API key (overrides .env) | From .env |
| `--quiet` | Minimal output | `false` |

### Output

Test results are saved to:
- **Generated code:** `tests/llm-tests/generated/`
- **Test results:** `tests/llm-tests/results/`

Each test produces:
- `{test-name}_iter{N}_{timestamp}.cns` - Generated code
- `{test-name}_{timestamp}.json` - Full test results with metadata

### Example Results

```json
{
  "test_name": "factorial-function",
  "model": "grok-2-latest",
  "timestamp": "2025-11-03T12:34:56",
  "success": true,
  "total_attempts": 1,
  "attempts": [
    {
      "attempt_number": 1,
      "generation_time": 1.23,
      "validation_passed": true,
      "execution_passed": true,
      "raw_output": "...",
      "cns_code": "...",
      "execution_output": "..."
    }
  ]
}
```

### Provider-Specific Notes

**Grok (xAI)**
- Default model: `grok-2-latest`
- API: https://api.x.ai/v1
- Get key: https://console.x.ai/

**OpenAI**
- Default model: `gpt-4`
- API: https://api.openai.com/v1
- Get key: https://platform.openai.com/api-keys

**Claude (Anthropic)**
- Default model: `claude-3-sonnet-20240229`
- API: https://api.anthropic.com/v1
- Get key: https://console.anthropic.com/

**OpenRouter**
- Default model: `anthropic/claude-3-sonnet`
- API: https://openrouter.ai/api/v1
- Get key: https://openrouter.ai/keys
- Supports 100+ models from multiple providers

### Auto-Retry Logic

The harness automatically:
1. Feeds validation errors back to the LLM
2. Feeds execution errors back to the LLM
3. Retries up to N times (default: 3)
4. Tracks all attempts in results JSON

This creates a self-improving feedback loop!

---

## build-starter.sh

**Build CNS starter package for distribution**

Creates lightweight tarball with essential files:
- Core executables (`cns-run`, `cns-validate`)
- 6 curated examples (both `.cns` and `.cnsc` formats)
- Quick start guide

```bash
./scripts/build-starter.sh
```

Output: `cns-starter.tar.gz` (~34KB)

---

## install-https.sh

**Install HTTPS support for CNS**

Installs Common Lisp libraries for SSL/TLS:
- cl+ssl (SSL/TLS client)
- flexi-streams (character encoding)

```bash
./scripts/install-https.sh
```

Requires: SBCL, Quicklisp

---

## generate-*.lisp

**Code generation scripts for testing**

Historical scripts used to generate large test datasets:
- `generate-dataset.lisp` - Generate training data
- `generate-extended-webservers.lisp` - Web server variations
- `generate-more-webservers.lisp` - Additional web examples
- `simple-webserver-gen.lisp` - Basic server generator

Primarily for internal development and benchmarking.

---

## Development Workflow

### 1. Test LLM Generation

```bash
# Test basic task
./scripts/llm-tester.py --task "Sum numbers 1 to 100"

# Test complex task with retries
./scripts/llm-tester.py \
  --task "Build REST API with JSON responses" \
  --template prompts/detailed-template.md \
  --retries 5
```

### 2. Review Results

```bash
# Check generated code
cat tests/llm-tests/generated/sum-numbers-1-to-100_iter1_*.cns

# View full results
cat tests/llm-tests/results/sum-numbers-1-to-100_*.json | jq
```

### 3. Validate and Run

```bash
# Validate generated code
./cns-validate tests/llm-tests/generated/sum-numbers-1-to-100_iter1_*.cns

# Execute
./cns-run tests/llm-tests/generated/sum-numbers-1-to-100_iter1_*.cns
```

### 4. Batch Testing

```bash
# Test multiple models
for provider in grok openai claude; do
  ./scripts/llm-tester.py \
    --provider $provider \
    --task "Calculate factorial of 10" \
    --name factorial-$provider
done

# Compare results
ls -lh tests/llm-tests/results/factorial-*
```

---

## Prompt Templates

### quick-template.md

Minimal prompt for simple tasks. Good for:
- Basic algorithms
- Simple data transformations
- Quick prototypes

### detailed-template.md

Comprehensive prompt with examples. Good for:
- Complex applications
- Multi-step workflows
- Edge case handling

### cns-system-prompt.md

System-level instructions for the LLM. Contains:
- Complete CNS syntax reference
- Expression rules and limitations
- Common patterns and examples
- Best practices

**Customization:**
Create your own templates in `prompts/` directory. Use `{TASK}` placeholder for task injection.

---

## Tips

1. **Start with quick-template** - Simpler prompts often work better
2. **Use --retries 5** - Allows LLM to self-correct
3. **Test multiple providers** - Different models have different strengths
4. **Review generated code** - Even successful tests may have style issues
5. **Update system prompt** - Add patterns as you discover them

---

## Troubleshooting

**"No API key provided"**
- Check `.env` file exists in project root
- Verify key format: `PROVIDER_API_KEY=your-key`
- Try `--api-key` flag to override

**"requests library not found"**
```bash
pip install requests
```

**"Validation failed"**
- Check LLM output for markdown fences
- Review system prompt for clarity
- Try detailed-template for complex tasks

**"Execution timeout"**
- Increase `--timeout` value
- Check for infinite loops in generated code
- Review iteration limits

---

**Last Updated:** November 3, 2025  
**Maintainer:** Jesse Crouch
