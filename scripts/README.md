# LLM Test Harness

Autonomous testing tool for evaluating LLM performance on CNS code generation.

## Setup

1. **Install dependencies**:
   ```bash
   pip install requests
   ```

2. **Configure API key** (choose one method):

   **Option 1: .env file (recommended)**
   ```bash
   # Copy template and add your key
   cp .env.template .env
   # Edit .env and set GROK_API_KEY=your-key-here
   ```

   **Option 2: Environment variable**
   ```bash
   export GROK_API_KEY="your-api-key-here"
   ```
   
   **Option 3: Command line**
   ```bash
   python scripts/llm-tester.py --api-key "your-key-here" --task "..."
   ```

   Get your API key from: https://console.x.ai/

## Usage

### Basic Test
```bash
python scripts/llm-tester.py --task "factorial function"
```

### With Custom Template
```bash
python scripts/llm-tester.py \
  --task "webserver with three routes" \
  --template prompts/webserver-template.md \
  --retries 5
```

### Specify Model
```bash
python scripts/llm-tester.py \
  --task "fibonacci sequence" \
  --model grok-beta \
  --name fib-test
```

### Quiet Mode
```bash
python scripts/llm-tester.py --task "prime checker" --quiet
```

## What It Does

1. **Generates** CNS code using Grok API
2. **Extracts** code from LLM output (handles markdown)
3. **Validates** using `./src/cns-validate`
4. **Executes** using `./src/cns-run` (if validation passes)
5. **Retries** with error feedback if validation/execution fails
6. **Saves** results and generated code

## Output Files

- **Results**: `tests/llm-tests/results/{test-name}_{timestamp}.json`
- **Generated Code**: `tests/llm-tests/generated/{test-name}_iter{N}_{timestamp}.cns`

## Examples

### Test factorial generation
```bash
python scripts/llm-tester.py --task "calculate factorial of a number"
```

### Test webserver generation
```bash
python scripts/llm-tester.py \
  --task "HTTP server with /hello, /time, and /status routes" \
  --template prompts/webserver-template.md
```

## Rate Limits

Current settings (reasonable defaults):
- Max tokens: 2000 per request
- Temperature: 0.7
- Timeout: 30s per request
- No built-in rate limiting (Grok has generous limits)

Adjust in code if needed.

## Auto-Retry Logic

The harness automatically:
1. Feeds validation errors back to the LLM
2. Feeds execution errors back to the LLM
3. Retries up to N times (default: 3)
4. Tracks all attempts in results JSON

This creates a self-improving feedback loop!
