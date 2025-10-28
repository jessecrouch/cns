# CNS Training Dataset

This directory contains training data for teaching Large Language Models (LLMs) to generate Causal Narrative Script (CNS) code.

## Dataset Overview

### Files

| File | Examples | Description |
|------|----------|-------------|
| `cns-examples.json` | 26 | General CNS examples (math, file I/O, lists, etc.) |
| `webserver-examples.json` | 8 | Basic webserver implementations |
| `webserver-examples-extended.json` | 54 | Extended webserver variants |
| **Total** | **88** | **Complete training dataset** |

## Dataset Format

Each dataset uses JSON format with the following structure:

```json
{
  "version": "1.0",
  "description": "CNS training dataset",
  "count": 26,
  "examples": [
    {
      "id": "unique-identifier",
      "prompt": "Natural language task description",
      "cns-code": "Complete CNS implementation",
      "category": "webserver|math|file-io|list|general",
      "tags": ["feature", "tags"],
      "difficulty": "easy|medium|hard",
      "expected": "Expected behavior description",
      "notes": "Additional notes"
    }
  ]
}
```

## Dataset Statistics

### By Category

- **Webserver**: 62 examples (70%)
- **Math**: 8 examples (9%)
- **File I/O**: 4 examples (5%)
- **Lists**: 3 examples (3%)
- **General**: 11 examples (13%)

### By Difficulty

- **Easy**: 45 examples (51%)
- **Medium**: 35 examples (40%)
- **Hard**: 8 examples (9%)

### Common Features

- **Loops**: 75% of examples
- **Conditionals**: 65% of examples
- **Effects**: 80% of examples
- **Networking**: 70% of examples
- **State Management**: 55% of examples

## Using the Dataset

### Training LLMs

#### Fine-Tuning Format

Convert to training pairs:
```python
{
  "prompt": "Generate CNS code for: {task_description}",
  "completion": "{cns_code}"
}
```

#### Few-Shot Learning

Use 3-5 examples as context:
```
Example 1: [prompt] -> [cns-code]
Example 2: [prompt] -> [cns-code]
...
Now generate for: [new-task]
```

### Validation

Test generated code:
```bash
sbcl --eval "(load \"cns.lisp\")" \
     --eval "(validate-cns (parse-cns \"...\"))" \
     --quit
```

### Execution

Run generated code:
```bash
./cns-run generated-code.cns
```

## Dataset Categories

### Webserver Examples

Covers:
- Different ports (3000-10000)
- HTTP methods (GET, POST, PUT, DELETE, PATCH)
- Status codes (200, 404, 500, etc.)
- Content types (HTML, JSON, XML, CSS, JS)
- Multi-route handling
- Connection limits
- Error handling

### Mathematical Examples

- Factorial
- Fibonacci
- Prime checking
- GCD calculation
- Power functions
- Digit sum
- Number filtering

### File I/O Examples

- Reading files
- Writing files
- Appending data
- Word counting
- Log file processing

### List Processing

- Filtering
- Mapping
- Aggregation
- Sorting
- Statistics

## Quality Guidelines

All examples follow CNS best practices:

✅ **Required:**
- `Story:` description
- All variables declared in `Given:`
- Sequential step numbering
- `Because:` clause for every step
- `Effect:` declarations for side effects
- `End:` block with return value

✅ **Recommended:**
- Clear, narrative step descriptions
- Explicit causality explanations
- Error handling where appropriate
- Semantic type tags (e.g., `[≥ 0]`, `[network port]`)

## Extending the Dataset

### Adding New Examples

1. Create CNS code following the template
2. Test with interpreter:
   ```bash
   ./cns-run new-example.cns
   ```
3. Generate JSON entry:
   ```bash
   sbcl --script generate-dataset.lisp
   ```

### Webserver Variants

Generate more variants:
```bash
sbcl --script simple-webserver-gen.lisp
```

## Dataset Generation

The dataset was generated using:
- `generate-dataset.lisp` - Converts `.cns` files to JSON
- `simple-webserver-gen.lisp` - Creates webserver variants
- Source examples from `examples/` directory

### Regenerate Dataset

```bash
sbcl --script generate-dataset.lisp
sbcl --script simple-webserver-gen.lisp
```

## Evaluation Metrics

When evaluating LLM-generated CNS code, measure:

1. **Syntactic Validity**: Does it parse correctly?
2. **Structural Completeness**: Has all required sections?
3. **Causality**: Every step has meaningful `Because:`?
4. **Effect Declaration**: All side effects declared?
5. **Executability**: Does it run without errors?
6. **Correctness**: Does it solve the task?

Target metrics for webserver task:
- Parse success: ≥95%
- Structural completeness: ≥90%
- Execution success: ≥85%
- Correct behavior: ≥80%
- Iterations to success: ≤3

## License

This dataset is part of the CNS project. See main README for license information.

## Contributing

To contribute new examples:
1. Follow CNS syntax guidelines
2. Test the code works correctly
3. Add to appropriate category
4. Regenerate dataset JSON
5. Update statistics in this README

## References

- CNS Language Spec: `../README.md`
- Prompt Templates: `../prompts/`
- Agent Guide: `../AGENTS.md`
- Development Updates: `../UPDATES.md`
