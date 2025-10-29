# CNS LLM Training Package

**Status**: ✅ Ready for LLM training and code generation

## Overview

CNS has completed LLM integration infrastructure. We have training datasets, prompt templates, and validation tools to enable LLMs to generate CNS code from natural language.

## Dataset Summary

**Location**: `dataset/`

**Statistics**:
- 88 total training examples
- 62 webserver examples (70%) - various routes, methods, status codes
- 26 general examples (30%) - math, file I/O, lists, logic

**Format**: JSON with structure:
```json
{
  "id": "unique-identifier",
  "prompt": "Natural language task",
  "cns-code": "Complete CNS implementation",
  "category": "webserver|math|file-io|...",
  "tags": ["features"],
  "difficulty": "easy|medium|hard"
}
```

**Files**:
- `cns-examples.json` - 26 general examples
- `webserver-examples.json` - 8 basic webserver examples
- `webserver-examples-extended.json` - 54 webserver variants

## Prompt Templates

**Location**: `prompts/`

**Files**:
- `cns-generation-template.md` - Complete CNS generation guide with rules and examples
- `webserver-generation-prompt.md` - Specialized webserver prompts
- `webserver-template.md` - Quick webserver template
- `quick-template.md` - Minimal template for simple tasks

**Usage**: Copy template, provide task description, send to LLM.

## Training Approaches

### Option 1: Fine-Tuning
Use dataset JSON files to fine-tune models:
- Convert to LLM-specific format (OpenAI, Anthropic, etc.)
- Train on prompt → CNS code pairs
- Target: 88 examples provide baseline patterns

### Option 2: Few-Shot Learning
Include 2-5 examples from dataset in prompt:
- Choose examples matching task category
- Add to prompt template as demonstrations
- More flexible, no training required

### Option 3: RAG (Retrieval-Augmented)
Retrieve similar examples at generation time:
- Index dataset by category/tags
- Find relevant examples for task
- Include in context for generation

## Validation

**Tools in `src/cns.lisp`**:
- `validate-cns` - Check AST completeness
- `validate-cns-file` - Validate CNS files
- `validate-all-examples` - Batch validation

**Usage**:
```bash
./cns-run examples/factorial.cns  # Execute and validate
```

## Success Criteria

**Target**: LLM generates working webserver in ≤3 iterations

**Metrics**:
- Syntax correctness (parses without errors)
- Semantic correctness (executes as expected)
- Causality completeness (all `Because:` clauses present)
- Effect declarations (all side effects explicit)

## Next Steps

1. **Test with LLM**: Use templates to generate code, validate results
2. **Iterate**: Refine prompts based on common errors
3. **Expand Dataset**: Add examples for weak areas
4. **Fine-Tune**: Train models if few-shot insufficient

See `AGENTS.md` for development guidance and `LLM-INTEGRATION.md` for detailed workflow.
