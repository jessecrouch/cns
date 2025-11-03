# LLM Integration Guide for CNS

This document explains how to use CNS with Large Language Models for code generation, validation, and iterative refinement.

## Overview

CNS is designed to be LLM-friendly, making it easier for AI models to:
1. Generate correct code with explicit reasoning
2. Understand and debug existing code
3. Iteratively improve code through feedback loops

## Phase 2: LLM Integration (Completed)

### Components

1. **Syntax Template** (`SYNTAX.md`)
   - Single comprehensive reference at project root
   - Complete rules and examples
   - Optimized for LLM code generation

2. **Validation System** (in `cns.lisp`)
   - `validate-cns`: Check AST for completeness
   - `validate-cns-file`: Validate CNS files
   - `validate-all-examples`: Batch validation

3. **Feedback Loop** (in `cns.lisp`)
   - `validate-and-correct`: Iterative error correction
   - `generate-correction-prompt`: Create fix prompts
   - `save-correction-prompts`: Export for LLM processing

4. **Dataset** (`dataset/`)
   - 5 complete webserver examples with task descriptions
   - 7 additional task templates for expansion
   - JSON format for easy LLM training

## Workflow: Generating CNS Code with LLMs

### Step 1: Prepare the Prompt

Use the template from `SYNTAX.md`:

```
Task: Create a webserver on port 8080 that responds with "Hello"

[Insert SYNTAX.md here with {TASK} replaced]
```

### Step 2: Get LLM Response

Send the prompt to your LLM (GPT-4, Claude, Grok, etc.). The response should be CNS code.

### Step 3: Validate the Generated Code

```lisp
;; In SBCL
(load "cns.lisp")

;; Save LLM output to file
(with-open-file (stream "generated.cns" :direction :output)
  (write-string llm-output stream))

;; Validate
(validate-cns-file "generated.cns")
```

### Step 4: Execute and Test

```bash
./cns-run generated.cns
```

### Step 5: Iterate if Needed

If validation fails:

```lisp
;; Get correction prompts
(multiple-value-bind (success-p code attempts prompts)
    (validate-and-correct llm-output :max-retries 3 :verbose t)
  (unless success-p
    ;; Save prompts for next LLM call
    (save-correction-prompts prompts "corrections.txt")))
```

Send the correction prompt back to the LLM and repeat.

## Validation Checks

The CNS validator checks for:

1. **Required Sections**
   - ✅ `Story:` header present
   - ✅ `Given:` section with variable declarations
   - ✅ `End:` section with return value

2. **Step Completeness**
   - ✅ Every step has a `Because:` clause
   - ✅ Steps are sequentially numbered (1, 2, 3...)
   - ✅ No gaps in step numbers

3. **Syntax Validity**
   - ✅ Code parses without errors
   - ✅ No undefined operations

## Common LLM Errors and Fixes

### Error: Missing Because Clause

**Problem:**
```cns
Step 1 → Create socket on port
  Effect: Create socket server_socket on 8080
```

**Fix:**
```cns
Step 1 → Create socket on port
  Effect: Create socket server_socket on 8080
  Because: We need to listen for incoming connections
```

### Error: Missing Variable Declaration

**Problem:**
```cns
Story: Test
Step 1 → Add to count
  Then: count becomes count + 1
End: Return count
```

**Fix:**
```cns
Story: Test
Given:
  count: Integer = 0
Step 1 → Add to count
  Then: count becomes count + 1
  Because: Increment the counter
End: Return count
```

### Error: Non-Sequential Steps

**Problem:**
```cns
Step 1 → First step
Step 3 → Third step  (Missing Step 2!)
```

**Fix:**
```cns
Step 1 → First step
Step 2 → Second step
Step 3 → Third step
```

## Prompt Engineering Tips

### 1. Be Specific About Format

Good: "Generate CNS code with explicit Because clauses for every step"
Bad: "Write a webserver"

### 2. Provide Context

Include 1-2 example programs in the prompt to show the expected structure.

### 3. Request Reasoning

Ask the LLM to explain its design choices, which often leads to better Because clauses.

### 4. Iterate with Feedback

If the first attempt fails validation, show the LLM the specific errors.

## Example: Complete Workflow

```bash
# 1. Create prompt file
cat prompts/webserver-template.md | sed 's/{TASK}/Create a counter webserver/' > /tmp/prompt.txt

# 2. Get LLM response (manual step with your LLM of choice)
# Save to /tmp/response.cns

# 3. Validate
sbcl --load cns.lisp --eval "(validate-cns-file \"/tmp/response.cns\")" --quit

# 4. If valid, test
./cns-run /tmp/response.cns

# 5. If errors, generate correction
sbcl --load cns.lisp --eval "(validate-and-correct (read-file \"/tmp/response.cns\"))" --quit
```

## Batch Processing

To validate many LLM-generated files:

```lisp
(load "cns.lisp")

(defun validate-directory (dir-path)
  "Validate all .cns files in directory."
  (dolist (file (directory (concatenate 'string dir-path "*.cns")))
    (validate-cns-file (namestring file))))

(validate-directory "generated/")
```

## Metrics for LLM Performance

Track these metrics when evaluating LLMs for CNS generation:

1. **First-Pass Success Rate**: % of code that validates on first try
2. **Iterations to Success**: Average attempts needed for valid code
3. **Execution Success Rate**: % of valid code that runs without errors
4. **Correctness**: % of code that produces expected output

Example tracking:

```lisp
(defun track-llm-performance (task generated-code expected-output)
  "Evaluate LLM-generated CNS code."
  (multiple-value-bind (valid-p code attempts prompts)
      (validate-and-correct generated-code)
    (if valid-p
        (let* ((ast (parse-cns code))
               (result (interpret-cns ast :verbose nil)))
          (list :valid t 
                :attempts attempts
                :correct (equal result expected-output)
                :result result))
        (list :valid nil
              :attempts attempts
              :errors prompts))))
```

## Dataset Structure

The `dataset/webserver-examples.json` file contains training examples:

```json
{
  "id": 1,
  "task": "Create a webserver on port 8080...",
  "category": "basic",
  "cns_code": "Story: ...\n..."
}
```

Use this to:
- Fine-tune models for CNS generation
- Create few-shot prompts
- Benchmark LLM performance

## Future Enhancements

- [ ] Automated LLM API integration (GPT-4, Claude, etc.)
- [ ] Real-time feedback during generation
- [ ] Semantic validation (check if code logic matches task)
- [ ] Automated test case generation
- [ ] Performance benchmarking suite

## Best Practices

1. **Start Simple**: Begin with basic examples before complex webservers
2. **Use Templates**: Consistent prompts lead to better results
3. **Validate Early**: Check code before execution
4. **Iterate**: Use feedback loops for refinement
5. **Document**: Track which prompts work best for your LLM

## Troubleshooting

**Q: LLM generates code without Because clauses**
A: Add explicit requirement to prompt: "EVERY step must include a Because: clause explaining the reasoning"

**Q: LLM uses wrong syntax**
A: Include more complete examples in the prompt, showing proper syntax

**Q: Validation passes but execution fails**
A: The validator checks structure, not logic. Add execution tests to your workflow.

**Q: Too many iterations needed**
A: Improve prompt specificity or try a different LLM model

## Contact & Feedback

For questions about LLM integration:
- Open an issue on GitHub
- Check UPDATES.md for latest roadmap
- See examples/ for working code patterns

---

**Next Steps**: Phase 3 - Testing and validation of LLM-generated webservers at scale!
