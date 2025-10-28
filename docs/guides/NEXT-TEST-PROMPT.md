# Next Test for Grok (Using Updated Template)

## Instructions for Testing

1. Copy the ENTIRE contents of `prompts/cns-generation-template.md` (updated version)
2. Paste it to Grok
3. Then provide this task:

---

## Task for Grok

**Task:** Write CNS code to calculate the sum of all even numbers in a list

**Additional Context:**
- Category: data-processing
- Difficulty: easy
- Required Effects: none

**Generate CNS code that:**
1. Follows the CNS structure (Story, Given, Steps, End)
2. Includes Because: clauses for all steps
3. Declares all effects explicitly
4. Uses clear, narrative step descriptions
5. Handles edge cases appropriately

---

**Expected Output:** CNS code that:
- Takes a list of integers
- Iterates through the list
- Accumulates sum of even numbers
- Returns the sum

**Test Input:** `[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]`
**Expected Result:** `30` (2 + 4 + 6 + 8 + 10)

---

## What to Check

After getting Grok's output, verify:

1. ✅ **No semantic tags** in variable declarations
2. ✅ **Boolean values** are `TRUE`/`FALSE` (if any)
3. ✅ **Uses `=` not `==`** in comparisons
4. ✅ **Single action per `Then:`** line
5. ✅ **No Error block** (unless needed for I/O)
6. ✅ **Single-line `End:`** format

## Test Commands

```bash
# Save Grok's output to:
nano llm-tests/grok-sum-evens.cns

# Validate structure:
cd /home/bolt/Documents/cns
sbcl --noinform --load cns.lisp --eval "(validate-cns-file \"llm-tests/grok-sum-evens.cns\")" --quit

# Test execution:
./cns-run llm-tests/grok-sum-evens.cns

# Expected output should show:
# Return: 30
```

## Scoring

**Perfect Generation:**
- Passes validation: +1 point
- Runs without errors: +1 point
- Correct result (30): +1 point
- No manual corrections needed: +2 points
- **Total: 5/5 points**

**Good Generation (needs 1-2 small fixes):**
- 3-4/5 points

**Poor Generation (needs 3+ fixes):**
- 0-2/5 points

---

## Alternative Test Tasks

If you want to test multiple scenarios:

### Task 2: Fibonacci Sequence
"Write CNS code to generate the first N numbers in the Fibonacci sequence"
- Test with N=10
- Expected: [1, 1, 2, 3, 5, 8, 13, 21, 34, 55]

### Task 3: Find Maximum in List
"Write CNS code to find the maximum value in a list of integers"
- Test with: [3, 7, 2, 9, 1, 5, 8]
- Expected: 9

### Task 4: Reverse a String
"Write CNS code to reverse a string"
- Test with: "hello world"
- Expected: "dlrow olleh"

---

## Reporting Results

After testing, update `ITERATION-SUMMARY.md` with:
- Task description
- Grok's output (full code)
- Issues found (if any)
- Corrections made (if any)
- Parse/execution results
- Score out of 5
