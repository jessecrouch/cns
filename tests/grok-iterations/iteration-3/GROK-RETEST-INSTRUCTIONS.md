# Grok Re-test Instructions with New Template

## Overview
Re-run the Grok iteration 3 test using the new consolidated template to verify it prevents the 10 validation errors we saw initially.

## Files Created for This Test

1. **NEW-GROK-TEST-PROMPT.md** - Complete prompt with task + template
2. **NEW-TEMPLATE-TEST-SUMMARY.md** - Test goals and expectations
3. **GROK-RETEST-INSTRUCTIONS.md** - This file

## What Changed Since Last Test

### Old Approach (Had Issues)
- Documentation scattered across 6+ files
- Function references inconsistent
- `NOW()` documented somewhere, `TIMESTAMP()` elsewhere
- CNSC format causing confusion
- Result: 10 validation errors

### New Approach (Should Work)
- Single 830-line consolidated template
- Function Lookup Table at top (lines 1-60)
- Explicit "Use This ✅ / NOT This ❌" format
- Shows `TIMESTAMP()` ✅ not `NOW()` ❌
- Validation Checklist included
- No CNSC references
- Self-contained (no cross-references)

## Testing Steps

### 1. Submit to Grok-2

**Input File:** `NEW-GROK-TEST-PROMPT.md`

**API Call:**
```bash
# Using x.ai API (adjust endpoint as needed)
curl -X POST https://api.x.ai/v1/chat/completions \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $GROK_API_KEY" \
  -d @- << 'JSON'
{
  "model": "grok-2-latest",
  "messages": [
    {
      "role": "system",
      "content": "You are a CNS programming language code generator. Generate only valid CNS code following the reference provided."
    },
    {
      "role": "user", 
      "content": "<contents of NEW-GROK-TEST-PROMPT.md>"
    }
  ],
  "temperature": 0.1
}
JSON
```

### 2. Save Generated Code

Save the output to: `grok-request-logger-v3.cns`

### 3. Validate

```bash
cd /home/bolt/Documents/cns/tests/grok-iterations/iteration-3
../../cns-validate grok-request-logger-v3.cns
```

**Expected:** ✅ VALIDATION PASSED (no errors)

### 4. Test Execution

```bash
# Start server in background
./cns-run grok-request-logger-v3.cns &
SERVER_PID=$!
sleep 2

# Run test script
./test-request-logger.sh

# Stop server
kill $SERVER_PID
```

**Expected:**
- Server starts on port 8080
- 3 requests logged to CSV
- History endpoint shows requests
- CSV has 4 lines (header + 3 requests)
- No /history request in log

### 5. Compare with Reference

```bash
# Visual comparison
diff -u reference-request-logger.cns grok-request-logger-v3.cns

# Check key differences
grep TIMESTAMP grok-request-logger-v3.cns  # Should exist
grep NOW grok-request-logger-v3.cns        # Should NOT exist
```

### 6. Document Results

Create: `GROK-V3-RESULTS.md` with:

```markdown
# Grok V3 Test Results (New Template)

## Date
2025-11-03

## Generated Code
[Paste grok-request-logger-v3.cns contents]

## Validation Results
[Paste validator output]

## Execution Results
[Paste test-request-logger.sh output]

## Comparison with Previous
- V2 (old template): 10 errors initially, fixed manually
- V3 (new template): [X] errors

## Functions Used
- [✅/❌] TIMESTAMP() vs NOW()
- [✅/❌] Correct built-in variables
- [✅/❌] Proper CSV writing
- [✅/❌] Correct control flow

## Conclusion
[PASS/FAIL] - [Explanation]
```

## Success Criteria

### Must Have (Critical)
- [ ] Code validates without errors
- [ ] Uses `TIMESTAMP()` not `NOW()`
- [ ] Uses correct built-in variable names
- [ ] Server starts and accepts connections
- [ ] Requests logged to CSV file
- [ ] History endpoint works

### Should Have (Important)
- [ ] Code structure matches CNS patterns
- [ ] Proper step numbering
- [ ] All "Because:" clauses present
- [ ] CSV format correct
- [ ] /history not logged

### Nice to Have (Bonus)
- [ ] Code similar to reference implementation
- [ ] Clear variable names
- [ ] Good comments in Because clauses

## Previous Test Results for Comparison

### Grok V2 (Before Template Fix)
**Initial Generation:**
```
ERROR: Cannot declare built-in variable: REQUEST_METHOD
ERROR: Cannot declare built-in variable: REQUEST_PATH  
ERROR: NOW is not a valid expression
ERROR: SPLIT is not a valid function
ERROR: JOIN is not a valid function
... (10 total errors)
```

**After Manual Fixes:**
- All errors corrected
- Validation: PASSED ✅
- Execution: PASSED ✅

### Expected Grok V3 (New Template)
**Initial Generation:**
- Validation: PASSED ✅ (0 errors)
- Uses correct functions from the start
- No manual fixes needed

## Troubleshooting

### If Validation Fails
1. Check which functions are being used incorrectly
2. Verify template's Function Lookup Table has correct info
3. Check if LLM is ignoring template sections
4. Document which parts of template were not followed

### If Execution Fails
1. Check if server starts (port 8080)
2. Verify CSV file creation
3. Check HTTP response format
4. Test with manual curl commands

### If Results Don't Match Reference
1. Compare logic flow (both should work even if different)
2. Check if all requirements met
3. Verify functionality over exact code match

## Notes

- This is a **manual test** requiring Grok API access
- Can be adapted for other LLMs (GPT-4, Claude, etc.)
- Template is in `prompts/detailed-template.md`
- Reference implementation in `reference-request-logger.cns`
- Test infrastructure already set up and working

## Files Location

```
tests/grok-iterations/iteration-3/
├── PROMPT.md                      # Original task description
├── NEW-GROK-TEST-PROMPT.md        # Task + new template
├── NEW-TEMPLATE-TEST-SUMMARY.md   # Test plan
├── GROK-RETEST-INSTRUCTIONS.md    # This file
├── reference-request-logger.cns   # Known good implementation
├── grok-request-logger.cns        # V1 (original)
├── grok-request-logger-v2.cns     # V2 (manually fixed)
├── grok-request-logger-v3.cns     # V3 (new template) - TO BE CREATED
├── test-request-logger.sh         # Test script
└── GROK-V3-RESULTS.md            # Results - TO BE CREATED
```

---

**Status:** Ready for manual testing with Grok API
**Next Action:** Submit NEW-GROK-TEST-PROMPT.md to Grok-2
**Expected Duration:** 5-10 minutes for full test cycle
