# Grok LLM Generation Feedback & Corrections

## Test Case: Prime Number Checker

### What Grok Generated (First Attempt)

```cns
Story: Determine if a given integer greater than or equal to 2 is a prime number

Given:
  num: Integer [≥ 2] = 17
  is_prime: Boolean = True
  i: Integer = 2

Step 1 → If i * i > num
  Because: All potential factors up to sqrt(num) have been checked
  Then: go to End
  Otherwise: go to Step 2

Step 2 → If num % i == 0
  Because: i is a divisor, so num is not prime
  Then: is_prime becomes False
       go to End
  Otherwise: go to Step 3

Step 3 → Increment i by 1
  Because: Move to the next potential factor
  Then: i becomes i + 1
       repeat from Step 1

Error:
  Return False
  Effect: none
  Because: Handle case where num < 2 although assumed valid

End: Return is_prime
```

### Issues Found

#### 1. **Semantic Tag Placement** ❌
- **Wrong:** `num: Integer [≥ 2] = 17`
- **Correct:** `num: Integer = 17`
- **Explanation:** Semantic tags like `[≥ 2]` are optional documentation hints, not part of the variable declaration syntax. They should either be omitted or placed after the value (though current parser doesn't use them).

#### 2. **Boolean Literal Casing** ❌
- **Wrong:** `is_prime: Boolean = True`
- **Correct:** `is_prime: Boolean = TRUE`
- **Explanation:** CNS uses uppercase `TRUE` and `FALSE` for boolean literals, not Python-style capitalization.

#### 3. **Comparison Operator** ❌
- **Wrong:** `If num % i == 0`
- **Correct:** `If num % i = 0`
- **Explanation:** CNS uses single `=` for both assignment context and comparison, not `==`.

#### 4. **Multi-line Then: Clause** ❌
- **Wrong:**
  ```
  Then: is_prime becomes False
       go to End
  ```
- **Correct:** Need separate steps or control flow logic
- **Explanation:** The `Then:` clause should contain a single action. Multiple actions require either:
  - Multiple `Then:` lines (for state changes)
  - Separate steps with control flow
  - In this case, we need an intermediate check

#### 5. **Error Block Structure** ❌
- **Wrong:**
  ```
  Error:
    Return False
    Effect: none
    Because: Handle case where num < 2 although assumed valid
  ```
- **Correct:** Error blocks are for exception handling, not input validation
- **Explanation:** The Error block is meant for runtime errors (network failures, file I/O errors), not logical flow. Input validation should be done in regular steps.

### Corrected Version ✅

```cns
Story: Determine if a given integer is a prime number

Given:
  num: Integer = 17
  is_prime: Boolean = TRUE
  i: Integer = 2

Step 1 → If i * i > num
  Because: All potential factors up to sqrt(num) have been checked
  Then: go to End
  Otherwise: go to Step 2

Step 2 → If num % i = 0
  Because: i is a divisor, so num is not prime
  Then: is_prime becomes FALSE
  Otherwise: go to Step 3

Step 3 → If is_prime = FALSE
  Because: Already determined num is not prime
  Then: go to End
  Otherwise: go to Step 4

Step 4 → Increment i by 1
  Because: Move to the next potential factor
  Then: i becomes i + 1
  Then: repeat from Step 1

End: Return is_prime
```

### Execution Results

**Original (Failed):**
```
ERROR: Could not evaluate 'num' - Unknown variable or expression: num
```

**Corrected (Success):**
```
=== Executing Story: Determine if a given integer is a prime number ===

Given:
  num: Integer = 17
  is_prime: Boolean = T
  i: Integer = 2

Execution Trace:

Step 1: If i * i > num
  Because: All potential factors up to sqrt(num) have been checked
  State: i=2, is_prime=T, num=17
  -> Going to Step 2

Step 2: If num % i = 0
  Because: i is a divisor, so num is not prime
  State: i=2, is_prime=T, num=17
  -> Going to Step 3

Step 3: If is_prime = FALSE
  Because: Already determined num is not prime
  State: i=2, is_prime=T, num=17
  -> Going to Step 4

Step 4: Increment i by 1
  Because: Move to the next potential factor
  Then: i becomes i + 1
  Then: repeat from Step 1
  State: i=3, is_prime=T, num=17

=== End ===
Return: T
Because: computation complete
```

✅ **Result:** TRUE (17 is prime)

## Key Takeaways for LLM Training

### Critical Syntax Rules

1. **Variable Declarations:**
   ```
   variable_name: Type = value
   ```
   - No semantic tags in current implementation
   - Use uppercase TRUE/FALSE for booleans

2. **Comparison Operators:**
   - Use `=` not `==`
   - Use `>`, `<`, `>=`, `<=` as expected
   - Modulo: `%` works correctly

3. **Control Flow:**
   - `Then: action` should be single action per line
   - `Then: variable becomes value` for state changes
   - `Then: go to End` or `Then: repeat from Step N`
   - Multiple `Then:` lines are allowed for multiple actions

4. **Error Block Usage:**
   - Use for exception handling (I/O failures, network errors)
   - NOT for input validation or logical branching
   - Format:
     ```
     Error:
       Return error_value
       Effect: error_handling_action
       Because: explanation
     ```

5. **Boolean Values:**
   - `TRUE` and `FALSE` (uppercase)
   - Not `True`, `true`, `False`, or `false`

### Pattern: Early Exit with Flag Check

When you need to exit early after setting a flag:

```cns
Step N → If condition_that_invalidates
  Because: why this matters
  Then: flag becomes FALSE
  Otherwise: continue_to_next

Step N+1 → If flag = FALSE
  Because: early exit when invalid
  Then: go to End
  Otherwise: continue_processing
```

This is better than trying to combine state change + exit in one step.

## Recommendations for Prompt Improvement

### Add to Template:

1. **Explicit Syntax Examples:**
   ```markdown
   ### CRITICAL Syntax Rules
   
   ✅ **CORRECT:**
   - `n: Integer = 5`
   - `is_prime: Boolean = TRUE`
   - `If x = 0`
   - `Then: y becomes y + 1`
   
   ❌ **WRONG:**
   - `n: Integer [≥ 1] = 5` (no semantic tags)
   - `is_prime: Boolean = True` (use TRUE not True)
   - `If x == 0` (use = not ==)
   - `Then: y becomes y + 1` followed by `go to End` on same line
   ```

2. **Common Mistakes Section:**
   ```markdown
   ### Common Mistakes to Avoid
   
   1. **Don't use Python syntax**: Use `=` not `==`, `TRUE` not `True`
   2. **Don't combine actions**: Each `Then:` is one action
   3. **Don't use Error for logic**: Error blocks are for exceptions only
   4. **Don't add semantic tags**: Current parser doesn't support `[≥ 2]` syntax
   ```

3. **Pattern Library:**
   - Early exit pattern (shown above)
   - Flag-based termination
   - Counter-based loops
   - Divisibility checking

## Next Steps

- [ ] Update `prompts/cns-generation-template.md` with corrections
- [ ] Add "Common Mistakes" section to template
- [ ] Create quick-reference syntax card
- [ ] Test with more LLMs (GPT-4, Claude)
- [ ] Build error message feedback loop
