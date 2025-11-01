# CNS Development Checklist

## CNSC-First Policy (v1.1.0+)

**ALL NEW EXAMPLES MUST BE WRITTEN IN CNSC FORMAT FIRST.**

**Mandatory Guidelines:**
- Examples >30 lines: `.cnsc` required, `.cns` optional
- Examples <30 lines: Use judgment (prefer `.cnsc` for consistency)
- Agent code: `.cnsc` only
- Test cases: `.cnsc` for better coverage
- Documentation snippets: Show `.cnsc` syntax prominently

**Why:** 62% token reduction = more examples in context, better LLM performance, cheaper API calls

**See:** [CNSC-COMPACT.md](../guides/CNSC-COMPACT.md) and [ROADMAP.md](./ROADMAP.md) CNSC-First section

---

## Feature Evaluation Framework

Before adding ANY feature to CNS, it must pass this evaluation checklist. CNS is designed to be the **best language for LLMs to semi-autonomously code with** - every decision must align with that goal.

---

## The Checklist

### 1. Consistency Test ❌ FAIL = REJECT

**Question**: Does this create optional variations in syntax?

- ❌ **FAIL**: Optional sections, alternative keywords, "style preferences"
- ✅ **PASS**: Single canonical way to express the concept

**Why it matters**: LLMs learn from training data. Inconsistent examples → inconsistent output. Optional features teach LLMs that "this can be skipped" → lower quality code.

**Example**:
- ❌ Optional Planning section - some examples have it, some don't
- ✅ Required Because clauses - every step has one, always

---

### 2. Cognitive Load Test ❌ FAIL = REJECT

**Question**: Does this add keywords/concepts LLMs must memorize?

- ❌ **FAIL**: New keywords for functionality that could extend existing patterns
- ✅ **PASS**: Reuses existing keywords/patterns in new contexts

**Why it matters**: Every new keyword is cognitive overhead. LLMs have finite "working memory" in context windows. Minimize vocabulary, maximize expressiveness.

**Example**:
- ❌ Adding `Inputs:` and `Outputs:` for functions (new keywords)
- ✅ Using `Given:` for parameters, `End: Return` for outputs (reuse)

---

### 3. Explicitness Test ❌ FAIL = REJECT

**Question**: Does this feature have implicit behavior or hidden side effects?

- ❌ **FAIL**: Magic defaults, implicit conversions, hidden control flow
- ✅ **PASS**: All behavior is declared upfront and traceable

**Why it matters**: LLMs struggle with implicit behavior. Explicit = predictable = correct code generation.

**Example**:
- ❌ Functions auto-returning last expression (implicit)
- ✅ Functions require `End: Return <value>` (explicit)

---

### 4. Pattern Extension Test ✅ PASS = GOOD SIGN

**Question**: Does this extend existing CNS patterns consistently?

- ✅ **GOOD**: Uses Story/Given/Step/End structure
- ✅ **GOOD**: Follows Because/Then/Effect patterns
- ✅ **GOOD**: Fits naturally with existing syntax

**Why it matters**: Extending patterns = LLM reuses learned structures. New patterns = new learning required.

**Example**:
- ✅ Functions as `Story: FuncName (function)` - extends Story pattern
- ✅ CNSC `G:` extending `Given:` - consistent abbreviation strategy

---

### 5. Token Efficiency Test ✅ PASS = GOOD SIGN

**Question**: Does this reduce token count or improve context usage?

- ✅ **GOOD**: Fewer tokens for same expressiveness
- ⚠️ **NEUTRAL**: Same tokens, but necessary functionality
- ❌ **BAD**: More tokens without measurable benefit

**Why it matters**: Context windows are precious. More tokens = fewer examples, shorter programs, less context for LLM decisions.

**Example**:
- ✅ CNSC format: 70-76% token reduction
- ⚠️ Functions: More tokens, but necessary for modularity
- ❌ Optional Planning section: More tokens, unproven benefit

---

### 6. Accuracy Test ✅ MEASURE BEFORE ADDING

**Question**: Does this improve LLM code generation accuracy?

- ✅ **PROVEN**: Empirical testing shows improvement
- ⚠️ **HYPOTHESIS**: Plausible but needs testing
- ❌ **UNPROVEN**: No evidence of benefit

**Why it matters**: Don't add features based on intuition. Measure first. If it doesn't help LLMs generate better code, why add complexity?

**How to test**:
1. Generate 20 programs WITHOUT the feature (baseline)
2. Generate 20 programs WITH the feature (experimental)
3. Measure: first-pass validation rate, execution success rate, correctness
4. If experimental ≤ baseline: **REJECT THE FEATURE**

**Example**:
- ✅ CNSC: Tested with Grok 2, equal success rate, better performance
- ⚠️ Planning section: Hypothesis only, needs A/B testing
- ✅ Because clauses: Core to CNS philosophy, proven necessary

---

## Feature Decision Matrix

| Feature | Consistent? | Low Cognitive Load? | Explicit? | Extends Patterns? | Token Efficient? | Proven Accurate? | **DECISION** |
|---------|-------------|---------------------|-----------|-------------------|------------------|------------------|--------------|
| **CNSC Format** | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | **APPROVED** |
| **Functions** | ✅ | ✅ | ✅ | ✅ | ⚠️ | ⚠️ Needs test | **IMPLEMENT & TEST** |
| **Optional Planning** | ❌ | ❌ | ✅ | ❌ | ❌ | ❌ | **REJECTED** |
| **Because clauses** | ✅ | ⚠️ | ✅ | ✅ | ❌ | ✅ | **CORE FEATURE** |

---

## The "Simplicity First" Principle

**Guiding Rule**:
> Every feature must make CNS **simpler** for LLMs to generate, not more complex.

If a feature adds complexity, it must provide **measurable, significant benefits** to justify the cost.

---

## Feature Proposal Template

When proposing a new feature, fill out this template:

### Feature Name: [Name]

**Description**: [1-2 sentence description]

**Motivation**: [Why is this needed?]

**Checklist Evaluation**:

1. **Consistency**: Does this create optional variations?
   - [ ] No variations (PASS)
   - [ ] Creates variations (FAIL)

2. **Cognitive Load**: Does this add new keywords?
   - [ ] Reuses existing patterns (PASS)
   - [ ] Adds N new keywords (JUSTIFY)

3. **Explicitness**: Any implicit behavior?
   - [ ] Fully explicit (PASS)
   - [ ] Has implicit behavior (FAIL)

4. **Pattern Extension**: Does this fit CNS patterns?
   - [ ] Extends Story/Given/Step/End (GOOD)
   - [ ] Extends Because/Then/Effect (GOOD)
   - [ ] New pattern (JUSTIFY)

5. **Token Efficiency**: Impact on token count?
   - [ ] Reduces tokens (GOOD)
   - [ ] Neutral/necessary (ACCEPTABLE)
   - [ ] Increases tokens (JUSTIFY)

6. **Accuracy**: Proven to help LLMs?
   - [ ] Empirically tested and proven (APPROVED)
   - [ ] Hypothesis, needs testing (TEST FIRST)
   - [ ] No evidence (REJECT)

**Syntax Example**:
```cns
[Show how this feature looks in CNS]
```

**CNSC Example** (if applicable):
```cnsc
[Show compact format]
```

**Test Plan**: [How will you measure if this helps LLMs?]

**Decision**: [APPROVED / TEST FIRST / REJECTED]

---

## Historical Decisions

### ✅ APPROVED: CNSC Compact Format

**Date**: 2025-10-30

**Rationale**:
- ✅ Consistent: Single canonical compact form
- ✅ Low cognitive load: Standard abbreviations (G: S1→ E:)
- ✅ Explicit: Maps 1:1 to verbose CNS
- ✅ Extends patterns: Abbreviates existing keywords consistently
- ✅ Token efficient: 70-76% reduction proven
- ✅ Accurate: Grok 2 testing showed equal success rate

**Outcome**: Implemented and documented in CNSC-COMPACT.md

---

### ❌ REJECTED: Optional Planning Section

**Date**: 2025-10-30

**Rationale**:
- ❌ Inconsistent: Creates optional variation
- ❌ Cognitive load: LLMs must decide "do I include this?"
- ✅ Explicit: Would be explicit if included
- ❌ Pattern extension: Adds new top-level section
- ❌ Token efficiency: Increases tokens
- ❌ Accuracy: No evidence it helps (hypothesis only)

**Alternative**: Use descriptive Story lines and detailed Because clauses instead

**Outcome**: Documented in discussion, not implemented

---

### ⚠️ IN PROGRESS: Functions

**Date**: 2025-10-30

**Status**: Design phase, pending implementation and testing

**Rationale**:
- ✅ Consistent: Single way to define functions
- ✅ Low cognitive load: Reuses Story/Given/Step/End
- ✅ Explicit: Requires explicit return values
- ✅ Extends patterns: Functions are Stories with (function) tag
- ⚠️ Token efficiency: Necessary feature, enables modularity
- ⚠️ Accuracy: Hypothesis - needs empirical testing

**Test Plan**:
1. Implement minimal syntax
2. Generate 20 programs with functions using LLM
3. Measure: validation rate, execution success, correctness
4. Compare to baseline programs without functions

**Decision**: PROCEED with implementation, TEST before declaring success

---

## Review Process

All new features must:

1. **Pass checklist evaluation** (documented in this file)
2. **Be reviewed by maintainers** (GitHub PR process)
3. **Include test plan** (how will we measure impact?)
4. **Update documentation** (guides, templates, examples)

**For breaking changes or major features**:
- Post proposal in GitHub Issues for community feedback
- Wait 48 hours for discussion
- Implement only after consensus

---

## References

- [LLM Integration Guide](./LLM-INTEGRATION.md) - How LLMs use CNS
- [CNSC Compact Format](../guides/CNSC-COMPACT.md) - Token optimization
- [Testing Guide](./TESTING.md) - How to measure LLM performance

---

**Remember**: CNS exists to make LLM code generation better. Every feature decision must serve that goal.
