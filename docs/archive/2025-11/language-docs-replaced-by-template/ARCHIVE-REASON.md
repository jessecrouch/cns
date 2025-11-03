# Archive Reason: Language Documentation Consolidation

**Date:** 2025-11-03  
**Action:** Archived docs/language/ directory  
**Reason:** Consolidation to single source of truth

---

## Why This Was Archived

CNS had two competing documentation sources:

1. **prompts/detailed-template.md** - Single comprehensive LLM reference (849 lines)
2. **docs/language/** - Multi-file documentation (6 files, ~2500 lines)

**Problem:**
- ~90% content overlap
- Maintenance burden (updating same info in multiple places)
- Risk of contradictory information
- docs/language/README.md already deferred to the template

**Solution:**
- Made `prompts/detailed-template.md` the **single source of truth**
- Archived docs/language/ here for reference

---

## What Was in docs/language/

- **README.md** - Overview and navigation (already pointed to template)
- **SYNTAX.md** - Complete syntax reference
- **CONTROL-FLOW-RULES.md** - Loop and jump patterns
- **EXPRESSION-LIMITATIONS.md** - What works/doesn't work
- **COMMON-PATTERNS.md** - Reusable code templates
- **LLM-COMMON-MISTAKES.md** - Known errors and fixes

**All content now consolidated in:** `prompts/detailed-template.md`

---

## Benefits of Consolidation

✅ Single file to maintain  
✅ No duplicate content to keep in sync  
✅ Faster LLM context loading  
✅ Clear hierarchy: template IS the spec  
✅ LLM-first design (no human-oriented TOCs)

---

## Philosophy

**CNS is LLM-first.** We don't need separate human documentation.

The template serves both purposes:
- **For LLMs:** Code generation reference with lookup tables
- **For humans:** Complete syntax and pattern guide

---

**If you need this content:** It's all in `prompts/detailed-template.md`
