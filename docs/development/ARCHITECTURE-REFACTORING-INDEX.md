# Architecture Refactoring Documentation Index

**Decision Date:** November 3, 2025  
**Status:** 📋 Planning Phase  
**Priority:** 🔴 CRITICAL - Blocking all Phase D features

---

## Quick Start

**If you only read one document, read this:**
- 📄 **[CRITIQUE-SUMMARY.md](CRITIQUE-SUMMARY.md)** - Executive summary (8.6 KB)

**If you're doing the refactoring work:**
- 📋 **[REFACTORING-STEP-BY-STEP.md](REFACTORING-STEP-BY-STEP.md)** - Surgical guide (15 KB) ⭐ START HERE

---

## All Documents

### Understanding the Problem

1. **[CRITIQUE-SUMMARY.md](CRITIQUE-SUMMARY.md)** (8.6 KB)
   - Executive summary and TL;DR
   - What's wrong, why it matters, what to do
   - Q&A section
   - **Read this first**

2. **[ARCHITECTURE-CRITIQUE.md](ARCHITECTURE-CRITIQUE.md)** (9.7 KB)
   - Deep technical analysis
   - God function anti-pattern explanation
   - Order-dependent precedence issues
   - Guard explosion problem
   - Comparison to other languages

3. **[ARCHITECTURE-COMPARISON.txt](ARCHITECTURE-COMPARISON.txt)** (13 KB)
   - Visual diagrams of current vs. better architecture
   - Flow charts showing tokenizer/parser/evaluator phases
   - Side-by-side comparisons
   - Restaurant kitchen analogy

### How to Fix It

4. **[REFACTORING-GUIDE.md](REFACTORING-GUIDE.md)** (9.1 KB)
   - Concrete code examples
   - Before/after comparisons
   - Three refactoring options (helper functions, Pratt parser, full rewrite)
   - Side-by-side operator migration examples

5. **[REFACTORING-STEP-BY-STEP.md](REFACTORING-STEP-BY-STEP.md)** (15 KB) ⭐
   - **THE IMPLEMENTATION GUIDE**
   - Week-by-week breakdown
   - Step-by-step instructions
   - Surgical approach with testing checklist
   - Success metrics and rollback plan
   - **Use this to actually do the work**

### Project Planning

6. **[ROADMAP.md](ROADMAP.md)** (Updated)
   - Phase D development ON HOLD
   - Architecture refactoring as blocking priority
   - Timeline and success criteria

---

## The Problem in One Sentence

**The interpreter has two "god functions" (eval-expr at 562 lines and apply-effect at 871 lines) that use order-dependent pattern matching, making every new feature harder to add.**

---

## The Solution in Three Steps

### Week 1: Extract Helpers (4-6 hours)
- Extract `quoted-string-p`, `filepath-p`, `datetime-expr-p`
- Create `should-skip-operator-p` combining all guards
- Eliminate ~100 lines of duplicated code

### Week 2-3: Extract Handlers (8-16 hours)
- Create `try-binary-operator` helper
- Extract `eval-arithmetic`, `eval-comparison` functions
- Move operator logic out of giant `cond`

### Week 3-4: Precedence Table (16-24 hours)
- Build `*operator-precedence*` table with explicit levels
- Implement precedence-based parser
- Make precedence explicit instead of implicit

---

## Critical Rules

### ⚠️ MUST FOLLOW THESE RULES

1. **Test after EVERY change** - Run `./test-all-examples.sh`
2. **One change at a time** - Extract one helper, test, commit, repeat
3. **If tests fail, REVERT** - Don't fix forward, go back to working state
4. **Commit frequently** - Every working change gets a commit
5. **Document as you go** - Update comments

### Baseline Test Results

Before starting, verify:
```bash
./test-all-examples.sh
```

**Expected:** 36 PASS, 0 FAIL, 1 TIMEOUT

If you don't have this, **STOP** and fix issues first.

---

## Timeline & Effort

| Phase | Time | Benefit |
|-------|------|---------|
| Week 1: Helpers | 4-6 hours | Eliminate duplication, cleaner code |
| Week 2-3: Handlers | 8-16 hours | Reusable operator logic |
| Week 3-4: Precedence | 16-24 hours | Explicit precedence, easy to add operators |
| **Total** | **2-4 weeks** | **Adding operator: 30 min → 2 min** |

---

## Success Metrics

### Before Refactoring
- ❌ God functions: 1,433 lines (31% of codebase)
- ❌ Duplicated guards: ~100 lines
- ❌ Adding operator: 30 minutes, 20+ lines
- ❌ Precedence: Hidden in code order

### After Refactoring
- ✅ Helper functions: ~100 lines
- ✅ Core parsing: ~400 lines
- ✅ Adding operator: 2 minutes, 1 line
- ✅ Precedence: Explicit table

---

## Resources

### Learning Materials
- **Crafting Interpreters** by Robert Nystrom (free online)
  - Chapter on Pratt parsing / operator precedence
- **Pratt parser** - Google "top-down operator precedence"

### Internal Docs
- All 5 documents in this directory
- `ROADMAP.md` for project timeline
- Git history for examples of incremental refactoring

---

## Questions?

**Q: Is this really necessary?**  
A: Yes. You're already feeling the pain with operator precedence bugs and guard explosion. It will only get worse.

**Q: Can we do Phase D features first?**  
A: Not recommended. Each new feature will require updating all operator guards. Better to fix the architecture first.

**Q: What if I break something?**  
A: That's why we test after EVERY change and commit frequently. Revert broken commits immediately.

**Q: How long will this really take?**  
A: If you follow the surgical approach: 2-4 weeks. If you rush: months of debugging.

**Q: Can I skip the testing?**  
A: **NO.** Testing is not optional.

---

## Next Steps

1. ✅ Read CRITIQUE-SUMMARY.md (understand the problem)
2. ✅ Read REFACTORING-STEP-BY-STEP.md (understand the solution)
3. ⏳ Run baseline tests (establish starting point)
4. ⏳ Start Week 1: Extract `quoted-string-p` (first small step)
5. ⏳ Follow step-by-step guide religiously

---

**Remember:** This is not a rewrite. This is surgical refactoring. Slow is smooth, smooth is fast.

**Last Updated:** November 3, 2025
