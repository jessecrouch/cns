# AGENTS.md: System Prompt for CNS Development

**Purpose**: Instructions for AI agents developing the CNS language and tooling.

**Scope**: Development workflow, testing requirements, code quality standards, and best practices for working on the CNS project itself.

**Not in scope**: CNS language syntax/patterns (see `docs/language/`), CNS examples (see `examples/`).

---

## Table of Contents

1. [Core Principles](#core-principles)
2. [Before You Start](#before-you-start)
3. [Development Workflow](#development-workflow)
4. [Testing Requirements](#testing-requirements)
5. [Code Quality Standards](#code-quality-standards)
6. [Repository Structure](#repository-structure)
7. [Common Tasks](#common-tasks)
8. [Code Bloat Management](#code-bloat-management)

---

## Core Principles

### 1. LLM-First Development

CNS is **optimized for LLM comprehension**. Every decision should prioritize:
- **Clarity** - Explicit causality, narrative structure  
- **Learnability** - Patterns over syntax, examples over specs
- **Predictability** - Consistent rules, helpful error messages

⚠️ **Expression Rules for LLMs** (with Auto-Fix Support)

CNS now automatically fixes common expression errors with helpful warnings:

1. **Variable-First (Auto-Fixed)**: `3 * n` → `n * 3` (now works, but shows warning!)
2. **One Operator Per Line**: Split `n * 3 + 1` into two steps (not auto-fixed)
3. **No Parentheses**: Use temp variables instead of `(a + b) * c` (not auto-fixed)
4. **Best Practice**: Write correct expressions from the start to avoid warnings

✨ **Auto-Fix Support** (as of 2025-11-02):
- Operators: `*`, `-`, `/`, `%` automatically detect and fix literal-first order
- Example: `result becomes 3 * n` now works (was NIL before)
- Warning message guides you to correct pattern
- See `examples/features/test-expression-autofix.cns` for demonstrations

📖 See [`docs/language/EXPRESSION-LIMITATIONS.md`](../language/EXPRESSION-LIMITATIONS.md) for complete reference

### 2. Test-Driven Changes

**NEVER commit code without testing first:**
```bash
# REQUIRED before every commit
./test-all-examples.sh           # All 42 examples must pass
./cns-validate examples/new.cns  # Validate syntax
./tests/run-all-tests.sh         # Regression tests
```

### 3. Documentation as Code

- Keep docs synchronized with implementation
- Update relevant docs in the **same commit** as code changes
- Reference existing docs instead of duplicating content
- Archive completed session docs after integration

### 4. Context Management

**The repository is an LLM context window.** Keep it lean:
- Remove obsolete documentation after completion
- Avoid code duplication in docs (reference instead)
- Delete redundant examples that don't add unique coverage
- Archive historical artifacts (see `docs/archive/`)
- Compress verbose documentation while preserving essential information

---

## Before You Start

### Essential Reading (in order)

1. **`docs/language/SYNTAX.md`** - Complete CNS syntax reference
2. **`docs/language/COMMON-PATTERNS.md`** - Reusable code patterns
3. **`docs/language/EXPRESSION-LIMITATIONS.md`** - Critical parser limitations (READ THIS!)
4. **`docs/language/CONTROL-FLOW-RULES.md`** - If/Otherwise and loop patterns
5. **`docs/development/TESTING.md`** - Test infrastructure
6. **`docs/development/LISP-DEBUGGING-GUIDE.md`** - Required if modifying `src/cns.lisp`

### Quick Context

**What is CNS?**
- A narrative programming language designed for LLM comprehension
- Explicit causality (`Because:`), step-by-step execution
- Pattern-based syntax optimized for token efficiency
- Lisp-based interpreter (SBCL/CCL), ~4000 lines

**Current state:**
- 42 working examples (100% pass rate)
- Comprehensive test suite (`tests/`)
- Three-tier example structure (core/features/advanced)
- Iteration safety (10,000 step limit with `--max-iterations`)

**Recent work:**
- Phase 1-3: Repository reorganization (10MB → 3.5MB, 65% reduction)
- Parser fixes for End section and effect keywords in Then clauses
- LLM-first documentation (EXPRESSION-LIMITATIONS.md, CONTROL-FLOW-RULES.md)

---

## Development Workflow

### Standard Development Cycle

```bash
# 1. Create feature branch (if multi-commit work)
git checkout -b feature/your-feature-name

# 2. Make changes to src/cns.lisp or other files
# ... edit files ...

# 3. Test immediately (don't batch!)
sbcl --non-interactive --load src/cns.lisp  # Syntax check
./cns-run examples/hello.cns                # Smoke test
./test-all-examples.sh                      # Full validation

# 4. If tests fail, fix immediately before proceeding
# ... fix bugs ...

# 5. Update documentation in SAME commit as code changes
# ... update docs/language/*.md or docs/development/*.md ...

# 6. Run full test suite before commit
./test-all-examples.sh           # Must be 42/42 passing
./tests/run-all-tests.sh         # Regression tests
./cns-validate examples/*.cns    # Validate all syntax

# 7. Commit with descriptive message
git add <relevant files only>    # Don't use `git add .` blindly!
git commit -m "Add feature X: <concise description>

- What changed (code changes)
- Why (motivation)
- Impact (what works now that didn't before)
- Tests: 42/42 examples pass"

# 8. For multi-commit features, merge when complete
git checkout main
git merge feature/your-feature-name
```

### Modifying the Interpreter (`src/cns.lisp`)

**CRITICAL: Read `docs/development/LISP-DEBUGGING-GUIDE.md` first!**

```bash
# 1. Backup before modifying
cp src/cns.lisp src/cns.lisp.backup-$(date +%Y%m%d-%H%M%S)

# 2. Make small, incremental changes
# ... edit ONE function at a time ...

# 3. Test after EVERY change
sbcl --non-interactive --load src/cns.lisp
# Look for "Unmatched parenthesis" or "Undefined function" errors

# 4. If Lisp errors occur:
#    a. Use binary search to isolate the problematic section
#    b. Check parentheses balance with editor tooling
#    c. Add tracing: (format t "~%DEBUG: var=~A~%" var)
#    d. Test in REPL: sbcl --load src/cns.lisp
#    e. See LISP-DEBUGGING-GUIDE.md for detailed methodology

# 5. Once Lisp loads successfully, test CNS execution
./cns-run examples/factorial.cns
./test-all-examples.sh

# 6. Keep backup until feature is complete and tested
# Delete backup after successful commit:
rm src/cns.lisp.backup-*
```

### Adding New Features

**Standard feature development checklist:**

- [ ] Research: Check existing code for similar patterns
- [ ] Design: Document approach in `docs/development/` (if complex)
- [ ] Implement: Small incremental changes with frequent testing
- [ ] Test: Add regression test in `tests/` or example in `examples/`
- [ ] Document: Update `docs/language/SYNTAX.md` or relevant guide
- [ ] Validate: Run full test suite (`./test-all-examples.sh`)
- [ ] Commit: Descriptive message with test results
- [ ] Archive: Move session docs to `docs/archive/` if temporary

---

## Testing Requirements

### Before Every Commit (MANDATORY)

```bash
# Test 1: All examples pass (REQUIRED)
./test-all-examples.sh
# Expected: 42/42 examples pass (100%)

# Test 2: Validation passes (REQUIRED)
./cns-validate examples/*.cns
# Expected: All files validate without errors

# Test 3: Regression tests (RECOMMENDED)
./tests/run-all-tests.sh
# Expected: All test suites pass
```

### Adding New Tests

**For new features:**
1. Add working example to `examples/features/test-<feature>.cns`
2. Add `# STARTER` tag at top for test discovery
3. Run `./test-all-examples.sh` to verify inclusion

**For bug fixes:**
1. Create minimal reproduction in `tests/` directory
2. Run `./tests/run-validation-tests.sh` to verify fix
3. Document in commit message

**For edge cases:**
1. Add to `tests/regression-tests.lisp` (Lisp-based tests)
2. Document expected behavior in comments
3. Run `sbcl --load tests/regression-tests.lisp`

### LLM Code Generation Testing (REQUIRED for template changes)

**When to run LLM tests:**
- After modifying prompt templates (`prompts/*.md`)
- After changing language syntax or built-in functions
- Before claiming "LLM-friendly" improvements
- When adding new CNS features that LLMs need to learn

**How to run LLM tests:**
```bash
# Use the automated LLM test harness
python3 scripts/llm-tester.py \
  --task "Write a CNS program that calculates factorial of 5" \
  --template prompts/detailed-template.md \
  --provider grok \
  --retries 3

# The script will:
# 1. Generate CNS code using the LLM
# 2. Validate the generated code
# 3. Execute the code
# 4. Save results to tests/llm-tests/results/
# 5. Save generated code to tests/llm-tests/generated/
```

**Supported LLM providers:**
- `grok` / `xai` - xAI Grok models (requires `GROK_API_KEY` in `.env`)
- `openai` - OpenAI GPT models (requires `OPENAI_API_KEY` in `.env`)
- `claude` / `anthropic` - Anthropic Claude (requires `ANTHROPIC_API_KEY` in `.env`)
- `openrouter` - OpenRouter unified API (requires `OPENROUTER_API_KEY` in `.env`)

**Environment setup:**
```bash
# Create .env file in project root
echo "GROK_API_KEY=your-key-here" >> .env
echo "OPENAI_API_KEY=your-key-here" >> .env
echo "ANTHROPIC_API_KEY=your-key-here" >> .env

# Install dependencies
pip install requests
```

**Test scenarios to verify:**
1. **Basic math** - Factorial, fibonacci, prime checking
2. **File I/O** - Reading/writing files, CSV operations
3. **HTTP servers** - Multi-route servers with request logging
4. **Control flow** - If/Otherwise with goto/repeat
5. **String operations** - SPLIT, JOIN, UPPERCASE, LOWERCASE, TRIM

**Success criteria:**
- ✅ Validation passes (0 errors)
- ✅ Execution succeeds (correct output)
- ✅ Uses correct built-in functions (TIMESTAMP not NOW)
- ✅ No hallucinated syntax (no array indexing, no undefined functions)

**Failure analysis:**
If LLM tests fail, check:
1. Is the function documented in the template's lookup table?
2. Are there conflicting examples in the documentation?
3. Does the template show explicit ✅/❌ examples?
4. Is deprecated syntax (CNSC) still referenced anywhere?

**Results location:**
- Generated code: `tests/llm-tests/generated/`
- Test results: `tests/llm-tests/results/` (JSON format)
- Historical tests: `tests/grok-iterations/` (manual test archives)

### Test Coverage Strategy

**Current coverage (as of 2025-11-02):**
- Core features: 11 examples (factorial, fibonacci, GCD, etc.)
- Language features: 25 examples (HTTP, DB, file I/O, etc.)
- Advanced patterns: 6 examples (webservers, agents, etc.)

**When adding new tests:**
- **core/**: Only add for fundamental language features
- **features/**: Add for each new effect, syntax, or API
- **advanced/**: Add for complex multi-feature patterns
- **tests/**: Add for edge cases, regressions, validation

---

## Code Quality Standards

### 1. No Duplication

**Bad:** Documenting CNS syntax in multiple places
```
❌ AGENTS.md contains syntax reference
❌ README.md duplicates examples
❌ Session docs duplicate final documentation
```

**Good:** Single source of truth with references
```
✅ SYNTAX.md is the syntax reference
✅ README.md links to SYNTAX.md
✅ Session docs archived after integration
```

### 2. Lean Context

**Repository size targets:**
- **Current**: 3.5MB (down from 10MB after Phase 1-2)
- **Target**: <5MB total (strict control on bloat)
- **Method**: Regular cleanup, archive completed work, compress verbose docs

**File count targets:**
- **Examples**: 42 files (consolidated from 92)
- **Docs**: ~25 files (down from 40+)
- **Tests**: ~15 files (core test coverage)

### 3. Clear Documentation

**Session documentation** (temporary, for active work):
- Created during development to track progress
- Moved to `docs/archive/` after completion
- Examples: `PHASE-1-REORGANIZATION-COMPLETE.md`

**Reference documentation** (permanent):
- `docs/language/`: CNS syntax, patterns, limitations
- `docs/development/`: Testing, debugging, roadmap
- `docs/guides/`: User-facing guides
- `examples/README.md`: Pattern guide with examples

### 4. Git Hygiene

**Commit messages:**
```
Good format:
"Add iteration safety with --max-iterations flag

- Implemented *max-iterations* global (default 10,000)
- Added counter tracking to prevent infinite loops  
- Fast failure (~1 sec vs 60 sec timeout)
- Tests: 42/42 examples pass, new flag documented"

Bad format:
"Fixed stuff"
"WIP"
"Update code"
```

**What to commit:**
- Relevant source files only (no `git add .` without review!)
- Updated documentation in same commit as code changes
- Test additions for new features

**What NOT to commit:**
- Backup files (`.backup-*`, `*.bak`)
- Session logs or debug output
- Temporary test files
- Commented-out code blocks (delete instead)

---

## Repository Structure

### Source Code (`src/`)

| File | Purpose | When to modify |
|------|---------|----------------|
| `cns.lisp` | Main interpreter (4000 lines) | Adding features, fixing bugs |
| `cns-validator.lisp` | Syntax validator | Adding validation rules |
| `cns-run` | CLI runner script | Adding flags, options |
| `cns-validate` | Validation CLI | Validation flags |

**Before modifying `cns.lisp`**: Read `docs/development/LISP-DEBUGGING-GUIDE.md`

### Documentation (`docs/`)

| Directory | Purpose | Update when |
|-----------|---------|-------------|
| `docs/language/` | CNS syntax, patterns, rules | Language changes |
| `docs/development/` | Testing, debugging, roadmap | Development process changes |
| `docs/guides/` | User-facing guides | Feature additions |
| `docs/install/` | Setup instructions | Dependency changes |
| `docs/archive/` | Historical session docs | Archiving completed work |

### Examples (`examples/`)

| Directory | Purpose | Add when |
|-----------|---------|----------|
| `examples/core/` | Fundamental patterns (11 files) | Core language features |
| `examples/features/` | Feature demos (25 files) | New effects or APIs |
| `examples/advanced/` | Complex patterns (6 files) | Multi-feature orchestration |

See `examples/README.md` for complete pattern guide.

### Tests (`tests/`)

| File/Directory | Purpose | Add when |
|----------------|---------|----------|
| `tests/llm-tests/` | LLM-generated test cases | Testing LLM output |
| `tests/if-otherwise-tests/` | Control flow tests | If/Otherwise changes |
| `tests/grok-iterations/` | Iterative refinement tests | Testing corrections |
| `tests/regression-tests.lisp` | Lisp-based regression suite | Bug fixes |

Run with: `./tests/run-all-tests.sh` or `./test-all-examples.sh`

### Scripts (`scripts/`)

Development utilities (test runners, dataset generation, etc.). Generally don't modify unless adding new tooling.

---

## Common Tasks

### Task 1: Fix a Bug

```bash
# 1. Create minimal reproduction
cat > test-bug.cns << 'EOF'
Story: Reproduce Bug
Given:
  x: Integer = 5
Step 1 → Trigger bug
  Then: x becomes x * 3
End: Return x
EOF

# 2. Verify bug exists
./cns-run test-bug.cns  # Should fail or produce wrong output

# 3. Locate bug in src/cns.lisp
#    - Search for relevant function (e.g., eval-expr, interpret-single-story)
#    - Add tracing: (format t "~%DEBUG: x=~A~%" x)
#    - Test incrementally

# 4. Fix bug (small change!)
# ... edit src/cns.lisp ...

# 5. Test fix
sbcl --non-interactive --load src/cns.lisp  # Lisp syntax check
./cns-run test-bug.cns                       # Verify fix
./test-all-examples.sh                       # Ensure no regressions

# 6. Add regression test
mv test-bug.cns tests/regression-bug-fix-description.cns

# 7. Update documentation if behavior changed
# ... edit relevant docs/language/*.md ...

# 8. Commit with test results
git add src/cns.lisp tests/regression-bug-fix-description.cns docs/...
git commit -m "Fix bug in expression evaluation

- Bug: Literal-first multiplication returned NIL
- Fix: Added variable-first check in eval-expr
- Tests: 42/42 examples pass, new regression test added
- Docs: Updated EXPRESSION-LIMITATIONS.md with workaround"
```

### Task 2: Add New Effect

```bash
# 1. Design effect syntax (check SYNTAX.md for patterns)
# Example: CSV READ FROM file INTO variable

# 2. Add to src/cns.lisp in apply-effect function
# ... edit apply-effect to handle new effect keyword ...

# 3. Create test example
cat > examples/features/test-csv.cns << 'EOF'
# STARTER
Story: CSV Read Test
Given:
  file: String = "/tmp/data.csv"
  data: String = ""
Step 1 → Read CSV
  Then: data becomes CSV READ FROM file
  Effect: Print "Data: {data}"
End: Return data
EOF

# 4. Test new effect
./cns-run examples/features/test-csv.cns
./test-all-examples.sh  # Should include new test

# 5. Document in SYNTAX.md
# ... add to Effects section ...

# 6. Commit
git add src/cns.lisp examples/features/test-csv.cns docs/language/SYNTAX.md
git commit -m "Add CSV READ effect for file parsing

- New effect: CSV READ FROM file INTO variable
- Implementation in apply-effect
- Example: examples/features/test-csv.cns
- Tests: 43/43 examples pass (was 42/42)
- Docs: Added to SYNTAX.md Effects section"
```

### Task 3: Repository Cleanup

```bash
# 1. Identify bloat candidates
find docs -name "*.md" -size +50k  # Large docs
find examples -name "*.cns" | wc -l  # Example count
du -sh docs/* examples/* | sort -h  # Directory sizes

# 2. Archive completed session docs
mv docs/development/SESSION-COMPLETE.md docs/archive/

# 3. Remove redundant examples
# ... delete examples that duplicate existing coverage ...

# 4. Compress verbose docs (while preserving info)
# ... edit large .md files to be more concise ...

# 5. Measure improvement
du -sh . # Total size
git log --oneline -5  # Recent commits for context

# 6. Commit cleanup
git add docs examples
git commit -m "Repository cleanup: reduce context bloat

- Archived: SESSION-COMPLETE.md (work done)
- Removed: 5 redundant examples (duplicated coverage)
- Compressed: VERBOSE-DOC.md (50KB → 20KB, info preserved)
- Before: 4.2MB, After: 3.5MB (17% reduction)
- Tests: 42/42 examples still pass"
```

### Task 4: Update Documentation

```bash
# 1. Identify affected docs
# Language change → docs/language/*.md
# Testing change → docs/development/TESTING.md
# Feature change → docs/guides/*.md

# 2. Update in SAME commit as code change
# ... edit docs/language/SYNTAX.md ...

# 3. Cross-reference related docs
# Add links: See also COMMON-PATTERNS.md

# 4. Update examples if needed
# ... edit examples/features/test-*.cns ...

# 5. Test examples still work
./test-all-examples.sh

# 6. Commit together
git add src/cns.lisp docs/language/SYNTAX.md examples/features/test-*.cns
git commit -m "Add feature X with documentation

- Code: <description>
- Docs: Updated SYNTAX.md, added examples
- Tests: 42/42 examples pass"
```

---

## Code Bloat Management

### Measuring Repository Health

**Repository size targets:**
```bash
# Total repository size
du -sh .
# Target: <5MB (currently 3.5MB)

# Documentation size
du -sh docs
# Target: <2MB (currently 1.2MB)

# Examples size
du -sh examples
# Target: <500KB (currently 350KB)

# Source code size
du -sh src
# Target: <1MB (currently 600KB)
```

**File count targets:**
```bash
# Count files by type
find docs -name "*.md" | wc -l      # Target: <30 (currently 25)
find examples -name "*.cns*" | wc -l # Target: <50 (currently 42)
find tests -type f | wc -l          # Target: <20 (currently 15)
```

**Line count monitoring:**
```bash
# Source code complexity
wc -l src/cns.lisp
# Currently: ~4000 lines (monitor for growth)

# Documentation verbosity
wc -l docs/language/*.md
# Monitor: Large files (>1000 lines) are candidates for compression
```

### When to Clean Up

**Immediate cleanup triggers:**
- ❌ Repository exceeds 5MB
- ❌ Documentation exceeds 2MB
- ❌ More than 50 example files
- ❌ Duplicate content across multiple docs
- ❌ Session docs older than 1 month (archive them)

**Regular maintenance (monthly):**
- Archive completed session documentation
- Remove redundant examples (keep unique patterns only)
- Compress verbose documentation (preserve essential info)
- Delete obsolete test files (keep regression tests)
- Review `docs/archive/` for very old items (consider deletion)

### Cleanup Checklist

```bash
# 1. Identify bloat
find docs -name "*.md" -size +100k        # Very large docs
find examples -name "*.cns" -exec wc -l {} \; | sort -n  # Duplicate examples
git log --all --pretty=format: --name-only | sort | uniq -c | sort -rg | head  # Frequently changed files

# 2. Archive session docs
mkdir -p docs/archive/YYYY-MM
mv docs/development/OLD-SESSION-*.md docs/archive/YYYY-MM/

# 3. Remove duplicates
# ... delete examples that don't add unique value ...
git rm examples/redundant-*.cns

# 4. Compress docs (example)
# Before: 150 lines of verbose explanation
# After: 50 lines of concise patterns + examples
# Preserve: All essential information, examples, links

# 5. Validate after cleanup
./test-all-examples.sh  # Ensure nothing broke
du -sh .                # Measure improvement

# 6. Commit cleanup
git commit -m "Reduce repository bloat: <X>% size reduction

- Archived: <session docs>
- Removed: <redundant examples>
- Compressed: <verbose docs>
- Before: XMB, After: YMB (Z% reduction)
- Tests: 42/42 examples still pass"
```

### Anti-Bloat Patterns

**DO:**
- ✅ Reference existing docs instead of duplicating content
- ✅ Archive session docs after integration
- ✅ Keep examples minimal and focused
- ✅ Use concise language in documentation
- ✅ Delete temporary files before committing
- ✅ Monitor repository size with `du -sh .`

**DON'T:**
- ❌ Duplicate CNS syntax across multiple docs
- ❌ Keep old session docs in main development folder
- ❌ Create examples that duplicate existing patterns
- ❌ Write verbose explanations when examples suffice
- ❌ Commit backup files, logs, or temporary artifacts
- ❌ Let repository grow without regular audits

### Automated Cleanup Tool

**Use the cleanup script for regular maintenance:**

```bash
# Preview what will be cleaned (safe, no changes)
./scripts/cleanup-repo.sh --dry-run

# Show repository size metrics only
./scripts/cleanup-repo.sh --report

# Execute cleanup automatically
./scripts/cleanup-repo.sh --auto

# Full cleanup with aggressive duplicate removal
./scripts/cleanup-repo.sh --auto --aggressive
```

**What the script does:**

1. **Archives old session docs** (>7 days) from `docs/development/` to `docs/archive/YYYY-MM/`
2. **Moves test results** from root directory to `tests/results/`
3. **Flags large files** (>100KB) for manual review
4. **Reports metrics** before/after cleanup

**Protected files (never archived):**
- `LISP-DEBUGGING-GUIDE.md`
- `TESTING.md`
- `ROADMAP.md`
- `README.md`
- Active roadmaps/status files

**When to run cleanup:**
- ✅ Monthly (first of month)
- ✅ Before major releases
- ✅ When `docs/development/` exceeds 20 files
- ✅ When repository exceeds 5MB
- ✅ After completing multi-day feature work

**Retention policy:**
- **<7 days**: Keep in `docs/development/`
- **7-90 days**: Archive to `docs/archive/YYYY-MM/`
- **>90 days**: Review for deletion (if superseded)

---

## Best Practices Summary

### Before Every Change
1. Read relevant documentation first
2. Understand current implementation
3. Test existing behavior

### During Development
1. Make small, incremental changes
2. Test after every change (not batched!)
3. Add tracing/logging to debug issues
4. Keep backups until feature complete

### Before Every Commit
1. Run `./test-all-examples.sh` (REQUIRED)
2. Run `./cns-validate examples/*.cns` (REQUIRED)
3. Run `./tests/run-all-tests.sh` (RECOMMENDED)
4. Update documentation in same commit
5. Write descriptive commit message with test results

### Regular Maintenance
1. Run automated cleanup: `./scripts/cleanup-repo.sh --auto`
2. Monitor repository size (`du -sh .`)
3. Remove redundant examples/tests
4. Compress verbose documentation
5. Review `docs/archive/` for very old items (>90 days)

---

## Quick Reference Card

### Commands to Memorize

```bash
# Development cycle
sbcl --non-interactive --load src/cns.lisp  # Syntax check
./cns-run examples/factorial.cns            # Run example
./test-all-examples.sh                      # Full test suite

# Testing
./cns-validate examples/new.cns             # Validate syntax
./tests/run-all-tests.sh                    # Regression tests

# Cleanup & Measurement
./scripts/cleanup-repo.sh --report          # Repository metrics
./scripts/cleanup-repo.sh --dry-run         # Preview cleanup
./scripts/cleanup-repo.sh --auto            # Execute cleanup
du -sh .                                    # Repository size
find docs -name "*.md" | wc -l              # Doc count
wc -l src/cns.lisp                          # Interpreter size

# Git workflow
git status                                  # Check state
git add <specific files>                    # Stage changes
git commit -m "Description\n\n- Details"    # Commit
git log --oneline -10                       # Recent history
```

### Files to Know

| File | Purpose |
|------|---------|
| `src/cns.lisp` | Main interpreter (read LISP-DEBUGGING-GUIDE.md first!) |
| `docs/language/SYNTAX.md` | Complete syntax reference |
| `docs/language/EXPRESSION-LIMITATIONS.md` | Critical parser limitations |
| `docs/development/TESTING.md` | Test infrastructure |
| `examples/README.md` | Pattern guide with examples |
| `test-all-examples.sh` | Run before every commit |

---

## See Also

**Language Documentation:**
- `docs/language/SYNTAX.md` - Complete CNS syntax reference
- `docs/language/COMMON-PATTERNS.md` - Reusable code patterns
- `docs/language/EXPRESSION-LIMITATIONS.md` - Parser limitations (CRITICAL)
- `docs/language/CONTROL-FLOW-RULES.md` - If/Otherwise and loop patterns

**Development Documentation:**
- `docs/development/TESTING.md` - Test infrastructure and validation
- `docs/development/LISP-DEBUGGING-GUIDE.md` - Debugging Common Lisp (CRITICAL)
- `docs/development/ROADMAP.md` - Current priorities and direction
- `docs/development/README.md` - Development guide index

**Examples:**
- `examples/README.md` - Complete pattern guide
- `examples/core/` - Fundamental language patterns (11 files)
- `examples/features/` - Feature-specific examples (25 files)
- `examples/advanced/` - Complex multi-feature patterns (6 files)

---

*Last Updated: 2025-11-03*  
*Status: Focused development system prompt with automated cleanup*  
*Audience: AI agents developing CNS*
