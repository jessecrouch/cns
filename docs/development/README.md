# CNS Development Documentation

**Purpose**: Internal development guides for LLM agents and contributors working on CNS itself.

---

## Core Development Guides

### Essential Reading

**[LISP-DEBUGGING-GUIDE.md](LISP-DEBUGGING-GUIDE.md)** ⭐ **CRITICAL**
- **Read before modifying `src/cns.lisp`**
- Real-world methodology for debugging Common Lisp parentheses issues
- Case study from If/Otherwise fix
- Proven techniques that actually work

**[TESTING.md](TESTING.md)**
- Test infrastructure and validation system
- Running regression tests
- Adding new tests
- Test coverage strategy

**[ROADMAP.md](ROADMAP.md)**
- Current development direction (Phase C focus)
- Feature priorities
- Version history and velocity

---

## Language & Implementation

**[QUICK-REFERENCE-IF-OTHERWISE.md](QUICK-REFERENCE-IF-OTHERWISE.md)**
- If/Otherwise syntax quick reference
- Working patterns and common mistakes
- Waterfall pattern for multiple conditions
- Known limitations and workarounds

**[LLM-FIRST-IMPROVEMENTS.md](LLM-FIRST-IMPROVEMENTS.md)**
- Strategic improvements to make CNS easier for LLMs
- Better error messages, validation mode, strict mode
- Expression limitations documentation
- Priority roadmap for LLM-focused features

---

## Recent Work

**[PHASE-1-REORGANIZATION-COMPLETE.md](PHASE-1-REORGANIZATION-COMPLETE.md)**
- Repository cleanup session (10MB → 3.5MB)
- 65% size reduction, consolidated docs
- Context for current structure

**[PHASE-2-EXAMPLES-COMPLETE.md](PHASE-2-EXAMPLES-COMPLETE.md)**
- Examples reorganization (92 → 42 files)
- Three-tier structure (core/features/advanced)
- Comprehensive pattern guide creation

**[TEST-RESULTS-2025-11-02.md](TEST-RESULTS-2025-11-02.md)**
- Recent comprehensive test results
- Bugs fixed: String literal initialization, If/Otherwise in functions
- Known limitations documented

---

## Development Workflow

### Before Modifying Interpreter

1. **Read** [LISP-DEBUGGING-GUIDE.md](LISP-DEBUGGING-GUIDE.md)
2. **Backup** `cp src/cns.lisp src/cns.lisp.backup-$(date +%Y%m%d)`
3. **Test frequently** `sbcl --non-interactive --load src/cns.lisp`
4. **Use version control** Commit after each working change

### Common Development Tasks

| Task | Guide | Command |
|------|-------|---------|
| Add new feature | [ROADMAP.md](ROADMAP.md) | See current priorities |
| Debug Lisp parens | [LISP-DEBUGGING-GUIDE.md](LISP-DEBUGGING-GUIDE.md) | Binary search + structural tracing |
| Run tests | [TESTING.md](TESTING.md) | `./tests/run-validation-tests.sh` |
| Add test | [TESTING.md](TESTING.md) | Add to `tests/` with `# STARTER` tag |
| Validate syntax | | `./cns-validate examples/file.cns` |

### Documentation Standards

**For session docs** (like PHASE-1-REORGANIZATION-COMPLETE.md):
- Clear "Status" section (In Progress / Complete)
- "What", "Why", "How" structure
- Code examples and test results
- Note breaking changes
- Update this README with link

**For feature docs** (like QUICK-REFERENCE.md):
- Quick reference at top
- Examples of working patterns
- Common mistakes with ❌ Wrong / ✅ Right
- Known limitations clearly documented

---

## Archived Work

**SWE-bench agent work** (paused, see docs/archive/SWEBENCH-EXPERIMENTS.md):
- Determined LLMs need better CNS understanding first
- Shifted focus to LLM-first improvements (Phase C)
- Will resume after language maturity improves

**If/Otherwise fix** (completed, integrated into v1.7.0):
- Both story and function interpreters now support If/Otherwise correctly
- Documented in QUICK-REFERENCE-IF-OTHERWISE.md
- Session summaries deleted (work complete)

---

## Repository Context

This `/docs/development/` directory is part of a **Phase 1-2 reorganization** to optimize CNS for LLM-first development:

- **Phase 1** (Complete): Repository cleanup (10MB → 3.3MB)
- **Phase 2** (Complete): Examples consolidation (92 → 42 files)
- **Phase 3** (Current): Documentation restructure
- **Phase 4** (Next): Final validation and commit

See PHASE-1 and PHASE-2 docs for complete reorganization context.

---

## Related Documentation

- **User guides**: `/docs/guides/` - Feature guides for CNS users
- **Language reference**: `/docs/language/` - Syntax and patterns (Phase 3)
- **Installation**: `/docs/install/` - Setup guides
- **Archive**: `/docs/archive/` - Historical experiments

---

*Last Updated: 2025-11-02*  
*Files: 9 essential docs (down from 19)*  
*Focus: LLM-readable development guides*
