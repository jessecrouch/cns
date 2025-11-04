# CNS Development Documentation

**Purpose**: Internal development guides for contributors and LLM agents working on CNS itself.

**Last Updated**: November 3, 2025

---

## Essential Reading

### **[ROADMAP.md](ROADMAP.md)** ⭐
- Current status: v1.7.0 (Production Ready)
- Next priorities: CLI args, process management, file system ops
- Feature timeline and coverage goals
- Development velocity: 7 releases in 6 days

### **[TESTING.md](TESTING.md)** ⭐
- Test infrastructure and validation system
- Running regression tests (28/32 passing)
- Adding new tests
- Test coverage strategy

### **[LISP-DEBUGGING-GUIDE.md](LISP-DEBUGGING-GUIDE.md)** ⭐ **CRITICAL**
- **Read before modifying `src/cns.lisp`**
- Real-world methodology for debugging Common Lisp
- Proven techniques from actual bug fixes
- Binary search + structural tracing approach

---

## Language Reference

### **[QUICK-REFERENCE-IF-OTHERWISE.md](QUICK-REFERENCE-IF-OTHERWISE.md)**
- If/Otherwise syntax quick reference
- Working patterns and common mistakes
- Waterfall pattern for multiple conditions
- Known limitations and workarounds

---

## Current State (v1.7.0)

### Features Complete
- ✅ HTTP/HTTPS, TCP sockets, file I/O
- ✅ JSON, CSV, regex, date/time
- ✅ SQLite database
- ✅ Shell execution, git operations
- ✅ File search (FIND, GREP)
- ✅ Math functions (SQRT, POW, etc.)
- ✅ Strict mode, trace mode, validation mode
- ✅ LLM-first design (100% success with Grok-2)

### Test Status
- **28/32 tests passing (87.5%)**
- 4 expected timeouts (web servers)
- 100% validation success
- Zero false positives from validator

---

## Development Workflow

### Before Modifying Interpreter

1. **Read** [LISP-DEBUGGING-GUIDE.md](LISP-DEBUGGING-GUIDE.md) first
2. **Run tests** to establish baseline: `./tests/run-all-tests.sh`
3. **Make changes** incrementally
4. **Test after each change** - never batch changes
5. **Commit working code** frequently

### Common Development Tasks

| Task | Command | Notes |
|------|---------|-------|
| Run all tests | `./test-all-examples.sh` | Expect 28/32 pass |
| Run validation tests | `./tests/run-validation-tests.sh` | Should be 100% |
| Validate single file | `./cns-validate file.cns` | Check syntax |
| Run with trace mode | `./cns-run --trace file.cns` | Debug execution |
| Run with strict mode | `./cns-run --strict file.cns` | NIL safety |
| Load interpreter | `sbcl --load src/cns.lisp` | Interactive testing |

### Adding New Features

**Standard Process:**
1. Design the syntax (check SYNTAX.md for consistency)
2. Add to interpreter (`src/cns.lisp`)
3. Add to validator (`src/cns-validator.lisp`)
4. Create examples (`examples/features/`)
5. Write tests
6. Update SYNTAX.md
7. Update ROADMAP.md if it's a major feature

### Documentation Standards

**Critical docs** (keep forever):
- ROADMAP.md - Development direction
- TESTING.md - Test infrastructure
- LISP-DEBUGGING-GUIDE.md - Debugging methodology
- QUICK-REFERENCE-*.md - Language features

**Session docs** (archive when complete):
- Move to `docs/archive/2025-11/` when work is done
- Keep only if historical context is valuable
- Delete if information is incorporated elsewhere

---

## Next Steps (v1.8.0)

### Immediate Priorities
1. **CLI Arguments** - ARGS[0], ARG("--flag", "default"), HAS_FLAG
2. **Process Management** - Background jobs, signals, wait
3. **File System** - LIST_FILES, DELETE, RENAME, metadata

### Implementation Approach
- Follow existing patterns (see git/shell/file operations)
- Add comprehensive examples
- Test with LLMs during development
- Update SYNTAX.md immediately
- Maintain 100% backward compatibility

---

## Project Structure

```
docs/development/
├── README.md                          ← You are here
├── ROADMAP.md                         ← Development timeline
├── TESTING.md                         ← Test infrastructure
├── LISP-DEBUGGING-GUIDE.md           ← Debugging guide
└── QUICK-REFERENCE-IF-OTHERWISE.md   ← Language feature guide
```

**Archived**: All old session notes moved to `docs/archive/2025-11/`

---

## Related Documentation

- **Language Reference**: `/SYNTAX.md` - Complete CNS reference (830 lines)
- **User Guides**: `/docs/guides/` - LLM integration, trace mode, agents
- **Installation**: `/docs/install/` - Dependencies and setup
- **Examples**: `/examples/` - 35+ working programs
- **Archive**: `/docs/archive/` - Historical session notes

---

*Maintained by: CNS Development Team*  
*Last Major Update: November 3, 2025*  
*Current Version: v1.7.0 (Production Ready)*
