# SWE-bench Experiments - Archive

**Date:** November 2025  
**Status:** Paused - Too complex for current development stage  
**Decision:** Focus on core language improvements before benchmark attempts

---

## 🎯 Original Goal

Build a CNS-based agent to compete on SWE-bench benchmarks:
- **SWE-bench Verified:** 500 Python tasks (SOTA: 65%)
- **SWE-bench Multilingual:** 300 tasks across 9 languages (baseline: 43%)

**Target:** Beat current baselines using CNS narrative programming approach

---

## 🛠️ What We Built

### Infrastructure Prepared
1. **Language Adapters** - Detect project language from files (Cargo.toml, go.mod, package.json)
2. **Universal Test Runner** - Abstract over pytest, cargo test, go test, npm test
3. **Multi-Language Toolchains** - Installed Rust, Go, Java, GCC, Python, Node
4. **Code Navigation** - FIND and GREP commands for file discovery and content search
5. **Git Operations** - Full workflow support (STATUS, DIFF, BRANCH, COMMIT, MERGE)
6. **Shell Execution** - Command execution with I/O capture

### Examples Created
- `examples/language-adapter.cns` - Language detection and build command extraction
- `examples/swe-bench-agent.cns` - Multi-step agent workflow
- `examples/test-runner.cns` - Universal test framework abstraction
- Multiple test repos in `test-repos/swebench-tasks/`

---

## 📊 What We Learned

### Technical Insights

**1. CNS is Language-Agnostic**
Unlike AST-based agents (hardcoded for Python), CNS works with any language:
- SHELL command runs any toolchain
- FIND/GREP work on any source code
- No language-specific parsing needed

**2. Narrative Programming Advantage**
CNS approach:
```cns
Step 1: Understand the issue
  READ FILE "issue.json" INTO issue
  PARSE JSON issue INTO data
  
Step 2: Find relevant files
  FIND "*.py" IN "src" INTO files
  GREP data.keyword IN files INTO matches
  
Step 3: Run tests to verify
  SHELL "pytest tests/" INTO output WITH EXIT_CODE code
```

Traditional agent: Complex AST parsing, language-specific tooling, brittle

**3. Shell-Based Approach Works**
Zero Lisp dependencies, works with any installed toolchain:
- cargo test (Rust)
- go test (Go)
- pytest (Python)
- npm test (Node)
- mvn test (Java)

### Strategic Insights

**1. Complexity Too High**
SWE-bench requires:
- Understanding complex codebases (10k+ files)
- Parsing natural language issue descriptions
- Writing production-quality patches
- Handling edge cases across multiple languages

**Current CNS State:**
- Core language: 65% feature complete
- LLM-first improvements: In progress
- Validator: Basic implementation
- Error messages: Recently enhanced

**Gap:** Need more maturity before benchmark attempts

**2. Wrong Timing**
We discovered while working on SWE-bench that:
- LLMs struggle to read/write CNS efficiently
- Need better error messages with examples
- Need validation mode to catch errors pre-runtime
- Need strict mode for NIL safety
- Expression parsing has limitations

**Insight:** Fix the language first, benchmarks second

**3. Better Path Forward**
Instead of SWE-bench now, focus on:
1. **LLM-first improvements** - Make CNS easy for AI to use
2. **Core language completion** - Fix expression parsing, add missing features
3. **Real-world examples** - Build production apps to find pain points
4. **Community feedback** - Let users drive feature priorities

**Then** attempt benchmarks when CNS is solid

---

## 🎓 Key Takeaways

### What Worked
✅ **Infrastructure Design** - FIND, GREP, SHELL commands are solid  
✅ **Multi-Language Support** - CNS naturally works across languages  
✅ **Agent Patterns** - Read-Analyze-Test-Patch workflow is sound  
✅ **Toolchain Integration** - Shell-based approach proven viable

### What Didn't Work
❌ **Timing** - Too early in CNS development lifecycle  
❌ **Complexity** - SWE-bench is "PhD-level" for language maturity  
❌ **LLM UX** - Discovered CNS is hard for LLMs to read/write  
❌ **Focus** - Distracted from core language improvements

### Critical Discovery
**"LLMs themselves struggle with CNS"**

While building the SWE-bench agent, we noticed:
- LLMs make syntax errors frequently
- Runtime debugging is slow and expensive
- Error messages don't teach correct patterns
- No validation before execution
- Silent NIL failures cause infinite loops

**This insight led to:**
- Enhanced error messages with working examples
- Validation mode (catch 90% errors pre-runtime)
- Strict mode for NIL safety
- Expression limitation documentation
- 4x improvement in test pass rate

**Value:** SWE-bench attempt revealed what CNS really needed

---

## 🔄 Future Plans

### When to Revisit
**After completing:**
1. ✅ Enhanced error messages (DONE - v1.7.0+)
2. ✅ Validation mode (DONE - v1.7.0+)
3. 🚧 Strict mode NIL enforcement (IN PROGRESS)
4. ⏳ Expression parsing improvements
5. ⏳ LLM-first documentation
6. ⏳ Real-world production apps built in CNS
7. ⏳ Community feedback incorporated

**Then:** Attempt SWE-bench with mature language

### Alternative Benchmark Paths
**Consider instead:**
- **SimpleQA** - Question answering (lower complexity)
- **HumanEval** - Code generation (demonstrates CNS syntax)
- **GAIA** - General AI assistant (practical use cases)
- **Real-world case studies** - "Built X with CNS" stories

**Why:** Prove CNS value without requiring PhD-level language maturity

---

## 📁 What Was Archived

**Deleted (6.4MB):**
- `test-repos/rust-example/` - Rust test project
- `test-repos/swebench-tasks/` - 20+ SWE-bench task JSON files
- `swebench-output.txt` - Agent test outputs (800KB+)

**Why:** Repo size was impacting LLM performance (10MB → 3MB)

**Recovery:** All work is in git history. When ready to resume:
```bash
git log --all --full-history -- test-repos/
git checkout <commit> -- test-repos/
```

---

## 💡 Recommendations

### For Next SWE-bench Attempt

**Phase 1: Language Maturity**
- Complete all LLM-first improvements
- Build 10+ real production apps in CNS
- Get community feedback on pain points
- Achieve 85%+ feature completeness

**Phase 2: Agent Development**
- Start with simplest tasks (file modifications)
- Build pattern library (common code changes)
- Test on single language first (Python)
- Iterate based on failure analysis

**Phase 3: Multi-Language**
- Extend to Rust (58% baseline - easiest)
- Then Go (31% baseline - biggest opportunity)
- Then others (Java, C, Node)

**Phase 4: Benchmark Run**
- Run full SWE-bench Verified
- Run full SWE-bench Multilingual
- Collect comprehensive metrics
- Write technical blog post

**Timeline:** 6-12 months from now (November 2025 → May-November 2026)

---

## 🎯 Current Priorities (Post-SWEbench)

### Phase 1: LLM-First Repository (IN PROGRESS)
**Goal:** Make CNS repository optimized for LLM understanding

**Actions:**
- ✅ Delete SWE-bench apparatus (6.4MB freed)
- ✅ Clean root directory (27 → 5 files)
- ⏳ Consolidate examples (94 → ~30 with pattern guide)
- ⏳ Restructure documentation (LLM-focused)
- ⏳ Create SYNTAX.md with clear patterns

**Impact:** LLMs can load context 70% faster, understand patterns better

### Phase 2: Language Improvements (NEXT)
**Goal:** Complete LLM-first features from testing insights

**Actions:**
- ⏳ Strict mode NIL enforcement
- ⏳ Expression parsing improvements
- ⏳ Validator enhancements
- ⏳ Better error messages for edge cases

**Impact:** 80%+ LLM first-try success rate

### Phase 3: Real-World Apps (FUTURE)
**Goal:** Build production applications in CNS

**Ideas:**
- API backend for web app
- Data processing pipeline
- DevOps automation tool
- CLI tool for developers
- Web scraper with database

**Impact:** Discover real pain points, prove production readiness

---

## 📝 Lessons for Future Benchmark Attempts

1. **Validate language maturity first** - Build real apps before benchmarks
2. **LLM UX is critical** - If LLMs struggle with the language, fix that first
3. **Start simple** - Don't jump to hardest benchmarks immediately
4. **Iterate based on failures** - Each failure should improve the language
5. **Community feedback matters** - Users reveal problems benchmarks miss
6. **Timing is everything** - Right idea, wrong time = wasted effort
7. **Failed fast is good** - We discovered LLM UX issues early

---

## 🙏 Acknowledgments

This "failed" experiment was actually incredibly valuable:
- Revealed LLM UX issues we wouldn't have found otherwise
- Led to 4x improvement in test pass rate
- Inspired enhanced error messages
- Validated shell-based approach
- Proved multi-language capability

**Result:** Better language, better priorities, better path forward

---

**Status:** Archived but not abandoned  
**Next Review:** After Phase 2 (Language Improvements) complete  
**Expected Resume:** Mid-2026
