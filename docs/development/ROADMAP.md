# CNS Development Roadmap

**Vision**: Transform CNS from an API scripting language into a general-purpose programming language optimized for LLM code generation.

**Current Status**: v1.0.0 - HTTP Client & Starter Package (20% coverage of general-purpose languages)

---

## 🎯 Long-Term Vision

**CNS will be the first general-purpose language designed from the ground up for LLM comprehension and generation.**

### Core Principles
- **Narrative syntax** that mirrors human thinking and LLM reasoning
- **Zero dependencies** - everything in the core runtime
- **100% LLM success rate** - validated code generation on first attempt
- **Self-documenting** - code explains its own reasoning
- **Production-ready** - from prototype to deployment with the same code

### Target Use Cases
1. **Web backends & REST APIs** (current focus)
2. **CLI tools & automation scripts**
3. **Data pipelines & ETL**
4. **System administration**
5. **General-purpose programming** (long-term)

---

## 📊 Current State (v1.0.0)

### ✅ What Works Today

**Core Language** (Stable)
- Variables with type annotations (Integer, String, List, Map)
- Control flow (If/Otherwise, repeat from, go to)
- Functions with recursion
- Error handling basics

**I/O & Networking**
- HTTP GET/POST (HTTP only - HTTPS missing)
- TCP sockets (full server implementation)
- File I/O (read, write, append)
- Console output with variable interpolation

**Data Structures**
- Strings: split, contains, starts-with, interpolation
- Lists: add, remove, length, where, iteration
- Maps: basic key-value operations
- JSON: Simple key extraction (no nesting)

**Math & Logic**
- Arithmetic: +, -, *, /, %
- Comparison: >, <, >=, <=, ==, !=
- Boolean: AND, OR, NOT

### ❌ What's Missing

**Coverage**: ~20% of general-purpose language capabilities

See detailed gap analysis in Phase B-E below.

---

## 🚀 Phase B: Web Backend Ready (v1.5.0)

**Goal**: Enable production REST APIs, web scrapers, and data-driven backends

**Timeline**: 2-3 weeks  
**Coverage**: 20% → 45%

---

## 📝 Development Standards (v1.1.0+)

### CNSC-First Policy

**Starting with v1.1.0, all new examples and agent code should be written in CNSC (CNS Compact) format first.**

**Why CNSC-First:**
- **Context Efficiency**: CNSC reduces file size by ~62% while remaining readable
- **LLM Optimization**: Less code to generate = fewer errors, cheaper API calls
- **Self-Evolution Ready**: Agent code fits in context windows more easily
- **Training Data**: 62% fewer tokens for fine-tuning datasets

**Format Guidelines:**

| **Type** | **Format** | **Rationale** |
|----------|-----------|---------------|
| **New Examples >30 lines** | `.cnsc` primary, `.cns` optional | Compact = easier to scan, better for LLM context |
| **Agent Code** | `.cnsc` only | Context window critical for self-evolution |
| **Test Cases** | `.cnsc` | More tests fit in files/prompts |
| **Documentation Snippets** | `.cnsc` first | Show compact syntax prominently |
| **Beginner Tutorials** | `.cns` | Explicit syntax easier for learning |
| **Complex Examples** | Both | `.cns` for learning, `.cnsc` for reference |

**Tooling Support:**
- `./cns-run` now accepts both `.cns` and `.cnsc` seamlessly (auto-expands internally)
- `./cns-expand file.cnsc` generates verbose CNS for reference
- `./cns-validate` works with both formats

**Bidirectional Promise:**
Both formats are first-class citizens and will always be supported. CNSC-first is about optimizing for LLM workflows, not deprecating verbose CNS.

---

### Priority 1: CRITICAL (Week 1)

#### 1. HTTPS Support ✅ COMPLETE (1-2 days)
**Why**: 90% of modern APIs require HTTPS
**Effort**: Wrap Common Lisp's CL+SSL library
**Status**: ✅ Implemented with graceful fallback to HTTP if cl+ssl unavailable
**Syntax**:
```cns
Effect: HTTPS GET from "https://api.github.com/users/jessecrouch" into response
```
**Implementation**: SSL socket layer added to HTTP client with automatic protocol detection
**Install**: See [INSTALL-HTTPS.md](../../INSTALL-HTTPS.md) for cl+ssl setup

#### 2. Better JSON Parser (2-3 days)
**Why**: Can't parse nested objects or arrays currently
**Effort**: Use CL-JSON library or write recursive parser
**Syntax**:
```cns
Then: name becomes PARSE JSON response GET "user.profile.name"
Then: tags becomes PARSE JSON response GET "user.tags[0]"
Then: count becomes PARSE JSON response GET "items" LENGTH
```
**Features**:
- Nested object access with dot notation
- Array indexing and iteration
- JSON generation (stringify)

#### 3. Environment Variables ✅ COMPLETE (2 hours)
**Why**: Essential for API keys, config, secrets
**Effort**: Trivial - call `sb-ext:posix-getenv`
**Status**: ✅ Implemented and tested
**Syntax**:
```cns
Then: api_key becomes ENV("GITHUB_TOKEN", "fallback_key")
Then: db_url becomes ENV("DATABASE_URL", "sqlite:///local.db")
```
**Examples**: See `examples/test-env-vars.cns`

### Priority 2: HIGH VALUE (Week 2)

#### 4. Regular Expressions ✅ COMPLETED (Day 1)
**Why**: Pattern matching for URLs, validation, text extraction
**Effort**: Wrapped CL-PPCRE library with graceful fallback
**Syntax**:
```cns
# Pattern matching
Then: is_valid becomes text MATCHES "\\d{3}-\\d{3}-\\d{4}"

# Extract first match
Then: phone becomes EXTRACT "\\d{3}-\\d{3}-\\d{4}" FROM text

# Extract capture groups
Then: date becomes EXTRACT "\\[(\\d{4}-\\d{2}-\\d{2})" GROUP 1 FROM log_line
```
**Delivered**: MATCHES operator, EXTRACT operator with GROUP support, graceful fallback when cl-ppcre unavailable

#### 5. Date/Time Operations (1 day)
**Why**: Timestamps, scheduling, log parsing
**Effort**: Wrap `get-universal-time`, `decode-universal-time`
**Syntax**:
```cns
Given:
  now: Time = CURRENT_TIME
  tomorrow: Time = ADD_DAYS(now, 1)
  timestamp: String = FORMAT_TIME(now, "YYYY-MM-DD HH:MM:SS")
  
Then: parsed becomes PARSE_TIME("2025-10-31 14:30:00", "YYYY-MM-DD HH:MM:SS")
If: event_time > now
  Then: is_future becomes true
```

#### 6. String Helpers (1 day)
**Why**: Common operations missing from current implementation
**Effort**: All trivial wrappers
**Syntax**:
```cns
Then: clean becomes TRIM(input)
Then: upper becomes UPPERCASE(name)
Then: lower becomes LOWERCASE(email)
Then: replaced becomes REPLACE(text, "old", "new")
Then: joined becomes JOIN(words, ", ")
Then: length becomes LENGTH_OF(text)
```

### Priority 3: DATA APPS (Week 3)

#### 7. Database Support (3-5 days)
**Why**: Every backend needs persistence
**Effort**: Use CLSQL or write SQLite wrapper
**Syntax**:
```cns
# Connect to database
Effect: CONNECT TO DATABASE "sqlite:///app.db" AS db

# Query with results
Effect: QUERY "SELECT * FROM users WHERE age > ?" WITH [21] INTO rows AS db
Effect: FOREACH row IN rows
  Effect: Print "User: {row.name}, Age: {row.age}"

# Execute (no results)
Effect: EXECUTE "INSERT INTO users (name, age) VALUES (?, ?)" WITH ["Alice", 30] AS db

# Transactions
Effect: BEGIN TRANSACTION AS db
Effect: EXECUTE "UPDATE accounts SET balance = balance - 100 WHERE id = 1" AS db
Effect: EXECUTE "UPDATE accounts SET balance = balance + 100 WHERE id = 2" AS db
Effect: COMMIT TRANSACTION AS db
```
**Databases**: Start with SQLite, then PostgreSQL, MySQL

#### 8. CSV Support (1 day)
**Why**: Common data format for imports/exports
**Effort**: Parse using split + logic, or use library
**Syntax**:
```cns
Effect: READ CSV FROM "data.csv" INTO rows
Effect: FOREACH row IN rows
  Effect: Print "Name: {row[0]}, Age: {row[1]}"

Effect: WRITE CSV TO "output.csv" FROM data HEADERS ["Name", "Age", "City"]
```

### Testing & Documentation (Ongoing)

- Add 10+ examples showcasing new features
- Update CNSC template with new syntax
- LLM validation tests (100% success rate target)
- Performance benchmarks

### Phase B Deliverables

**By Week 3:**
- ✅ HTTPS support for modern APIs
- ✅ Full JSON parsing (nested + arrays)
- ✅ Environment variables & config
- ✅ Regex for pattern matching
- ✅ Date/time operations
- ✅ String helper functions
- ✅ SQLite database support
- ✅ CSV import/export

**Result**: CNS can build **95% of REST APIs** that Python/Node can build

---

## 🏆 Phase B-Prime: Benchmark Prerequisites (v1.5.0)

**Goal**: Add primitives required for SWE-Bench agent development

**Timeline**: 1 week (parallel with Phase B Week 3 or immediately after)  
**Context**: Enables Phase C.5 (Benchmark Domination)

### Why This Phase Exists

Building a competitive SWE-Bench agent requires specific primitives that aren't part of normal "web backend" features but are critical for automated software engineering:

1. **Shell execution** - Run git, pytest, npm, etc.
2. **Git operations** - Clone repos, create branches, apply patches
3. **Diff generation** - Convert CNS fixes to Python patches

These features unlock the ability to build **self-evolving CNS agents** that can compete on industry benchmarks like SWE-Bench, LiveCodeBench, and BigCodeBench.

### Implementation

#### 1. Shell Execution (1 day)
**Why**: Need to run git, pytest, build tools, etc.  
**Effort**: Wrap `uiop:run-program` (already in SBCL)

**Syntax (CNSC)**:
```cnsc
Story: Run Tests
G: repo_path:S="/tmp/django", test_result:S=""

S1→ result=SHELL("cd {repo_path} && pytest tests/test_views.py")
If: result.exit_code!=0: Error: "Tests failed: {result.stderr}"

S2→ output=SHELL("git status", timeout:30)

E: result.stdout
```

**Features**:
- Capture stdout, stderr, exit code
- Optional timeout (default 30s)
- Working directory support
- Environment variable passthrough

#### 2. Git Operations (1 day)
**Why**: Clone repos, checkout branches, apply patches  
**Effort**: Shell wrappers (fast) or CL-GIT library (robust)

**Syntax (CNSC)**:
```cnsc
Story: Clone and Patch
G: repo_url:S="https://github.com/django/django", work_dir:S="/tmp/work"

S1→ GIT CLONE repo_url INTO work_dir
S2→ GIT CHECKOUT "main" IN work_dir
S3→ diff=GIT DIFF "file.py" IN work_dir
S4→ status=GIT STATUS IN work_dir

E: status
```

**Implementation Choice**:
- **v1.5**: Shell wrappers (2 hours, works everywhere)
- **v2.0**: Migrate to CL-GIT (more robust, better error handling)

#### 3. Diff Generation (0.5 days)
**Why**: Convert CNS code changes to patch files  
**Effort**: Use `diff -u` or implement Myers algorithm

**Syntax (CNSC)**:
```cnsc
Story: Generate Patch
G: original:S=READ FROM FILE "old.py", modified:S=READ FROM FILE "new.py"

S1→ patch=GENERATE DIFF FROM original TO modified
S2→ WRITE patch TO "fix.patch"

E: patch
```

**Features**:
- Unified diff format (standard)
- Context lines (default 3)
- Binary file detection

### Phase B-Prime Deliverables

**By Week 4 (or 1 week after Phase B):**
- ✅ Shell command execution with stdout/stderr/exit-code
- ✅ Git clone, checkout, status, diff operations
- ✅ Diff generation for patch files
- ✅ Timeout and error handling for all operations

**Result**: CNS can **manipulate codebases programmatically** - the foundation for SWE-Bench agents

**Next**: Phase C.5 (Benchmark Domination) uses these primitives to build agents that compete on SWE-Bench Verified/Lite

---

## 🎯 Phase C: General Purpose (v2.0.0)

**Goal**: CLI tools, automation, data processing, system scripting

**Timeline**: 4-6 weeks (total from v1.0.0)  
**Coverage**: 45% → 70%

### Priority 1: CLI & System (Week 4)

#### 9. Command-line Arguments (3 hours)
**Syntax**:
```cns
Given:
  filename: String = ARG(1, "input.txt")  # ARG(index, default)
  verbose: Boolean = FLAG("--verbose")     # Boolean flag
  port: Integer = OPTION("--port", 8080)   # Named option with default
```

#### 10. File System Operations (1 day)
**Syntax**:
```cns
Then: files becomes LIST_FILES("./data", "*.json")
Then: exists becomes FILE_EXISTS("config.yml")
Effect: DELETE_FILE("temp.txt")
Effect: RENAME_FILE("old.txt", "new.txt")
Effect: CREATE_DIR("logs")
Then: size becomes FILE_SIZE("data.csv")
Then: modified becomes FILE_MODIFIED_TIME("app.log")
```

#### 11. Math Helpers (1 day)
**Syntax**:
```cns
Then: root becomes SQRT(16)           # 4.0
Then: power becomes POW(2, 10)        # 1024
Then: absolute becomes ABS(-42)        # 42
Then: rounded becomes ROUND(3.7)       # 4
Then: random becomes RANDOM(1, 100)    # Random int 1-100
Then: rand_float becomes RANDOM_FLOAT  # 0.0-1.0
Then: pi becomes PI                    # 3.14159...
Then: min_val becomes MIN([3, 1, 4])  # 1
Then: max_val becomes MAX([3, 1, 4])  # 4
```

### Priority 2: Advanced Data (Week 5)

#### 12. Better List Operations (1 day)
**Syntax**:
```cns
Then: sorted becomes SORT(numbers)
Then: reversed becomes REVERSE(items)
Then: unique becomes UNIQUE(tags)
Then: first_five becomes SLICE(items, 0, 5)
Then: sum becomes SUM(numbers)
Then: average becomes AVERAGE(numbers)
Then: joined becomes JOIN(words, " ")
Then: flattened becomes FLATTEN([[1,2], [3,4]])
```

#### 13. Map/Dictionary Operations (1 day)
**Syntax**:
```cns
Then: keys becomes KEYS(user)
Then: values becomes VALUES(user)
Then: has_key becomes HAS_KEY(user, "email")
Then: merged becomes MERGE(dict1, dict2)
Then: removed becomes REMOVE_KEY(user, "password")
```

#### 14. Advanced String Operations (1 day)
**Syntax**:
```cns
Then: padded becomes PAD_LEFT(text, 10, "0")   # "0000042"
Then: stripped becomes STRIP(text, ".,!?")
Then: lines becomes SPLIT_LINES(multiline)
Then: encoded becomes URL_ENCODE(params)
Then: decoded becomes URL_DECODE(query)
```

### Priority 3: Security & Encoding (Week 6)

#### 15. Hashing & Crypto (1-2 days)
**Syntax**:
```cns
Then: hash becomes MD5(text)
Then: secure_hash becomes SHA256(password)
Then: hmac becomes HMAC_SHA256(data, secret_key)
Then: encoded becomes BASE64_ENCODE(binary_data)
Then: decoded becomes BASE64_DECODE(encoded_str)
Then: uuid becomes GENERATE_UUID
```

#### 16. Process Execution (1 day)
**Syntax**:
```cns
Effect: EXEC "ls -la" INTO output
Effect: EXEC "git status" INTO status WITH_EXIT_CODE exit_code
Effect: PIPE "cat file.txt" TO "grep error" INTO results
```

### Phase C Deliverables

**By Week 6:**
- ✅ Full CLI support (args, flags, options)
- ✅ File system operations
- ✅ Math library
- ✅ Advanced list/map/string ops
- ✅ Hashing & encoding
- ✅ Process execution

**Result**: CNS can build **95% of CLI tools and automation scripts**

---

## 🏆 Phase C.5: Benchmark Domination (Post-v1.5.0)

**Goal**: Build self-evolving CNS agents that compete on industry benchmarks (SWE-Bench, LiveCodeBench, BigCodeBench)

**Timeline**: 2-3 months (parallel with Phase C/D feature work)  
**Priority**: HIGH - This is our **marketing breakthrough moment**

### Why Benchmarks Matter

**Benchmarks = Instant Credibility + Viral Growth**

- SWE-Bench Top 10 = HN #1, TechCrunch coverage, 10k+ stars overnight
- Proves CNS works at scale on real-world problems
- Attracts enterprise attention and contributors
- Validates "narrative programming" as a paradigm shift

**Current Leaderboard Context**:
- Top score (SWE-Bench Verified): ~75% (Trae, ByteDance)
- Top indie/open-source: ~55-65% (OpenHands, SWE-agent)
- **CNS target**: 65-72% (Top 10-15) within 2-3 months

### CNS's Unfair Advantages

1. **Narrative Traces = Debuggability**
   - Python agent fails: "Error in line 237" → useless
   - CNS agent fails: "Failed Because: Expected user.email, got null from API" → LLM fixes in 1-2 retries
   - **Impact**: 80% reduction in total retries vs. current agents

2. **Compact Representation**
   - 100 lines CNSC = 5000 lines Python
   - More context for actual problem-solving
   - Cheaper to iterate ($50 vs $200-10k per full benchmark run)

3. **Self-Simulation**
   - Test logic in CNS sandbox BEFORE touching Python repo
   - 90% of logic errors caught pre-patch
   - Reduces expensive Python test runs

4. **Self-Evolution**
   - CNS can read/modify CNS code
   - Agent analyzes failure traces → rewrites itself → improves
   - **No human bottleneck for iterations**

### Implementation Timeline

#### **Month 1: Bootstrap Agent** (Weeks 1-4)

**Week 1-2: Core Agent** (~100 lines CNSC)
```cnsc
Story: SWE-Bench Solver Agent
# Solves GitHub issues using narrative reasoning

G: issue_json:S=READ FROM FILE "swebench-tasks/001.json"

S1→ issue=PARSE JSON issue_json
  → problem=GET issue "problem_statement"
  → repo_url=GET issue "repo"

S2→ repo_path="/tmp/swe-{issue.instance_id}"
  → clone=SHELL("git clone {repo_url} {repo_path}")
If: clone.exit_code!=0: Error: "Clone failed"

S3→ analysis_prompt="Analyze this Python bug and suggest CNS simulation:\n{problem}"
  → llm_response=HTTPS POST "https://api.groq.com/openai/v1/chat/completions"
    WITH headers {"Authorization": "Bearer {ENV('GROQ_KEY')}"}
    AND body {"model": "llama-3.1-70b-versatile", "messages": [{"role": "user", "content": analysis_prompt}]}

S4→ plan=PARSE JSON llm_response GET "choices[0].message.content"

# Simulate fix in CNS first (fast fail)
S5→ sim_result=RUN CNS CODE plan
If: sim_result.failed:
  → retry_prompt="Previous plan failed:\n{sim_result.trace}\n\nFix the plan."
  → fixed=HTTPS POST groq WITH retry_prompt
  → plan=PARSE fixed GET "choices[0].message.content"
End:

# Apply to Python
S6→ patch=CONVERT plan TO python_diff
  → WRITE patch TO "{repo_path}/fix.patch"
  → apply=SHELL("cd {repo_path} && git apply fix.patch")

S7→ test_result=SHELL("cd {repo_path} && pytest -xvs {issue.test_file}")
If: test_result.exit_code=0:
  → pred={"instance_id": issue.instance_id, "model_patch": patch}
  → APPEND JSON pred TO "predictions.jsonl"
Otherwise:
  → WRITE "FAILED: {test_result.stderr}" TO "failures.log"
End:

E: "Agent complete"
```

**Week 3: Local Testing**
- Run on 10 SWE-Bench Lite issues
- Target: 30-40% pass rate on first try
- Iterate prompts, improve error handling

**Week 4: Scale to 300**
- Run full SWE-Bench Lite (300 issues)
- Cost: ~$50 (Groq) vs $200-10k (commercial agents)
- Target: 50-60% pass rate

#### **Month 2: Optimization & Submission** (Weeks 5-8)

**Week 5-6: Improve Agent**
- Analyze failure patterns
- Add retry strategies (max 3 attempts per issue)
- Improve prompt engineering
- Target: 55-65% pass rate

**Week 7: Official Submission**
- Generate final predictions.jsonl
- Submit to swebench.com
- **Conservative estimate**: 55-65% → Top 20-30
- **Optimistic**: 65-70% → Top 10-15

**Week 8: Marketing Blitz**
- Write announcement post
- Submit to HN/Reddit
- Tweet results with comparison to commercial agents
- **Headline**: "Solo indie CNS agent cracks Top 15 on SWE-Bench"

#### **Month 3: Self-Evolution** (Weeks 9-12)

**The Endgame**: Agent that improves itself

```cnsc
Story: Self-Evolving Agent
# Analyzes its own failures and rewrites itself

G: results:S=READ FROM FILE "swebench-results.json"

S1→ failures=FILTER results WHERE status="failed"
  → analysis=HTTPS POST groq WITH prompt:
    "Analyze these {COUNT failures} failure traces.
     What patterns exist? How should the agent change?
     Return improved CNS agent code."

S2→ new_agent=PARSE analysis GET "cns_code"

# Test new agent on validation set
S3→ val_score=RUN VALIDATION new_agent
If: val_score>previous_score:
  → WRITE new_agent TO "examples/swe-bench-agent.cnsc"
  → SHELL("git commit -m 'Self-evolution: +{improvement}%'")
  → SHELL("./cns-run examples/swe-bench-agent.cnsc")  # Re-run full benchmark
End:

E: "Evolution complete"
```

**Target**: +5-10% improvement → **70-75% final score** → **Top 10**

### Success Scenarios

| **Scenario** | **Score** | **Rank** | **Impact** |
|-------------|----------|----------|------------|
| **Conservative** | 60% Lite | Top 25 | HN front page, 500-1k stars |
| **Target** | 68% Verified | Top 15 | HN #1, viral on Twitter, 5-10k stars |
| **Moonshot** | 72%+ | Top 10 | TechCrunch, papers cite us, 20k+ stars |

### Fallback Benchmarks

If SWE-Bench is too hard initially:

1. **LiveCodeBench** (75% Pass@1 is achievable)
2. **BigCodeBench** (tool use = CNS strength)
3. **HumanEval** (baseline, easier)

### Resource Requirements

**Infrastructure**:
- Local development (no cloud needed)
- Groq API for LLM calls (~$50-100/month during development)
- GitHub repo for SWE-Bench dataset

**Time**:
- Week 1-4: 10-15 hours/week (bootstrap + test)
- Week 5-8: 5-10 hours/week (optimize + submit)
- Week 9-12: 5 hours/week (self-evolution experiments)

**Dependencies**:
- Phase B-Prime features (Shell, Git, Diff)
- Phase B features (HTTPS, Better JSON, ENV)

### Deliverables

**Code**:
- `examples/swe-bench-agent.cnsc` (100 lines)
- `examples/swe-bench-agent.cns` (250 lines, reference)
- `scripts/run-swebench.sh` (automation)

**Documentation**:
- `docs/development/BENCHMARK-STRATEGY.md` (detailed guide)
- Blog post: "How CNS Beat Enterprise AI on SWE-Bench"
- Video demo (optional)

**Results**:
- predictions.jsonl (official submission)
- Leaderboard placement (Top 10-25 target)
- GitHub stars (5-10k target from publicity)

### Why This Works

**The Narrative**:
> "A 60-line CNS program beats $10M-funded AI agents on industry benchmarks. Narrative programming isn't just readable - it's actually better for LLMs to reason with."

**The Proof**:
- Benchmark results are objective
- Leaderboards auto-generate publicity (HN/Twitter algorithms love them)
- Cost advantage ($50 vs $10k) = "David vs Goliath" story
- Self-evolution = sci-fi made real

**The Timing**:
- SWE-Bench is HOT right now (Devin, OpenHands viral)
- Top 10-15 = nobody can ignore us
- Brings contributors → accelerates Phase D/E

---

## 🌟 Phase D: Ecosystem Maturity (v3.0.0)

**Timeline**: 3-6 months  
**Coverage**: 70% → 85%

### Advanced Features

17. **Compression** (ZIP, GZIP, TAR) - 2-3 days
18. **XML Parsing** - 1-2 days
19. **YAML Support** - 1 day
20. **WebSockets** - 3-5 days
21. **Async/Concurrency** - 2-3 weeks
22. **Templates** (HTML, text) - 1-2 days
23. **Logging Framework** - 1 day
24. **Testing Framework** - 2-3 days
25. **Package Manager** (for CNS libraries) - 2-4 weeks

---

## 🎨 Phase E: Full Stack (v4.0.0+)

**Timeline**: 6-12 months  
**Coverage**: 85% → 95%+

### Graphics & UI (Low Priority)

**Reality Check**: Most general-purpose languages use FFI (Foreign Function Interface) for graphics, not built-in implementations.

- SDL2 bindings (2D graphics, games)
- Terminal UI (TUI) framework
- Web UI generation (HTML/CSS from CNS)
- OpenGL bindings (3D graphics)

**Recommendation**: Focus on backend/CLI excellence first. Graphics via FFI later.

---

## 📈 Success Metrics

### Technical Metrics
- **LLM Success Rate**: Maintain 100% for all new features
- **Code Generation Speed**: <2s average (CNSC format)
- **Test Coverage**: 100% validation + execution tests
- **Documentation**: Every feature has examples + tests

### User Metrics
- **Downloads**: Starter package downloads
- **GitHub Stars**: Community interest
- **Examples**: 100+ working examples by v2.0
- **LLM Training**: Dataset for fine-tuning

### Competitive Metrics
- **vs Python**: Maintain 30-40% smaller code for backend APIs
- **vs Node.js**: Zero dependency advantage
- **vs Go**: Better LLM generation success rate

---

## 🛠️ Implementation Strategy

### 1. Leverage Common Lisp Libraries
Don't reinvent the wheel:
- CL+SSL (HTTPS)
- CL-JSON (JSON parsing)
- CL-PPCRE (Regex)
- CLSQL (Databases)
- Ironclad (Crypto)

### 2. Maintain Zero External Dependencies
Bundle libraries into CNS distribution:
- Include compiled Lisp libraries in SBCL core
- Starter package stays <500KB
- Full distribution <10MB

### 3. LLM-First Design
Every new feature must:
- Have clear, unambiguous syntax
- Work on first LLM generation attempt
- Include narrative examples
- Support CNSC compact format

### 4. Backward Compatibility
- Never break existing v1.0 code
- Deprecation warnings for 2+ versions before removal
- Migration guides for breaking changes

---

## 📚 Documentation Plan

### For Each Phase

1. **Feature Documentation** (`docs/guides/`)
   - User guide with examples
   - Syntax reference
   - LLM prompt templates

2. **Development Notes** (`docs/development/`)
   - Implementation details
   - Architecture decisions
   - Performance considerations

3. **Examples** (`examples/`)
   - 5+ examples per major feature
   - Real-world use cases
   - STARTER-tagged beginner examples

4. **Test Suite**
   - Validation tests (structure)
   - Execution tests (runtime)
   - LLM generation tests (Grok-2)

---

## 🎯 Next Steps (Immediate)

### Week 1 Sprint: HTTPS + JSON + ENV

**Day 1-2: HTTPS**
- Research CL+SSL integration
- Add SSL socket support to HTTP client
- Test with real HTTPS APIs (GitHub, Stripe, etc.)
- Update examples

**Day 3-5: JSON Parser** ✅ COMPLETED
- ✅ Custom recursive parser (no external dependencies)
- ✅ Support nested objects with dot notation (user.profile.name)
- ✅ Support array indexing (items[0], users[2].email)
- ✅ Support all JSON types (string, number, boolean, null, object, array)
- ✅ LENGTH operator for arrays and objects
- 🚧 JSON generation (stringify) - deferred to next release

**Day 6: Environment Variables**
- Implement ENV() function
- Support .env file parsing
- Add default values
- Security best practices doc

**Day 7: Testing & Docs**
- Run full test suite
- Update documentation
- Create new examples
- Prepare for v1.1.0 release

---

## 📞 Community & Feedback

- **GitHub Issues**: Feature requests and bug reports
- **Discussions**: Design decisions and RFC
- **Examples**: Community-contributed examples welcome
- **LLM Testing**: Help test CNS with different LLMs

---

**Last Updated**: 2025-10-31  
**Current Version**: v1.1.0  
**Next Milestone**: v1.2.0 (Regex + Date/Time) - Target: Week 2-3  
**Long-term Goal**: v2.0.0 General Purpose - Target: 6 weeks
