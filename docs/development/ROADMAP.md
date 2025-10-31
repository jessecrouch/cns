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

### Priority 1: CRITICAL (Week 1)

#### 1. HTTPS Support (1-2 days)
**Why**: 90% of modern APIs require HTTPS
**Effort**: Wrap Common Lisp's CL+SSL library
**Syntax**:
```cns
Effect: HTTPS GET from "https://api.github.com/users/jessecrouch" into response
```
**Implementation**: Add SSL socket layer to existing HTTP client

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

#### 3. Environment Variables (2 hours)
**Why**: Essential for API keys, config, secrets
**Effort**: Trivial - call `sb-ext:posix-getenv`
**Syntax**:
```cns
Given:
  api_key: String = ENV("GITHUB_TOKEN")
  db_url: String = ENV("DATABASE_URL", "sqlite:///local.db")  # with default
```

### Priority 2: HIGH VALUE (Week 2)

#### 4. Regular Expressions (1 day)
**Why**: Pattern matching for URLs, validation, text extraction
**Effort**: Wrap CL-PPCRE library
**Syntax**:
```cns
If: email MATCHES "^[\w\.-]+@[\w\.-]+\.\w+$"
  Then: valid becomes true
  
Then: user_id becomes EXTRACT "user-(\d+)" FROM url GROUP 1
Then: matches becomes FIND ALL "\d{3}-\d{4}" IN text
```

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

**Day 3-5: JSON Parser**
- Integrate CL-JSON or write recursive parser
- Support nested objects with dot notation
- Support array indexing
- Add JSON generation (stringify)
- Update killer-app-demo.cns

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
**Current Version**: v1.0.0  
**Next Milestone**: v1.1.0 (HTTPS) - Target: Week 1  
**Long-term Goal**: v2.0.0 General Purpose - Target: 6 weeks
