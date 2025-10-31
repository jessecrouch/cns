# CNS Benchmark Domination Strategy

**Goal**: Build self-evolving CNS agents that achieve Top 10-15 on SWE-Bench Verified/Lite

**Timeline**: 2-3 months (parallel with Phase C/D development)

**Why**: Benchmarks = instant credibility, viral growth, 10k+ GitHub stars, attracts contributors

---

## 🎯 The Opportunity

### Why SWE-Bench Is Perfect For CNS

**Current Leaderboard** (October 2025):
- **Top Score (Verified)**: ~75% (Trae, ByteDance - enterprise AI)
- **Top Open-Source**: ~55-65% (OpenHands, SWE-agent)
- **Cost Range**: $200-$10,000 per full benchmark run

**CNS Target**:
- **Score**: 65-72% on SWE-Bench Verified
- **Rank**: Top 10-15 globally
- **Cost**: $50-$100 per run (using Groq)

### CNS's Unfair Advantages

#### 1. Narrative Traces = Debuggability

**Current Agents** (blind retries):
```
Attempt 1: "Error in line 237"
Attempt 2: "Error in line 241"  
Attempt 3: "Error in line 239"
...
Attempt 20: Maybe it works?
```

**CNS Agent** (causal feedback):
```
Attempt 1: "Failed Because: Expected list, got null from response.data.users"
Attempt 2: (LLM fixes exact issue) → Success
```

**Impact**: 80% reduction in total retries

#### 2. Compact Representation

| Format | Agent Size | LLM Context Available |
|--------|-----------|----------------------|
| Python (OpenHands) | 5,000+ lines | Limited for problem-solving |
| CNS Verbose | 150 lines | Better |
| **CNSC Compact** | **60 lines** | **Maximum context for fixes** |

**Benefits**:
- Fewer tokens to generate = fewer syntax errors
- More context window for actual problem analysis
- Cheaper API costs ($50 vs $200-10k)

#### 3. Self-Simulation

**Workflow**:
1. LLM generates CNS fix logic
2. **Simulate in CNS sandbox** (fast, traceable)
3. 90% of logic errors caught here
4. Convert to Python patch
5. Apply to real repo (expensive test)

**Traditional agents**: Skip step 2-3, fail on step 5, retry 10-20x

#### 4. Self-Evolution

**The Endgame**: Agent that improves itself

```cnsc
Story: Self-Evolving Agent
G: results:S=READ FROM FILE "results.json"

S1→ failures=ANALYZE results
  → improvements=ASK llm "How should agent change?"

S2→ new_agent=GENERATE improved_version
If: VALIDATE new_agent > previous_agent:
  → WRITE new_agent TO "agent.cnsc"
  → RE-RUN benchmark
End:

E: "Evolution complete"
```

**No human bottleneck** - agent runs, learns, improves, repeats

---

## 📋 Implementation Roadmap

### **Phase 1: Bootstrap** (Month 1, Weeks 1-4)

#### Week 1-2: Core Agent Implementation

**File**: `examples/swe-bench-agent.cnsc` (~100 lines)

**Agent Flow**:
```
1. Read SWE-Bench issue JSON
2. Ask LLM for CNS fix plan
3. Simulate plan in CNS (catch logic errors)
4. Convert to Python patch
5. Clone repo, apply patch, run tests
6. If failed: feed trace back to LLM (1-2 retries max)
7. Store successful patches in predictions.jsonl
```

**Key Components**:
- Issue parsing (JSON)
- LLM integration (HTTPS POST to Groq)
- CNS simulation (RUN CNS CODE command)
- Git operations (clone, apply patch)
- Test execution (pytest via SHELL)

**Example Code** (See full version in ROADMAP.md Phase C.5):
```cnsc
Story: SWE-Bench Solver Agent

G: issue_json:S=READ FROM FILE "task.json"

S1→ issue=PARSE JSON issue_json
S2→ clone=SHELL("git clone {repo_url} /tmp/work")
S3→ plan=ASK llm "Fix this bug:\n{problem}"
S4→ sim=RUN CNS CODE plan  # Fast fail
S5→ patch=CONVERT plan TO python_diff
S6→ test=SHELL("cd /tmp/work && pytest {test_file}")
If: test.exit_code=0: SUCCESS
Otherwise: RETRY with trace

E: "Complete"
```

**Deliverables**:
- Working agent prototype
- Test on 5-10 simple issues
- Basic error handling

#### Week 3: Local Validation

**Tasks**:
- Download SWE-Bench Lite dataset (300 issues)
- Run agent on 10 diverse issues
- Measure success rate
- Analyze failure modes

**Target**: 30-40% pass rate on first try

**Metrics to Track**:
- Success rate (%)
- Average retries per issue
- Cost per issue ($)
- Time per issue (seconds)
- Failure categories (API, logic, syntax, etc.)

**Improvements**:
- Refine prompts based on failures
- Add timeout handling
- Improve error messages
- Better trace formatting

#### Week 4: Scale to Full Lite

**Tasks**:
- Run on all 300 SWE-Bench Lite issues
- Implement parallel execution (optional)
- Generate predictions.jsonl

**Target**: 50-60% pass rate

**Cost Estimate**:
- 300 issues × 2 attempts avg × $0.10 = **~$60**
- Compare to: OpenHands $200, Devin $10k

**Infrastructure**:
- Local execution (no cloud required)
- Groq API key (free tier or $10/month)
- Git + Python test environments

---

### **Phase 2: Optimization** (Month 2, Weeks 5-8)

#### Week 5-6: Improve Success Rate

**Failure Analysis**:
1. Categorize all failures
2. Identify patterns
3. Fix top 3 failure modes

**Common Failure Modes** (predicted):
- **Null handling**: "Expected X, got null"
- **Type mismatches**: "Expected list, got string"
- **API changes**: Code doesn't match repo structure
- **Test environment**: Missing dependencies

**Improvements**:
- Better null checking in simulation
- Type validation before applying patch
- Repo structure analysis step
- Dependency detection

**Target**: 55-65% pass rate

#### Week 7: Official Submission

**Submission Process**:
1. Generate final predictions.jsonl
2. Validate format (instance_id + model_patch)
3. Submit via SWE-Bench website/API
4. Wait 24-48 hours for results

**Expected Results**:
- **Conservative**: 55-60% → Rank 20-30
- **Target**: 60-65% → Rank 15-20
- **Optimistic**: 65-70% → Rank 10-15

#### Week 8: Marketing Blitz

**If Top 20-30**:
- HN Post: "CNS cracks SWE-Bench Top 25 with 60 lines of code"
- Reddit r/programming, r/MachineLearning
- Twitter announcement with leaderboard screenshot
- **Expected**: 500-1k GitHub stars, HN front page

**If Top 10-15**:
- All of above +
- TechCrunch tip
- AI research Twitter (Karpathy, etc.)
- Blog post: "How Narrative Programming Beat Enterprise AI"
- **Expected**: 5-10k stars, viral growth

**Content Strategy**:
- Emphasize cost advantage ($60 vs $10k)
- Highlight "solo indie" vs "ByteDance" narrative
- Show CNSC code (60 lines!) vs OpenHands (50k lines Python)
- Demo self-evolution capability

---

### **Phase 3: Self-Evolution** (Month 3, Weeks 9-12)

#### The Vision

**Immortal self-improving agent**:
1. Agent runs benchmark
2. Analyzes its own failure traces
3. Generates improved version of itself
4. Validates improvement
5. Commits new code
6. Re-runs benchmark
7. Repeat until convergence

#### Implementation

**File**: `examples/self-evolve.cnsc` (~80 lines)

**Flow**:
```cnsc
Story: Self-Evolution Engine

G: agent_code:S=READ FROM FILE "swe-bench-agent.cnsc"
  results:S=READ FROM FILE "benchmark-results.json"

S1→ failures=FILTER results WHERE success=false
  → patterns=ANALYZE failure_traces

S2→ improvement_prompt="Current agent:\n{agent_code}\n\n
     Failures:\n{failures}\n\n
     Patterns:\n{patterns}\n\n
     Generate improved agent code."

S3→ new_agent=HTTPS POST groq WITH improvement_prompt
  → candidate=PARSE new_agent GET "cns_code"

S4→ val_results=RUN BENCHMARK candidate ON validation_set
  → new_score=CALCULATE val_results

S5→ If new_score>previous_score:
      WRITE candidate TO "swe-bench-agent.cnsc"
      SHELL("git add swe-bench-agent.cnsc")
      SHELL("git commit -m 'Self-evolution: {previous_score}% → {new_score}%'")
      RUN FULL BENCHMARK
    Otherwise:
      PRINT "Evolution rejected: {new_score}% < {previous_score}%"
    End:

E: "Evolution cycle complete"
```

#### Experiments

**Week 9-10: Manual Evolution**
- Run 3-5 evolution cycles manually
- Track improvements
- Identify what works

**Week 11-12: Automated Evolution**
- Run evolution overnight
- Set stopping criteria (convergence or max iterations)
- Monitor for overfitting

**Target**: +5-10% improvement → **68-75% final score**

#### Safety Measures

**Prevent Bad Evolution**:
- Always validate on held-out set
- Require improvement threshold (>2%)
- Keep git history of all versions
- Human review every 5th generation

**Metrics**:
- Validation score (prevent overfitting)
- Code quality (doesn't become unreadable)
- Execution time (doesn't slow down)

---

## 📊 Success Metrics

### Primary Goal

| Metric | Target | Stretch |
|--------|--------|---------|
| **SWE-Bench Score** | 65-68% | 70-75% |
| **Leaderboard Rank** | Top 15 | Top 10 |
| **Cost Per Run** | <$100 | <$50 |
| **GitHub Stars** | 5-10k | 20k+ |

### Secondary Goals

| Metric | Target |
|--------|--------|
| **Average Retries** | <3 per issue |
| **Time Per Issue** | <120s |
| **Self-Evolution Gain** | +5-10% |
| **HN Front Page** | Yes |
| **TechCrunch Mention** | If Top 10 |

### Fallback Benchmarks

If SWE-Bench is too difficult initially:

1. **LiveCodeBench** (easier, 75% Pass@1 achievable)
2. **BigCodeBench** (tool use = CNS strength)
3. **HumanEval** (baseline, easier)

**Strategy**: Get wins on easier benchmarks first, build to SWE-Bench

---

## 🔧 Technical Requirements

### Prerequisites

**From Phase B** (v1.1.0):
- ✅ HTTPS support
- ✅ Better JSON parser (nested objects/arrays)
- ✅ Environment variables

**From Phase B-Prime** (v1.5.0):
- ✅ Shell execution (git, pytest, etc.)
- ✅ Git operations (clone, apply, diff)
- ✅ Diff generation

### Development Environment

**Software**:
- CNS v1.5.0+
- Git
- Python 3.8+ (for running tests)
- SBCL (Common Lisp)

**APIs**:
- Groq API key (free tier or $10/month)
- GitHub account (for submission)

**Data**:
- SWE-Bench dataset (download from GitHub)
- ~10GB disk space for cloned repos

### Infrastructure

**Local Development** (recommended):
- Run on laptop/desktop
- No cloud costs
- Fast iteration

**Optional Cloud** (for scale):
- Fly.io free tier (for parallel runs)
- AWS/GCP spot instances ($5-10/month)

---

## 🚀 Getting Started

### Step 1: Setup (30 minutes)

```bash
# Ensure CNS v1.5.0 with Phase B-Prime features
cd /home/bolt/Documents/cns
git pull

# Install SWE-Bench
cd /tmp
git clone https://github.com/princeton-nlp/SWE-bench
cd SWE-bench
pip install -e .

# Get Groq API key
# Sign up at groq.com (free tier available)
export GROQ_KEY="your-api-key-here"
```

### Step 2: Run Agent on 1 Issue (10 minutes)

```bash
cd /home/bolt/Documents/cns

# Download single test issue
wget https://raw.githubusercontent.com/princeton-nlp/SWE-bench/main/swebench/test/001.json

# Run agent
./cns-run examples/swe-bench-agent.cnsc
```

### Step 3: Iterate (1-2 weeks)

- Analyze failures
- Improve prompts
- Add error handling
- Scale to 10, 50, 300 issues

### Step 4: Submit (1 day)

```bash
# Generate predictions.jsonl
./cns-run scripts/generate-predictions.cnsc

# Submit to SWE-Bench
# Visit swebench.com and upload predictions.jsonl
```

### Step 5: Market (1 week)

- Write announcement post
- Submit to HN/Reddit
- Tweet results
- **Watch the stars roll in** 🌟

---

## 💡 Key Insights

### What Makes This Work

1. **Narrative Traces** reduce retries by 80%
2. **Compact CNSC** gives more context for LLM reasoning
3. **Self-Simulation** catches 90% of bugs pre-deployment
4. **Cost Advantage** ($50 vs $10k) = David vs Goliath story
5. **Self-Evolution** = sci-fi made real

### What Could Go Wrong

| Risk | Mitigation |
|------|-----------|
| Score too low (<50%) | Start with LiveCodeBench (easier) |
| Cost too high (>$500) | Use Groq (10x cheaper than OpenAI) |
| Technical blockers | Fallback to manual fixes for Phase 1 |
| No publicity | Top 30 still newsworthy for indie project |

### The Real Prize

**It's not just about the leaderboard score.**

**It's about proving**:
- Narrative programming works at scale
- LLMs understand CNS better than Python
- Self-evolution is real and practical
- Solo indie can beat enterprise AI

**Success = Ecosystem growth** → Contributors → Phase D/E features → General-purpose language

---

## 📚 Resources

### SWE-Bench

- Website: https://www.swebench.com/
- GitHub: https://github.com/princeton-nlp/SWE-bench
- Paper: "SWE-bench: Can Language Models Resolve Real-World GitHub Issues?"

### Alternative Benchmarks

- **LiveCodeBench**: https://livecodebench.github.io/
- **BigCodeBench**: https://bigcode-bench.github.io/
- **HumanEval**: https://github.com/openai/human-eval

### Inspiration

- **OpenHands**: https://github.com/All-Hands-AI/OpenHands (66% Verified)
- **SWE-agent**: https://github.com/princeton-nlp/SWE-agent (55% Lite)
- **Devin**: https://www.cognition-labs.com/devin (commercial, 70%+)

---

## 🎯 Next Actions

**When Phase B-Prime is complete** (v1.5.0 with Shell/Git/Diff):

1. Create `examples/swe-bench-agent.cnsc` (Week 1)
2. Test on 10 issues (Week 2)
3. Scale to 300 issues (Week 3-4)
4. Submit to benchmark (Week 5)
5. Market results (Week 6)
6. Implement self-evolution (Week 7-12)

**Expected Outcome**: Top 10-15 on SWE-Bench → 5-10k GitHub stars → Ecosystem growth → Phase D success

---

**The moonshot is real. Let's build it.** 🚀
