# TODO: v1.2.1 Cleanup

## CNSC Conversion (Low Priority)

The v1.2.0 regex and datetime examples were written in verbose CNS format, violating our CNSC-first policy (ROADMAP.md line 78-103).

**Files to convert:**
- `examples/test-regex.cns` (90 lines) → `test-regex.cnsc`
- `examples/test-datetime.cns` (94 lines) → `test-datetime.cnsc`

**Action:** Convert to CNSC using the `cns-compact` tool (once implemented) or manually following [CNSC-COMPACT.md](../guides/CNSC-COMPACT.md)

**Priority:** LOW - examples work fine, this is just for consistency and context efficiency

**Note:** Simple examples (test-regex-simple.cns, test-datetime-simple.cns, test-now.cns, test-now2.cns) are <30 lines so they're fine as `.cns` per the policy.

---

**Last Updated:** 2025-10-31
