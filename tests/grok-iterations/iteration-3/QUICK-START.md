# Quick Start: Test Grok on Request Logger

## 1. Give This Prompt to Grok

Copy contents of `PROMPT.md` to Grok.

## 2. Save Grok's Output

Save as `grok-request-logger.cns` in this directory.

## 3. Test It

```bash
# Start server
./cns-run tests/grok-iterations/iteration-3/grok-request-logger.cns

# In another terminal, test it:
curl http://localhost:8080/          # Should log this
curl http://localhost:8080/          # Should log this  
curl http://localhost:8080/history   # Should show 2 logged requests

# Check CSV file:
cat requests.csv
# Expected: 2 lines with timestamps

# Stop server:
Ctrl+C (or pkill -f grok-request-logger)
```

## 4. Compare Results

- Does it work? ✅ / ❌
- CSV created? ✅ / ❌  
- Timestamps readable? ✅ / ❌
- /history works? ✅ / ❌
- /history NOT logged? ✅ / ❌

## 5. Compare Code

```bash
# Side by side comparison:
diff reference-request-logger.cns grok-request-logger.cns
```

## Reference Implementation

See `reference-request-logger.cns` for a working example.

## Need Help?

- **TESTING.md** - Detailed manual testing
- **README.md** - Complete documentation
- **EXPECTED-OUTPUT.md** - What output should look like

---

**That's it! Ready to test Grok's CNS skills.** 🚀
