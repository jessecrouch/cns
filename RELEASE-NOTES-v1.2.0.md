# CNS v1.2.0 Release Notes
**Release Date**: 2025-10-31  
**Codename**: "Pattern & Time"

---

## 🎯 Overview

CNS v1.2.0 adds **regex pattern matching** and **date/time operations**, completing Phase B Week 2 goals ahead of schedule. These features enable text processing, validation, scheduling, and log parsing - critical capabilities for web backends and data processing.

**Key Achievement**: Both features implemented in **1 day** (planned: 2-3 days)

---

## ✨ New Features

### 1. Regex Pattern Matching (Optional: requires cl-ppcre)

Perl-compatible regular expressions for text processing and validation.

#### MATCHES Operator
Pattern matching that returns boolean:
```cns
# Email validation
Then: is_valid becomes email MATCHES "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"

# Phone number detection
Then: has_phone becomes text MATCHES "\\d{3}-\\d{3}-\\d{4}"

# URL detection
Then: is_url becomes text MATCHES "https?://.*"
```

#### EXTRACT Operator
Extract first match or specific capture groups:
```cns
# Extract first match
Then: phone becomes EXTRACT "\\d{3}-\\d{3}-\\d{4}" FROM text

# Extract capture groups
Then: date becomes EXTRACT "\\[(\\d{4}-\\d{2}-\\d{2})" GROUP 1 FROM log_line
Then: time becomes EXTRACT "\\[(\\d{4}-\\d{2}-\\d{2}) (\\d{2}:\\d{2}:\\d{2})\\]" GROUP 2 FROM log_line

# Extract username from email
Then: username becomes EXTRACT "^([^@]+)@" GROUP 1 FROM email
```

**Features:**
- Full PCRE (Perl-Compatible Regular Expressions) syntax
- Capture group extraction (GROUP 1, GROUP 2, etc.)
- GROUP 0 returns full match
- Graceful fallback when cl-ppcre not installed
- Clear installation instructions

**Installation:**
```bash
sbcl --eval "(ql:quickload :cl-ppcre)" --eval "(quit)"
```
See [INSTALL-REGEX.md](INSTALL-REGEX.md) for details.

**Examples:**
- `examples/test-regex.cns` - Comprehensive test with 15 test cases
- `examples/test-regex-simple.cns` - Quick demonstration

---

### 2. Date/Time Operations (Zero Dependencies)

Built-in date/time operations using Common Lisp's standard library.

#### Core Functions

**NOW()** - Get current universal time:
```cns
Then: current_time becomes NOW()
```
Returns: Integer (seconds since 1900-01-01 00:00:00 UTC)

**TIMESTAMP()** - Get formatted current time:
```cns
Then: timestamp becomes TIMESTAMP()
```
Returns: String in ISO 8601 format: "YYYY-MM-DD HH:mm:SS"

#### FORMAT TIME Operator

Custom time formatting:
```cns
# Date only
Then: date_str becomes FORMAT TIME now WITH "YYYY-MM-DD"

# Time only
Then: time_str becomes FORMAT TIME now WITH "HH:mm:SS"

# Custom format
Then: full_str becomes FORMAT TIME now WITH "YYYY-MM-DD HH:mm:SS"
```

**Format tokens:**
- `YYYY` - 4-digit year (e.g., 2025)
- `MM` - 2-digit month (01-12)
- `DD` - 2-digit day (01-31)
- `HH` - 2-digit hour (00-23)
- `mm` - 2-digit minute (00-59)
- `SS` - 2-digit second (00-59)

#### Time Arithmetic

Add days, hours, or minutes to any time value:
```cns
# Add days
Then: tomorrow becomes ADD DAYS now BY 1
Then: next_week becomes ADD DAYS now BY 7

# Add hours
Then: next_hour becomes ADD HOURS now BY 1
Then: deadline becomes ADD HOURS start_time BY 24

# Add minutes
Then: in_30_mins becomes ADD MINUTES now BY 30
Then: meeting_end becomes ADD MINUTES meeting_start BY 90
```

#### Time Comparisons

Universal time is just an integer, so comparisons work naturally:
```cns
# Check if time is in the future
If: event_time > now
  Then: is_future becomes 1

# Check if deadline passed
If: deadline < now
  Then: is_overdue becomes 1

# Check time range
If: event_time > start_time AND event_time < end_time
  Then: is_during_event becomes 1
```

**Features:**
- Zero external dependencies (uses Common Lisp built-ins)
- Universal time for easy comparisons
- Simple arithmetic (just integer addition)
- Portable across all Common Lisp implementations

**Examples:**
- `examples/test-datetime-simple.cns` - Basic demonstration
- `examples/test-datetime.cns` - Comprehensive test

---

## 📚 Documentation Updates

### New Guides
- **INSTALL-REGEX.md** - Complete guide to installing cl-ppcre
  - Quicklisp installation
  - Manual installation
  - Troubleshooting
  - Usage examples

### Updated Documentation
- **README.md** - Added regex and date/time syntax examples
- **ROADMAP.md** - Marked regex and date/time as complete
- **examples/README.md** - Added regex and date/time example descriptions

### New Examples
- `test-regex.cns` - 15 comprehensive regex test cases
- `test-regex-simple.cns` - Quick regex demonstration  
- `test-datetime.cns` - 18 comprehensive date/time tests
- `test-datetime-simple.cns` - Quick date/time demo

---

## 🔧 Technical Details

### Implementation Notes

**Regex Support:**
- Integrates CL-PPCRE library via dynamic loading
- Graceful degradation when library not available
- Uses `funcall` with `intern` to avoid compile-time dependency
- Clear error messages guide users to installation

**Date/Time Support:**
- Uses Common Lisp standard: `get-universal-time`, `decode-universal-time`
- Universal time = seconds since 1900-01-01 00:00:00 UTC
- Simple token replacement for FORMAT TIME
- Time arithmetic via integer addition (86400 sec/day, 3600 sec/hour, 60 sec/min)
- No timezone handling (all UTC) - simple and predictable

### Code Changes
- **src/cns.lisp**: +185 lines
  - Lines 20-30: CL-PPCRE loading with fallback
  - Lines 58-69: `replace-all` helper function
  - Lines 1302-1312: NOW() and TIMESTAMP() functions
  - Lines 1544-1610: MATCHES operator
  - Lines 1612-1677: EXTRACT operator with GROUP support
  - Lines 1679-1771: FORMAT TIME and time arithmetic operators

---

## 🧪 Testing

### Test Coverage
- ✅ All existing tests pass (59/59)
- ✅ Factorial, Fibonacci, Prime number tests
- ✅ JSON parsing (nested objects, arrays)
- ✅ HTTP operations
- ✅ New regex examples
- ✅ New date/time examples

### Validation
Tested on:
- SBCL 2.1.11 on Linux
- With and without cl-ppcre installed
- With and without cl+ssl installed
- All graceful fallbacks working correctly

---

## 📊 Statistics

### Development Metrics
- **Time**: 1 day (planned: 2-3 days)
- **Files Changed**: 15 files
- **Lines Added**: +507 lines
- **New Examples**: 6 files
- **Breaking Changes**: 0
- **Dependencies Added**: 1 optional (cl-ppcre)

### Feature Comparison

| Feature | v1.1.0 | v1.2.0 |
|---------|--------|--------|
| HTTP/HTTPS | ✅ | ✅ |
| JSON Parser | ✅ (enhanced) | ✅ |
| Environment Variables | ✅ | ✅ |
| Regex | ❌ | ✅ |
| Date/Time | ❌ | ✅ |
| Functions | ✅ | ✅ |
| Optional Dependencies | 1 (cl+ssl) | 2 (cl+ssl, cl-ppcre) |

---

## 🚀 Migration Guide

### From v1.1.0 to v1.2.0

**No breaking changes!** All v1.1.0 code continues to work.

#### New Capabilities Available

**Add regex validation:**
```cns
# Before: manual string checks
If: email CONTAINS "@" AND email CONTAINS "."
  Then: might_be_valid becomes 1

# After: proper regex validation
Then: is_valid becomes email MATCHES "^[\\w\\.-]+@[\\w\\.-]+\\.\\w+$"
```

**Add timestamp tracking:**
```cns
# Before: no time tracking
Then: log_message becomes "Event occurred"

# After: with timestamps
Then: timestamp becomes TIMESTAMP()
Then: log_message becomes timestamp + " - Event occurred"
```

**Add time-based logic:**
```cns
# New capability: schedule future events
Then: now becomes NOW()
Then: meeting_start becomes ADD HOURS now BY 2
Then: reminder_time becomes ADD MINUTES meeting_start BY -15

If: now > reminder_time
  Then: PRINT "Send meeting reminder"
```

---

## 🎯 Use Cases

### Text Processing & Validation
- Email validation
- Phone number extraction
- URL parsing
- Log file parsing
- Data sanitization
- Pattern matching

### Scheduling & Time Management
- Event scheduling
- Deadline tracking
- Log timestamps
- Session timeouts
- Rate limiting
- Time-based workflows

### Web Backend Operations
- Request validation (email, phone, URLs)
- Access log parsing
- Session management
- API rate limiting
- Scheduled tasks
- Cache expiration

---

## 🔮 What's Next

### v1.3.0 - Database Support (Target: Week 3-4)
- SQLite integration
- PostgreSQL support
- Query operations
- Transaction support

### v2.0.0 - General Purpose Language (Target: 6 weeks)
- File system operations
- Process management
- Network operations
- Crypto/hashing
- Compression
- Complete stdlib

See [ROADMAP.md](docs/development/ROADMAP.md) for full development plan.

---

## 📝 Full Changelog

### Added
- Regex operators: MATCHES, EXTRACT with GROUP support
- Date/time functions: NOW(), TIMESTAMP()
- Date/time operators: FORMAT TIME, ADD DAYS, ADD HOURS, ADD MINUTES
- INSTALL-REGEX.md installation guide
- 6 new example files
- replace-all helper function

### Changed
- README.md: Added regex and date/time documentation
- ROADMAP.md: Updated Phase B progress
- examples/README.md: Added new example descriptions

### Technical
- CL-PPCRE integration with graceful fallback
- Common Lisp time functions integration
- Token-based time formatting
- Time arithmetic operators

---

## 🙏 Acknowledgments

- **CL-PPCRE** by Edi Weitz - Excellent Perl-compatible regex library
- **Common Lisp** standard - Robust built-in time functions
- **Community feedback** - Shaped feature priorities

---

## 📞 Support & Feedback

- **GitHub Issues**: Bug reports and feature requests
- **GitHub Discussions**: Questions and community support
- **Examples**: See `examples/` directory for working code

---

**CNS v1.2.0 - Pattern & Time**  
**Building the future of LLM-friendly programming** 🚀
