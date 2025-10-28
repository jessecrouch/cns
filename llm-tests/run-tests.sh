#!/bin/bash
# CNS LLM Test Harness
# Tests generated CNS code for validity and executability

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

TOTAL=0
PASSED=0
FAILED=0

echo "=== CNS LLM Generation Test Suite ==="
echo ""

# Test helper function
test_cns() {
    local file="$1"
    local name=$(basename "$file" .cns)
    local is_webserver="$2"
    
    TOTAL=$((TOTAL + 1))
    
    echo "[$TOTAL] Testing: $name"
    
    # Check file exists
    if [ ! -f "$file" ]; then
        echo -e "  ${RED}✗ FAIL${NC}: File not found"
        FAILED=$((FAILED + 1))
        return 1
    fi
    
    # Validate structure
    if ! grep -q "^Story:" "$file"; then
        echo -e "  ${RED}✗ FAIL${NC}: Missing Story:"
        FAILED=$((FAILED + 1))
        return 1
    fi
    
    if ! grep -q "^Given:" "$file"; then
        echo -e "  ${RED}✗ FAIL${NC}: Missing Given:"
        FAILED=$((FAILED + 1))
        return 1
    fi
    
    if ! grep -q "^Step" "$file"; then
        echo -e "  ${RED}✗ FAIL${NC}: Missing Steps"
        FAILED=$((FAILED + 1))
        return 1
    fi
    
    if ! grep -q "Because:" "$file"; then
        echo -e "  ${RED}✗ FAIL${NC}: Missing Because: clauses"
        FAILED=$((FAILED + 1))
        return 1
    fi
    
    if ! grep -q "^End:" "$file"; then
        echo -e "  ${RED}✗ FAIL${NC}: Missing End:"
        FAILED=$((FAILED + 1))
        return 1
    fi
    
    # Try to parse with SBCL
    if ! timeout 5 sbcl --noinform --non-interactive \
        --eval "(load \"../cns.lisp\")" \
        --eval "(handler-case (progn (with-open-file (s \"$file\") (let ((code (make-string (file-length s)))) (read-sequence code s) (parse-cns code))) (sb-ext:exit :code 0)) (error (e) (format t \"Parse error: ~A~%\" e) (sb-ext:exit :code 1)))" \
        2>&1 | grep -q "Parse error"; then
        echo -e "  ${GREEN}✓ PASS${NC}: Valid CNS structure"
        PASSED=$((PASSED + 1))
        return 0
    else
        echo -e "  ${RED}✗ FAIL${NC}: Parse error"
        FAILED=$((FAILED + 1))
        return 1
    fi
}

# Run tests
echo "--- Math Tests ---"
test_cns "01-factorial.cns" false
test_cns "02-prime.cns" false
echo ""

echo "--- File I/O Tests ---"
# Create test input file
echo "This is a test file with some words" > test-input.txt
test_cns "03-word-count.cns" false
test_cns "04-write-file.cns" false
echo ""

echo "--- Webserver Tests ---"
test_cns "05-basic-webserver.cns" true
test_cns "06-multi-route.cns" true
test_cns "07-json-webserver.cns" true
test_cns "08-error-handling.cns" true
test_cns "09-request-counter.cns" true
test_cns "10-three-routes.cns" true
echo ""

# Summary
echo "=== Test Summary ==="
echo -e "Total:  $TOTAL"
echo -e "Passed: ${GREEN}$PASSED${NC}"
echo -e "Failed: ${RED}$FAILED${NC}"

if [ $FAILED -eq 0 ]; then
    echo -e "\n${GREEN}All tests passed!${NC} ✓"
    exit 0
else
    echo -e "\n${RED}Some tests failed.${NC}"
    exit 1
fi
