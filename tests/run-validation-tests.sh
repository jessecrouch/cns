#!/bin/bash
# CNS Validation Test Suite
# Validates all CNS files for correct syntax and structure

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

TOTAL=0
PASSED=0
FAILED=0
WARNINGS=0

BASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
VALIDATOR="$BASE_DIR/src/cns-validate"

echo "==========================================="
echo "   CNS Validation Test Suite"
echo "==========================================="
echo ""

test_validate() {
    local file="$1"
    local name="$2"
    
    TOTAL=$((TOTAL + 1))
    printf "  [%3d] %-40s " "$TOTAL" "$name"
    
    output=$("$VALIDATOR" "$file" 2>&1)
    exit_code=$?
    
    if [ $exit_code -eq 0 ]; then
        if echo "$output" | grep -q "⚠"; then
            echo -e "${YELLOW}PASS (warnings)${NC}"
            WARNINGS=$((WARNINGS + 1))
        else
            echo -e "${GREEN}PASS${NC}"
        fi
        PASSED=$((PASSED + 1))
    else
        echo -e "${RED}FAIL${NC}"
        FAILED=$((FAILED + 1))
    fi
}

# Test all examples
echo "Testing examples/..."
for file in "$BASE_DIR"/examples/*.cns; do
    test_validate "$file" "$(basename "$file")"
done

echo ""
echo "Testing tests/llm-tests/..."
if [ -d "$BASE_DIR/tests/llm-tests" ]; then
    for file in "$BASE_DIR"/tests/llm-tests/*.cns; do
        [ -f "$file" ] && test_validate "$file" "llm-tests/$(basename "$file")"
    done
fi

echo ""
echo "Testing tests/grok-iterations/..."
if [ -d "$BASE_DIR/tests/grok-iterations" ]; then
    for file in "$BASE_DIR"/tests/grok-iterations/*/*.cns; do
        [ -f "$file" ] && test_validate "$file" "grok/$(basename "$file")"
    done
fi

echo ""
echo "==========================================="
echo " Summary"
echo "==========================================="
echo -e "Total:         $TOTAL"
echo -e "Passed:        ${GREEN}$PASSED${NC}"
echo -e "Failed:        ${RED}$FAILED${NC}"
echo -e "With Warnings: ${YELLOW}$WARNINGS${NC}"

PASS_RATE=0
[ $TOTAL -gt 0 ] && PASS_RATE=$((PASSED * 100 / TOTAL))
echo -e "Pass Rate:     $PASS_RATE%"
echo ""

if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}All validation tests passed! ✓${NC}"
    exit 0
else
    echo -e "${RED}$FAILED test(s) failed${NC}"
    exit 1
fi
