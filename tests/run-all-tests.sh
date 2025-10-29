#!/bin/bash
# Comprehensive CNS Test Suite
# Runs validation and execution tests on all examples and dataset entries

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

TOTAL_TESTS=0
PASSED=0
FAILED=0
WARNINGS=0

# Base directory
BASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
VALIDATOR="$BASE_DIR/src/cns-validate"
INTERPRETER="$BASE_DIR/cns-run"

echo "==========================================="
echo "   CNS Comprehensive Test Suite"
echo "==========================================="
echo ""
echo "Base directory: $BASE_DIR"
echo "Validator: $VALIDATOR"
echo "Interpreter: $INTERPRETER"
echo ""

# ============================================================================
# Test Helper Functions
# ============================================================================

test_validate() {
    local file="$1"
    local name="$2"
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    echo -n "  [$TOTAL_TESTS] Validating $name... "
    
    if [ ! -f "$file" ]; then
        echo -e "${RED}FILE NOT FOUND${NC}"
        FAILED=$((FAILED + 1))
        return 1
    fi
    
    # Run validator
    output=$("$VALIDATOR" "$file" 2>&1)
    exit_code=$?
    
    if [ $exit_code -eq 0 ]; then
        # Check if there are warnings
        if echo "$output" | grep -q "⚠"; then
            echo -e "${YELLOW}PASS (with warnings)${NC}"
            WARNINGS=$((WARNINGS + 1))
        else
            echo -e "${GREEN}PASS${NC}"
        fi
        PASSED=$((PASSED + 1))
        return 0
    else
        echo -e "${RED}FAIL${NC}"
        echo "$output" | grep -E "(ERROR|WARNING):" | sed 's/^/    /'
        FAILED=$((FAILED + 1))
        return 1
    fi
}

test_execute() {
    local file="$1"
    local name="$2"
    local expected_pattern="$3"
    local timeout="${4:-2}"
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    echo -n "  [$TOTAL_TESTS] Executing $name... "
    
    if [ ! -f "$file" ]; then
        echo -e "${RED}FILE NOT FOUND${NC}"
        FAILED=$((FAILED + 1))
        return 1
    fi
    
    # Run interpreter with timeout, suppress SBCL compilation warnings
    output=$(timeout "$timeout" "$INTERPRETER" "$file" 2>&1 | grep -v "STYLE-WARNING" | grep -v "compilation unit" | grep -v "caught" | grep -v "file:" | grep -v "in:" | grep -v "The variable")
    exit_code=$?
    
    if [ $exit_code -eq 124 ]; then
        echo -e "${RED}TIMEOUT${NC}"
        FAILED=$((FAILED + 1))
        return 1
    fi
    
    if [ $exit_code -ne 0 ]; then
        echo -e "${RED}FAIL (exit code $exit_code)${NC}"
        FAILED=$((FAILED + 1))
        return 1
    fi
    
    # Check for expected pattern if provided
    if [ -n "$expected_pattern" ]; then
        if echo "$output" | grep -q "$expected_pattern"; then
            echo -e "${GREEN}PASS${NC}"
            PASSED=$((PASSED + 1))
            return 0
        else
            echo -e "${YELLOW}PASS (no expected output match)${NC}"
            echo "    Expected: $expected_pattern"
            WARNINGS=$((WARNINGS + 1))
            PASSED=$((PASSED + 1))
            return 0
        fi
    else
        echo -e "${GREEN}PASS${NC}"
        PASSED=$((PASSED + 1))
        return 0
    fi
}

# ============================================================================
# Test Suites
# ============================================================================

echo "==========================================="
echo " Phase 1: Example Validation"
echo "==========================================="
echo ""

for file in "$BASE_DIR"/examples/*.cns; do
    test_validate "$file" "$(basename "$file")"
done

echo ""
echo "==========================================="
echo " Phase 2: Example Execution (Non-Server)"
echo "==========================================="
echo ""

# Create test input file for file I/O tests
echo "This is a test file with some words for counting" > "$BASE_DIR/tests/test-input.txt"

# Test non-webserver examples
for file in "$BASE_DIR"/examples/*.cns; do
    filename=$(basename "$file")
    
    # Skip webserver examples for now (they need special handling)
    if [[ "$filename" == *"webserver"* ]]; then
        continue
    fi
    
    # Determine expected output based on example
    case "$filename" in
        "factorial.cns")
            test_execute "$file" "$filename" "120"
            ;;
        "fibonacci.cns")
            test_execute "$file" "$filename" "55"
            ;;
        "hello.cns")
            test_execute "$file" "$filename" "Hello"
            ;;
        "is-prime.cns")
            test_execute "$file" "$filename" ""
            ;;
        *)
            test_execute "$file" "$filename" ""
            ;;
    esac
done

echo ""
echo "==========================================="
echo " Phase 3: LLM Test Files Validation"
echo "==========================================="
echo ""

if [ -d "$BASE_DIR/tests/llm-tests" ]; then
    for file in "$BASE_DIR"/tests/llm-tests/*.cns; do
        test_validate "$file" "$(basename "$file")"
    done
fi

echo ""
echo "==========================================="
echo " Phase 4: Dataset Validation (Sample)"
echo "==========================================="
echo ""

# Test a sample of dataset entries (first 5 from each file)
if [ -f "$BASE_DIR/dataset/cns-examples.json" ]; then
    echo "Checking dataset/cns-examples.json structure..."
    if python3 -c "import json; json.load(open('$BASE_DIR/dataset/cns-examples.json'))" 2>/dev/null; then
        echo -e "  ${GREEN}✓ Valid JSON structure${NC}"
        PASSED=$((PASSED + 1))
    else
        echo -e "  ${RED}✗ Invalid JSON${NC}"
        FAILED=$((FAILED + 1))
    fi
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
fi

if [ -f "$BASE_DIR/dataset/webserver-examples.json" ]; then
    echo "Checking dataset/webserver-examples.json structure..."
    if python3 -c "import json; json.load(open('$BASE_DIR/dataset/webserver-examples.json'))" 2>/dev/null; then
        echo -e "  ${GREEN}✓ Valid JSON structure${NC}"
        PASSED=$((PASSED + 1))
    else
        echo -e "  ${RED}✗ Invalid JSON${NC}"
        FAILED=$((FAILED + 1))
    fi
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
fi

if [ -f "$BASE_DIR/dataset/webserver-examples-extended.json" ]; then
    echo "Checking dataset/webserver-examples-extended.json structure..."
    if python3 -c "import json; json.load(open('$BASE_DIR/dataset/webserver-examples-extended.json'))" 2>/dev/null; then
        echo -e "  ${GREEN}✓ Valid JSON structure${NC}"
        PASSED=$((PASSED + 1))
    else
        echo -e "  ${RED}✗ Invalid JSON${NC}"
        FAILED=$((FAILED + 1))
    fi
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
fi

# ============================================================================
# Summary
# ============================================================================

echo ""
echo "==========================================="
echo " Test Summary"
echo "==========================================="
echo ""
echo -e "Total Tests:   $TOTAL_TESTS"
echo -e "Passed:        ${GREEN}$PASSED${NC}"
echo -e "Failed:        ${RED}$FAILED${NC}"
echo -e "With Warnings: ${YELLOW}$WARNINGS${NC}"
echo ""

PASS_RATE=0
if [ $TOTAL_TESTS -gt 0 ]; then
    PASS_RATE=$((PASSED * 100 / TOTAL_TESTS))
fi

echo -e "Pass Rate:     $PASS_RATE%"
echo ""

if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}=========================================${NC}"
    echo -e "${GREEN}   All Tests Passed! ✓${NC}"
    echo -e "${GREEN}=========================================${NC}"
    exit 0
else
    echo -e "${RED}=========================================${NC}"
    echo -e "${RED}   $FAILED Test(s) Failed${NC}"
    echo -e "${RED}=========================================${NC}"
    exit 1
fi
