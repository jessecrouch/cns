#!/bin/bash
# Test all CNS example files

PASS=0
FAIL=0
TIMEOUT=0

echo "Testing all CNS examples..."
echo "=============================="
echo

for file in examples/core/*.cns examples/core/*.cnsc examples/features/*.cns examples/features/*.cnsc examples/advanced/*.cns examples/advanced/*.cnsc; do
    # Skip if no files found
    [ -e "$file" ] || continue
    
    filename=$(basename "$file")
    category=$(basename $(dirname "$file"))
    printf "Testing %-10s %-45s ... " "[$category]" "$filename"
    
    # Run with 5 second timeout
    output=$(timeout 5 ./cns-run "$file" 2>&1)
    exit_code=$?
    
    if [ $exit_code -eq 124 ]; then
        echo "TIMEOUT"
        ((TIMEOUT++))
    elif [ $exit_code -eq 0 ]; then
        # Check if output contains error
        if echo "$output" | grep -q "CNS ERROR\|Error occurred"; then
            echo "FAIL (runtime error)"
            ((FAIL++))
        else
            echo "PASS"
            ((PASS++))
        fi
    else
        echo "FAIL (exit code $exit_code)"
        ((FAIL++))
    fi
done

echo
echo "=============================="
echo "Results:"
echo "  PASS:    $PASS"
echo "  FAIL:    $FAIL"
echo "  TIMEOUT: $TIMEOUT"
echo "  TOTAL:   $((PASS + FAIL + TIMEOUT))"
echo "=============================="

if [ $FAIL -eq 0 ] && [ $TIMEOUT -eq 0 ]; then
    echo "✓ All tests passed!"
    exit 0
else
    echo "✗ Some tests failed"
    exit 1
fi
