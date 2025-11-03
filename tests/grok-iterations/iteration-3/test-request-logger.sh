#!/bin/bash

# Test script for Request Logger server
# This script assumes the server is already running

echo "=== Testing Request Logger Server ==="
echo ""

# Clean up old CSV file
rm -f requests.csv

echo "1. Testing main endpoint (3 requests)..."
curl -s http://localhost:8080/ && echo ""
sleep 1
curl -s http://localhost:8080/ && echo ""
sleep 1
curl -s http://localhost:8080/ && echo ""
sleep 1

echo ""
echo "2. Checking CSV file was created..."
if [ -f requests.csv ]; then
    echo "✓ requests.csv exists"
    echo ""
    echo "CSV Contents:"
    cat requests.csv
else
    echo "✗ requests.csv not found!"
    exit 1
fi

echo ""
echo "3. Testing history endpoint..."
curl -s http://localhost:8080/history
echo ""

echo ""
echo "4. Verifying history endpoint wasn't logged..."
echo "CSV line count (should be 4: header + 3 requests):"
wc -l < requests.csv

echo ""
echo "=== Test Complete ==="
echo ""
echo "Expected results:"
echo "- 3 GET / requests logged"
echo "- CSV has header + 3 data lines (4 total)"
echo "- /history request NOT logged"
echo "- Timestamps are present and readable"
