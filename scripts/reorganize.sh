#!/bin/bash
# Repository reorganization script

set -e  # Exit on error

echo "Starting repository reorganization..."

# Create directory structure
mkdir -p src
mkdir -p scripts
mkdir -p docs/guides
mkdir -p docs/development
mkdir -p docs/archive
mkdir -p tests/grok-iterations/iteration-1
mkdir -p tests/grok-iterations/iteration-2

# Move core source files
echo "Moving core source files to src/..."
mv cns.lisp src/
mv cns-run src/

# Move generator scripts
echo "Moving generator scripts to scripts/..."
mv generate-dataset.lisp scripts/
mv generate-extended-webservers.lisp scripts/
mv generate-more-webservers.lisp scripts/
mv simple-webserver-gen.lisp scripts/

# Move guide documentation
echo "Moving guide documentation to docs/guides/..."
mv AGENTS.md docs/guides/
mv LLM-INTEGRATION.md docs/guides/
mv LLM-TRAINING-READY.md docs/guides/
mv TEST-WITH-LLM.md docs/guides/
mv NEXT-TEST-PROMPT.md docs/guides/

# Move development documentation
echo "Moving development documentation to docs/development/..."
mv UPDATES.md docs/development/
mv REAL-SOCKETS.md docs/development/
mv DEMO.md docs/development/

# Move archive documentation
echo "Moving archive documentation to docs/archive/..."
mv SESSION_SUMMARY.md docs/archive/
mv SUMMARY.md docs/archive/

# Move Grok iteration files
echo "Moving Grok iteration files..."
mv GROK-FEEDBACK.md tests/grok-iterations/
mv ITERATION-SUMMARY.md tests/grok-iterations/
mv GROK-ITERATION-2-SUCCESS.md tests/grok-iterations/

# Move Grok test files
mv llm-tests/grok-prime-test.cns tests/grok-iterations/iteration-1/
mv llm-tests/grok-prime-corrected.cns tests/grok-iterations/iteration-1/
mv llm-tests/grok-prime-test-composite.cns tests/grok-iterations/iteration-1/
mv llm-tests/grok-sum-range.cns tests/grok-iterations/iteration-2/

# Move llm-tests to tests/
echo "Moving llm-tests to tests/..."
mv llm-tests tests/

echo "Reorganization complete!"
echo ""
echo "New structure:"
tree -L 2 -d
