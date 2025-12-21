#!/bin/bash
# ci-check.sh - AMACS harness CI validation
#
# Usage: ./ci-check.sh
#
# Runs byte-compilation and test suite.
# Exit codes:
#   0 - All checks passed
#   1 - Byte-compilation failed
#   2 - Test suite failed

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "=== AMACS CI Check ==="
echo ""

# Phase 1: Byte compilation
echo "--- Byte Compilation ---"
COMPILE_FAILED=0

for f in "$SCRIPT_DIR"/*.el; do
  fname=$(basename "$f")
  # Skip test-harness.el - it has test-specific code
  if [[ "$fname" == "test-harness.el" ]]; then
    continue
  fi

  echo -n "Compiling $fname... "

  # Capture both stdout and stderr, check exit code
  OUTPUT=$(emacs -Q --batch \
      -L "$SCRIPT_DIR" \
      --eval "(setq byte-compile-error-on-warn t)" \
      -f batch-byte-compile "$f" 2>&1) || {
    echo "FAIL"
    echo "$OUTPUT" | grep -E "Error|Warning|error:|warning:" | head -5
    COMPILE_FAILED=1
    continue
  }

  # Check for warnings/errors in output even if exit was 0
  if echo "$OUTPUT" | grep -qE "Error|Warning|error:|warning:"; then
    echo "FAIL"
    echo "$OUTPUT" | grep -E "Error|Warning|error:|warning:" | head -5
    COMPILE_FAILED=1
  else
    echo "OK"
  fi
done

# Cleanup .elc files
rm -f "$SCRIPT_DIR"/*.elc

if [[ $COMPILE_FAILED -ne 0 ]]; then
  echo ""
  echo "Byte compilation failed!"
  exit 1
fi

echo ""
echo "--- Test Suite ---"

# Run tests in batch mode (disable set -e for this command)
set +e
TEST_OUTPUT=$(emacs -Q --batch \
    -L "$SCRIPT_DIR" \
    -l test-harness.el \
    --eval "(test-run-all-batch)" 2>&1)
TEST_EXIT=$?
set -e

echo "$TEST_OUTPUT"

# Check exit code from test run
if [[ $TEST_EXIT -ne 0 ]]; then
  echo ""
  echo "Tests failed!"
  exit 2
fi

# Also check for FAIL in output as backup
if echo "$TEST_OUTPUT" | grep -q "^FAIL:"; then
  echo ""
  echo "Tests failed!"
  exit 2
fi

echo ""
echo "=== All checks passed ==="
exit 0
