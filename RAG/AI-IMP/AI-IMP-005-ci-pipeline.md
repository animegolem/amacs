---
node_id: AI-IMP-005
tags:
  - IMP-LIST
  - Implementation
  - phase-2
  - ci
  - testing
kanban_status: planned
depends_on: []
confidence_score: 0.9
created_date: 2025-12-06
close_date: 
--- 

# AI-IMP-005-ci-pipeline

## CI Pipeline for AMACS Harness

Implement continuous integration that catches common elisp errors before they reach the agent. This is a prerequisite for Phase 2 infrastructure work - we need confidence that changes don't break the harness.

**Done when:** A shell script runs byte-compilation and tests, returning non-zero exit on failure. Can be integrated with Gitea Actions later.

See: [[AI-EPIC-002-hands-and-arms]]

### Out of Scope 

- Gitea Actions integration (separate IMP once Proxmox is up)
- Coverage reporting
- Performance benchmarks
- Multi-Emacs-version testing

### Design/Approach  

Two-phase validation:

1. **Byte compilation**: Catches undefined functions, setting constants (like `t`), missing requires
2. **Test suite**: Catches runtime bugs like backquote structure sharing

The script should be runnable locally during development and in CI.

**Key decisions:**
- Use `--batch` mode for headless execution
- Set `byte-compile-error-on-warn t` to be strict
- Clean up `.elc` files after compilation (we don't need them)
- Exit with appropriate codes for CI integration

### Files to Touch

```
harness/ci-check.sh           # NEW - Main CI script
harness/test-harness.el       # Modify - Add batch-mode exit code support
```

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**? 
</CRITICAL_RULE> 

- [ ] Create `ci-check.sh`:
  - [ ] Byte-compile all `.el` files except test-harness.el
  - [ ] Fail on any byte-compile warning or error
  - [ ] Run test-harness.el in batch mode
  - [ ] Capture test pass/fail count
  - [ ] Exit 0 on all pass, non-zero on any failure
  - [ ] Clean up `.elc` files after run
  - [ ] Make script executable
- [ ] Modify `test-harness.el`:
  - [ ] Add function to return exit code based on results
  - [ ] Support `--batch` invocation cleanly
  - [ ] Suppress interactive messages in batch mode
- [ ] Test: Introduce deliberate byte-compile error, verify CI catches it
- [ ] Test: Introduce deliberate test failure, verify CI catches it
- [ ] Test: Clean run returns exit code 0
- [ ] Document usage in script header comments
 
### Acceptance Criteria

**Scenario:** Clean codebase passes CI
**GIVEN** All elisp files are valid
**AND** All tests pass
**WHEN** User runs `./ci-check.sh`
**THEN** Script exits with code 0
**AND** Output shows "All checks passed" or similar

**Scenario:** Byte-compile error fails CI
**GIVEN** A file contains `(lambda (t) ...)` or similar error
**WHEN** User runs `./ci-check.sh`
**THEN** Script exits with non-zero code
**AND** Output identifies the problematic file

**Scenario:** Test failure fails CI
**GIVEN** A test assertion fails
**WHEN** User runs `./ci-check.sh`
**THEN** Script exits with non-zero code
**AND** Output shows test failure count

**Scenario:** Missing require fails CI
**GIVEN** A file uses a function without requiring its provider
**WHEN** User runs `./ci-check.sh`
**THEN** Byte-compile catches the undefined function
**AND** Script exits with non-zero code

### Script Template

```bash
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
EXIT_CODE=0

echo "=== AMACS CI Check ==="
echo ""

# Phase 1: Byte compilation
echo "--- Byte Compilation ---"
for f in "$SCRIPT_DIR"/*.el; do
  fname=$(basename "$f")
  if [[ "$fname" != "test-harness.el" ]]; then
    echo -n "Compiling $fname... "
    if emacs -Q --batch \
        -L "$SCRIPT_DIR" \
        --eval "(setq byte-compile-error-on-warn t)" \
        -f batch-byte-compile "$f" 2>&1 | grep -q "Error\|Warning"; then
      echo "FAIL"
      EXIT_CODE=1
    else
      echo "OK"
    fi
  fi
done

# Cleanup .elc files
rm -f "$SCRIPT_DIR"/*.elc

if [[ $EXIT_CODE -ne 0 ]]; then
  echo ""
  echo "Byte compilation failed!"
  exit 1
fi

echo ""
echo "--- Test Suite ---"
TEST_OUTPUT=$(emacs -Q --batch \
    -L "$SCRIPT_DIR" \
    -l test-harness.el \
    --eval "(test-run-all-batch)" 2>&1)

echo "$TEST_OUTPUT"

# Check for failures in output
if echo "$TEST_OUTPUT" | grep -q "FAIL"; then
  echo ""
  echo "Tests failed!"
  exit 2
fi

echo ""
echo "=== All checks passed ==="
exit 0
```

### Issues Encountered 

<!-- Fill during implementation -->
