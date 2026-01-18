---
node_id: AI-IMP-054
tags:
  - IMP-LIST
  - Implementation
  - EPIC-007
  - testing
kanban_status: backlog
depends_on:
  - AI-IMP-048
  - AI-IMP-049
  - AI-IMP-050
  - AI-IMP-051
  - AI-IMP-052
confidence_score: 0.85
created_date: 2025-01-11
close_date:
---

# AI-IMP-054-test-suite-update

## Summary

Update the test suite to cover the new consciousness-driven architecture. Remove tests for deleted shell code, add tests for inference layer integration, skill binding, and buffer hydration.

**Current state**: Tests exist for shell functions that will be removed. v3 module tests may exist but not run regularly.

**Target state**: Comprehensive test coverage for:
- Consciousness state management
- Inference layer (context assembly, API integration)
- Skill loading and binding
- Buffer hydration
- Shell UI (minimal, just display)

**Done when**: `./harness/ci-check.sh` passes with new architecture. All critical paths tested.

### Out of Scope

- Integration tests requiring real API calls (mock is fine)
- Performance testing
- UI/UX testing

### Design/Approach

1. Audit existing tests in `test-harness.el`
2. Remove tests for deleted shell functions (`amacs-shell--build-messages`, etc.)
3. Add/update tests for:
   - `agent-consciousness.el` - state get/set, initialization
   - `agent-inference.el` - context assembly, message formatting
   - `agent-skills.el` - list, bind, load
   - `agent-context.el` - buffer hydration, skill inclusion
4. Mock API calls (don't require real OpenRouter in tests)
5. Ensure tests are deterministic and fast

Test categories:
- Unit tests: individual functions
- Integration tests: multi-module workflows
- Regression tests: ensure old functionality still works

### Files to Touch

`harness/test-harness.el`: Main test file
`harness/*.el`: May need test helper functions

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**?
</CRITICAL_RULE>

- [ ] Audit existing test-harness.el for obsolete tests
- [ ] Remove tests for deleted shell functions:
  - [ ] `amacs-shell--build-messages` tests
  - [ ] `amacs-shell--build-context` tests
  - [ ] `amacs-shell--system-prompt` tests
- [ ] Add/verify consciousness tests:
  - [ ] `(agent-get 'field)` returns correct value
  - [ ] `(agent-set 'field value)` updates state
  - [ ] `(agent-current-tick)` returns current tick
  - [ ] `(agent-increment-tick)` increments correctly
  - [ ] Persistence: save and load consciousness
- [ ] Add inference layer tests:
  - [ ] `(agent-build-system-prompt)` returns skill content
  - [ ] `(agent-format-messages)` produces valid structure
  - [ ] Context includes all sections (consciousness, chat, etc.)
- [ ] Add skill system tests:
  - [ ] `(agent-list-available-skills)` finds skills
  - [ ] `(agent-bind-skill-to-thread "name")` updates thread
  - [ ] Bound skill content appears in context
- [ ] Add buffer hydration tests:
  - [ ] Buffer added to thread appears in context
  - [ ] Missing buffer handled gracefully
  - [ ] Large buffer truncated
- [ ] Add thread integration tests:
  - [ ] Create thread via consciousness
  - [ ] Switch thread updates active-thread
  - [ ] Complete thread moves to completed list
- [ ] Ensure all tests pass: `./harness/ci-check.sh`
- [ ] Document test coverage in comments

### Acceptance Criteria

**Scenario:** CI passes with new architecture
**GIVEN** EPIC-007 changes are complete
**WHEN** `./harness/ci-check.sh` is run
**THEN** all tests pass
**AND** no byte-compile warnings
**AND** test count is >= previous count

**Scenario:** Critical paths tested
**GIVEN** the test suite
**WHEN** reviewing test coverage
**THEN** consciousness state management is tested
**AND** inference layer context assembly is tested
**AND** skill binding is tested
**AND** buffer hydration is tested

### Issues Encountered

<!-- This section filled during implementation -->
