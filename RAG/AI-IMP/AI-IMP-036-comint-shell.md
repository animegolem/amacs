---
node_id: AI-IMP-036
tags:
  - IMP-LIST
  - Implementation
  - comint
  - shell
  - interface
kanban_status: completed
depends_on: []
confidence_score: 0.85
created_date: 2025-01-03
close_date: 2025-01-04
---

# AI-IMP-036: Comint Shell

## Summary

Create the comint-based shell buffer that serves as the primary human-agent I/O channel.

**Current state:** Org-mode prompt blocks with buffer navigation (caused 60-tick debugging session).

**Target state:** Single `*amacs-shell*` comint buffer. Human types at prompt, presses enter, agent reply appears inline.

**Done when:** Human can type a message, press enter, and see a placeholder response (actual inference in IMP-037).

### Out of Scope

- Actual API calls (IMP-037)
- Response parsing (IMP-037)
- Context assembly (IMP-038)
- Serialization to org files (IMP-039)

### Design/Approach

Use `comint-mode` with a fake process (no real subprocess). Override `comint-input-sender` to intercept human input and call our harness. Insert agent responses directly into buffer.

Key functions:
- `amacs-shell-mode`: derived from comint-mode
- `amacs-shell--input-sender`: intercepts input, stores for processing
- `amacs-shell--insert-response`: formats and inserts agent reply
- `amacs-shell-start`: creates/switches to shell buffer

Prompt format: `amacs> `

### Files to Touch

- `harness/amacs-shell.el`: new file, comint shell implementation
- `harness/agent-core.el`: add shell startup to init

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**?
</CRITICAL_RULE>

- [x] Create `harness/amacs-shell.el` with header and requires
- [x] Define `amacs-shell-mode` derived from comint-mode
- [x] Define `amacs-shell-prompt` variable (default "amacs> ")
- [x] Implement fake process setup (no real subprocess)
- [x] Implement `amacs-shell--input-sender` to capture input
- [x] Store captured input in variable for harness to read
- [x] Implement `amacs-shell--insert-response` for agent output
- [x] Format response with visual separation from prompt
- [x] Implement `amacs-shell-start` to create/show buffer
- [x] Add prompt insertion after response
- [ ] Wire up to agent-core.el init (optional auto-start) - deferred, not needed for MVP
- [x] Test: buffer creates successfully
- [x] Test: can type and press enter
- [x] Test: placeholder response appears
- [x] Test: new prompt appears after response
- [x] Byte-compile without warnings

### Acceptance Criteria

**Scenario:** Human starts shell and sends message
**GIVEN** Emacs is running with harness loaded
**WHEN** User calls `amacs-shell-start`
**THEN** Buffer `*amacs-shell*` appears with prompt "amacs> "

**Scenario:** Human sends input
**GIVEN** Shell buffer is active at prompt
**WHEN** Human types "hello" and presses RET
**THEN** Input is captured by harness
**AND** Placeholder response appears (e.g., "[Processing...]")
**AND** New prompt appears below

### Issues Encountered

None. Implementation went smoothly:
- Fake process using `cat` works well for comint satisfaction
- `comint-input-sender` override cleanly intercepts input
- Read-only prompt properties prevent user editing prompt text
