---
node_id: AI-IMP-056
tags:
  - IMP-LIST
  - Implementation
  - EPIC-007
  - shell
  - autonomous
  - tick
kanban_status: done
depends_on:
  - AI-IMP-055
confidence_score: 0.95
created_date: 2025-01-19
close_date: 2025-01-19
---

# AI-IMP-056-autonomous-tick-mechanism

## Summary

Enable agent to request continuation ticks without human input. This implements FR-11 from EPIC-007: "Agent shall be able to run ticks autonomously without human prompt."

**Current state**: Inference only triggers on human input via comint. Agent cannot work autonomously.

**Target state**: Agent can return `{"continue": true, ...}` to request another tick after current one completes. Enables multi-tick autonomous work.

**Done when**: Agent can chain N ticks by returning `continue: true`, with safety limit preventing runaway.

### Out of Scope

- Timer-based scheduled ticks (future enhancement)
- External API to trigger ticks (future enhancement)
- Parallel/concurrent ticks

### Design/Approach

**Mechanism**: Add optional `continue` boolean field to response format.

After processing response:
1. Check if `continue` is true
2. If true, schedule next tick via `run-at-time` with short delay (0.1s)
3. Increment autonomous tick counter
4. If counter exceeds `autonomous-tick-limit`, stop and warn

**Safety measures**:
- `autonomous-tick-limit` in consciousness (default: 10)
- Counter resets on human input
- Limit logged when hit (not silent failure)

**Response format update**:
```json
{
  "continue": true,  // Optional: request another tick
  "mood": "focused",
  "confidence": 0.85,
  "monologue": "Continuing work..."
}
```

### Files to Touch

`harness/amacs-shell.el`: Add continue field handling after response processing
`harness/agent-consciousness.el`: Add `autonomous-tick-limit` and counter fields
`RAG/draft-prompt.md`: Document `continue` field
`skills/amacs-bootstrap-skill/core/SKILL.md`: Document `continue` field
`harness/test-harness.el`: Add tests for autonomous ticks

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**?
</CRITICAL_RULE>

- [x] Add to consciousness schema:
  - [x] `autonomous-tick-limit` (default 10)
  - [x] `autonomous-tick-counter` (default 0)
- [x] Modify response handling in `amacs-shell.el`:
  - [x] Extract `continue` field from parsed response
  - [x] If `continue` is true, schedule next tick via `run-at-time`
  - [x] Check counter against limit before scheduling
  - [x] Increment counter on autonomous tick
  - [x] Reset counter on human input
- [x] Log when limit reached (message or monologue)
- [x] Update `RAG/draft-prompt.md` response format documentation
- [x] Update `skills/.../core/SKILL.md` response format
- [x] Add test: `continue: true` detected in response
- [x] Add test: autonomous tick fields exist in consciousness
- [x] Add test: counter can be incremented
- [x] Run CI: `./harness/ci-check.sh` - **132/132 tests pass**

### Acceptance Criteria

**Scenario:** Agent requests continuation
**GIVEN** agent returns `{"continue": true, "mood": "focused", "confidence": 0.8, "monologue": "Step 1"}`
**WHEN** shell processes the response
**THEN** a second tick is scheduled after short delay
**AND** `autonomous-tick-counter` is incremented

**Scenario:** Autonomous tick limit reached
**GIVEN** `autonomous-tick-limit` is 10
**AND** agent has run 10 autonomous ticks
**WHEN** agent returns `{"continue": true, ...}`
**THEN** no further tick is scheduled
**AND** a warning is logged
**AND** agent can still respond to human input

**Scenario:** Human input resets counter
**GIVEN** `autonomous-tick-counter` is 5
**WHEN** human sends a message
**THEN** counter is reset to 0

### Issues Encountered

**Implemented clean separation**: The `amacs-shell--handle-continue` function checks limits and schedules ticks, keeping the logic modular. A separate `amacs-shell--trigger-autonomous-tick` function handles the actual tick trigger for clarity.

**Counter reset on human input**: Added counter reset in `amacs-shell--trigger-inference` to ensure autonomous tick count resets whenever human sends new input, preventing limit exhaustion across sessions.

**Limit logging**: When limit is reached, both a message and monologue entry are created so the event is visible in both the minibuffer and git history.
