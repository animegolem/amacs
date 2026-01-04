---
node_id: AI-IMP-037
tags:
  - IMP-LIST
  - Implementation
  - inference
  - api
  - json
kanban_status: planned
depends_on:
  - AI-IMP-036
confidence_score: 0.85
created_date: 2025-01-03
close_date:
---

# AI-IMP-037: Basic Inference

## Summary

Wire up the comint shell to make API calls and display parsed responses.

**Current state:** Shell captures input but does nothing with it.

**Target state:** Input triggers API call, JSON response is parsed, `reply` field displayed in comint.

**Done when:** Human types message, agent responds via API, reply appears formatted in shell.

### Out of Scope

- Full context assembly (IMP-038) - use minimal context for now
- Serialization (IMP-039)
- Eval execution (IMP-040)

### Design/Approach

1. When input is captured, call `amacs-shell--trigger-inference`
2. Build minimal messages array (system prompt + user message)
3. Call existing `agent-api-call`
4. Parse JSON response with error handling
5. Extract `reply` field, format, insert into comint
6. Handle parse errors with auto-retry (max 2 retries)

Response format expected:
```json
{
  "eval": null,
  "reply": "Hello! I see you're...",
  "mood": "curious",
  "confidence": 0.8,
  "monologue": "Responded to greeting"
}
```

### Files to Touch

- `harness/amacs-shell.el`: add inference trigger, response handling
- `harness/agent-inference.el`: adapt or create new inference flow
- `harness/agent-api.el`: may need adjustments for new flow

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**?
</CRITICAL_RULE>

- [ ] Create `amacs-shell--trigger-inference` function
- [ ] Build minimal system prompt (core skill or placeholder)
- [ ] Build user message from captured input
- [ ] Call `agent-api-call` with messages
- [ ] Implement `amacs-shell--parse-response` for JSON parsing
- [ ] Handle markdown code fence extraction (```json ... ```)
- [ ] Extract `reply` field from parsed response
- [ ] Extract `mood`, `confidence`, `monologue` for later use
- [ ] Store parsed fields in buffer-local or global vars
- [ ] Implement parse error detection
- [ ] Implement auto-retry with error message (max 2 retries)
- [ ] Format reply for display (no raw JSON to user)
- [ ] Insert formatted reply via `amacs-shell--insert-response`
- [ ] Show "Thinking..." indicator while API call in progress
- [ ] Clear indicator when response received
- [ ] Test: API call succeeds, reply appears
- [ ] Test: Invalid JSON triggers retry
- [ ] Test: After max retries, show error to user
- [ ] Byte-compile without warnings

### Acceptance Criteria

**Scenario:** Successful inference
**GIVEN** Shell is running and API is configured
**WHEN** Human types "What can you do?" and presses RET
**THEN** "Thinking..." appears briefly
**AND** Agent's reply text appears formatted in buffer
**AND** New prompt appears

**Scenario:** Parse error with retry
**GIVEN** API returns malformed JSON
**WHEN** Response cannot be parsed
**THEN** Retry is triggered with error context
**AND** If retry succeeds, reply appears
**AND** If max retries exceeded, error message shown

### Issues Encountered

<!-- Fill during implementation -->
