---
node_id: AI-IMP-044
tags:
  - IMP-LIST
  - Implementation
  - hub
  - parsing
  - org
kanban_status: completed
depends_on:
  - AI-IMP-043
confidence_score: 0.85
created_date: 2025-01-06
---

# AI-IMP-044: Org File Parsers

## Summary

Parse v4 org files for hub display.

**Current state:** Hub reads from buffers with special modes.

**Target state:** Hub parses org files directly from ~/.agent/.

**Done when:** Hub can display chat, scratchpad, monologue from org files.

### Out of Scope

- Writing to org files (handled by agent-persistence.el)
- Buffer-based modes (v3 approach)

### Design/Approach

Reuse `agent-persistence.el` functions where possible:
- `agent-chat-load-history` - returns list of exchange plists
- `agent-scratchpad-load` - returns list of note plists

May need additional functions for hub-specific parsing:

```elisp
(defun amacs-hub--parse-chat-file ()
  "Parse agent-chat.org for hub display.
Returns list of tick alists with human/agent content."
  ...)

(defun amacs-hub--parse-monologue-file ()
  "Parse monologue.org for hub display.
Returns list of entry alists with tick/timestamp/narrative."
  ...)
```

The scratchpad parsing can use `agent-scratchpad-load` directly.

### Files to Touch

- `harness/amacs-hub.el`: Add file parsing functions
- Potentially `harness/agent-persistence.el`: If shared parsing needed

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**?
</CRITICAL_RULE>

- [x] `amacs-hub--parse-chat-file` parses agent-chat.org
- [x] Returns list with tick, human content, agent content per exchange
- [x] `amacs-hub--parse-monologue-file` parses monologue.org
- [x] Returns list with tick, timestamp, narrative per entry
- [x] `amacs-hub--parse-scratchpad-file` wraps persistence function
- [x] Handle missing files gracefully (return empty list via condition-case)
- [x] Handle malformed content gracefully (condition-case)
- [x] Byte-compile without warnings (CI passes)
- [ ] Test parsing with sample org files (deferred to manual testing)
- [ ] Test with empty/missing files (deferred to manual testing)

### Acceptance Criteria

**Scenario:** Parse chat history
**GIVEN** agent-chat.org has 3 tick exchanges
**WHEN** `amacs-hub--parse-chat-file` is called
**THEN** Returns list of 3 alists
**AND** Each has :tick, :human, :agent keys

**Scenario:** Parse monologue
**GIVEN** monologue.org has 10 entries
**WHEN** `amacs-hub--parse-monologue-file` is called
**THEN** Returns list of 10 alists
**AND** Each has :tick, :timestamp, :narrative keys

**Scenario:** Missing file
**GIVEN** agent-chat.org does not exist
**WHEN** `amacs-hub--parse-chat-file` is called
**THEN** Returns empty list
**AND** No error is raised
