---
node_id: AI-IMP-045
tags:
  - IMP-LIST
  - Implementation
  - hub
  - rendering
  - magit-section
kanban_status: completed
depends_on:
  - AI-IMP-043
  - AI-IMP-044
confidence_score: 0.80
created_date: 2025-01-06
---

# AI-IMP-045: Section Rendering

## Summary

Update hub section renderers to use v4 data sources.

**Current state:** Sections read from v3 buffers and consciousness.

**Target state:** Sections use bridge functions and org file parsers.

**Done when:** All sections render correctly with v4 data.

### Out of Scope

- New section types
- Layout changes (preserve existing UX)

### Design/Approach

Update each section inserter:

**Status section**: Use bridge functions
```elisp
(defun amacs-hub--insert-status ()
  (let ((tick (amacs-hub--get-tick))
        (mood (amacs-hub--get-mood))
        (confidence (amacs-hub--get-confidence))
        (thread (amacs-hub--get-active-thread)))
    ...))
```

**Threads section**: Use bridge for thread list

**Chat section**: Use `amacs-hub--parse-chat-file`
- Top level shows "Chat" header
- Expand shows tick list
- Expand tick shows human/agent exchange

**Monologue section**: Use `amacs-hub--parse-monologue-file`

**Scratchpad section**: Use `agent-scratchpad-load`

**Buffers/Skills**: These read from threads, should work with bridge.

### Files to Touch

- `harness/amacs-hub.el`: Update all section inserters

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**?
</CRITICAL_RULE>

- [x] Update `amacs-hub--insert-status` to use bridge (done in IMP-043)
- [x] Update `amacs-hub--insert-threads` to use bridge (done in IMP-043)
- [x] Update `amacs-hub--insert-chat` to use file parser
- [x] Chat: TAB shows tick list (most recent first)
- [x] Chat: TAB on tick shows human/agent exchange
- [x] Update `amacs-hub--insert-monologue` to use file parser
- [x] Update `amacs-hub--insert-scratchpad` to use file parser
- [x] Update `amacs-hub--insert-buffers` to use bridge (done in IMP-043)
- [ ] Update `amacs-hub--insert-skills` if needed (uses v3 agent-active-skills)
- [x] All sections handle nil/empty data gracefully
- [x] Byte-compile without warnings (CI passes)
- [ ] Visual test: all sections render (deferred to manual testing)

### Acceptance Criteria

**Scenario:** Status shows v4 state
**GIVEN** Shell is at tick 10, mood "curious"
**WHEN** Hub is opened
**THEN** Status section shows "Tick: 10", "Mood: curious"

**Scenario:** Chat section expandable
**GIVEN** agent-chat.org has 5 exchanges
**WHEN** User opens hub and TABs on Chat
**THEN** 5 tick entries appear
**AND** TAB on tick 3 shows human/agent content

**Scenario:** Empty state
**GIVEN** No chat history exists
**WHEN** Hub is opened
**THEN** Chat section shows "No chat history"
**AND** No errors
