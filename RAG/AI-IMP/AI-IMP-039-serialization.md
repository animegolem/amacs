---
node_id: AI-IMP-039
tags:
  - IMP-LIST
  - Implementation
  - serialization
  - persistence
  - org-mode
kanban_status: planned
depends_on:
  - AI-IMP-038
confidence_score: 0.80
created_date: 2025-01-03
close_date:
---

# AI-IMP-039: Serialization

## Summary

Persist chat history and scratchpad to org files for survival across restarts and git history.

**Current state:** Context assembled from memory only.

**Target state:** Chat exchanges written to `agent-chat.org`, scratchpad to `scratchpad.org`, loaded on warm start.

**Done when:** Restart Emacs, prior chat and notes are preserved and appear in context.

### Out of Scope

- Eval execution (IMP-040)
- Git commits (IMP-042)

### Design/Approach

**agent-chat.org structure:**
```org
* Tick 42
:PROPERTIES:
:TIMESTAMP: 2025-01-02T14:30:00Z
:END:

** Human
What's causing the ownership error?

** Agent
Looking at main.rs line 45...
```

**scratchpad.org structure:**
```org
* Working Notes
:PROPERTIES:
:THREAD: null
:TICK: 42
:END:
Content here...

* Parser Investigation
:PROPERTIES:
:THREAD: rust-debugging
:TICK: 45
:END:
Thread-specific content...
```

On inference complete:
1. Append completed pair to agent-chat.org
2. If scratchpad field present, append under heading

On startup:
1. Load chat history from agent-chat.org (parse org)
2. Load scratchpad headings from scratchpad.org

### Files to Touch

- `harness/agent-chat.el`: new serialization functions (or new file)
- `harness/agent-scratchpad.el`: add serialization, thread properties
- `harness/amacs-shell.el`: call serialization after inference
- `harness/agent-core.el`: load on init

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**?
</CRITICAL_RULE>

**Chat Serialization:**
- [ ] Define `agent-chat-file` variable (~/.agent/agent-chat.org)
- [ ] Implement `agent-chat-append-exchange` writes tick heading + pair
- [ ] Include :TIMESTAMP: property
- [ ] Implement `agent-chat-load-history` parses org file
- [ ] Extract Human/Agent subheadings content
- [ ] Return list of `((human . "...") (agent . "...") (tick . N))`
- [ ] Handle empty/missing file gracefully
- [ ] Call load on init, populate in-memory store

**Scratchpad Serialization:**
- [ ] Update `agent-scratchpad-append-heading` to add properties
- [ ] Add :THREAD: property (thread id or "null")
- [ ] Add :TICK: property
- [ ] Implement `agent-scratchpad-load` parses with properties
- [ ] Filter by thread property for context assembly
- [ ] Handle JSON `scratchpad` field from agent response
- [ ] Create heading if doesn't exist, append if does

**Integration:**
- [ ] After successful inference, call serialization
- [ ] On warm start, load both files
- [ ] Test: write exchange, restart, exchange appears in context
- [ ] Test: write scratchpad note, restart, note appears
- [ ] Test: thread-scoped notes filter correctly
- [ ] Byte-compile without warnings

### Acceptance Criteria

**Scenario:** Chat persistence
**GIVEN** Human has 5 exchanges with agent
**WHEN** Emacs is restarted and shell opened
**THEN** Prior 5 exchanges appear in context
**AND** agent-chat.org contains all exchanges with tick headings

**Scenario:** Scratchpad persistence
**GIVEN** Agent wrote notes under "Parser Notes" heading
**WHEN** Emacs is restarted
**THEN** Notes appear in scratchpad section of context
**AND** scratchpad.org contains heading with properties

**Scenario:** Thread-scoped notes
**GIVEN** Agent wrote notes with thread: "rust-debugging"
**WHEN** Different thread is active
**THEN** Those notes do NOT appear in context
**WHEN** rust-debugging thread is active
**THEN** Those notes DO appear in context

### Issues Encountered

<!-- Fill during implementation -->
