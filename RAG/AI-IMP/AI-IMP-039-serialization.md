---
node_id: AI-IMP-039
tags:
  - IMP-LIST
  - Implementation
  - serialization
  - persistence
  - org-mode
kanban_status: completed
depends_on:
  - AI-IMP-038
confidence_score: 0.80
created_date: 2025-01-03
close_date: 2025-01-05
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

- `harness/agent-persistence.el`: NEW - all serialization functions
- `harness/amacs-shell.el`: call serialization after inference, load on init

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**?
</CRITICAL_RULE>

**Chat Serialization:**
- [x] Define `agent-chat-file` variable (~/.agent/agent-chat.org)
- [x] Implement `agent-chat-append-exchange` writes tick heading + pair
- [x] Include :TIMESTAMP: property
- [x] Implement `agent-chat-load-history` parses org file
- [x] Extract Human/Agent subheadings content
- [x] Return list of plists with :human :agent :tick
- [x] Handle empty/missing file gracefully
- [x] Call load on init, populate in-memory store

**Scratchpad Serialization:**
- [x] Implement `agent-scratchpad-append` to add properties
- [x] Add :THREAD: property (thread id or "null")
- [x] Add :TICK: property
- [x] Implement `agent-scratchpad-load` parses with properties
- [x] `agent-scratchpad-filter-by-thread` for context assembly
- [x] Handle JSON `scratchpad` field from agent response
- [x] Create heading if doesn't exist, append if does

**Integration:**
- [x] After successful inference, call serialization
- [x] On warm start, load both files (`amacs-shell--load-history`)
- [x] Scratchpad in context via `amacs-shell--format-scratchpad`
- [x] Thread-scoped notes filter correctly (filter-by-thread)
- [x] Byte-compile without warnings
- [x] All 113 tests pass

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

**Design decision**: Created new `agent-persistence.el` file rather than adding to existing files.
This keeps serialization concerns separate from shell I/O (amacs-shell.el).

**Implementation notes**:
- Chat history uses plist format (:human :agent :tick) consistent with in-memory store
- Scratchpad filter function supports both global (thread=nil) and thread-specific notes
- Depth limiting applied separately to global vs thread notes in context assembly
- `amacs-shell--load-history` reverses loaded list to match push-based storage order
- Warm start shows "Resuming from tick N" message to indicate loaded state
