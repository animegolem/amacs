---
node_id: AI-EPIC-006
tags:
  - EPIC
  - AI
  - hub
  - dashboard
  - UX
date_created: 2025-01-06
kanban-status: completed
date_completed: 2025-01-06
AI_IMP_spawned:
  - AI-IMP-043
  - AI-IMP-044
  - AI-IMP-045
  - AI-IMP-046
  - AI-IMP-047
---

# AI-EPIC-006: Human Interface

## Problem Statement/Feature Scope

The v4 shell (EPIC-005) provides a functional comint interface, but lacks the rich dashboard view that amacs-hub.el provided in v3. The existing hub code depends on v3's buffer-based chat and scratchpad systems which are incompatible with v4's org-file persistence.

The hub needs to be adapted to read from v4's data sources while maintaining its magit-section-based UX.

## Proposed Solution(s)

Update amacs-hub.el to work with v4 architecture:

1. **Data Sources**: Read from org files instead of buffers
   - Chat: Parse `~/.agent/agent-chat.org`
   - Scratchpad: Parse `~/.agent/scratchpad.org`
   - Monologue: Parse `~/.agent/monologue.org`
   - Status/Threads: Read from amacs-shell state variables

2. **Shell Integration**:
   - RET on Chat section header opens/focuses `*amacs-shell*`
   - Auto-refresh after tick completion
   - Shell remains primary I/O, hub is the observer/navigator

3. **Navigation Pattern**:
   - TAB: Expand section to show entries
   - TAB (on entry): Expand to show full content
   - RET: Context-appropriate action (open shell, switch thread, etc.)

## Path(s) Not Taken

- **Hub as primary interface**: Too much complexity; shell is simpler for chat
- **Embedded shell in hub**: Would require significant magit-section hacking
- **Buffer-based data**: v4 uses org files; don't revert to buffer approach

## Success Metrics

1. **Hub renders**: Opens without errors, shows all sections
2. **Data accurate**: Status, threads, chat match shell state
3. **Navigation works**: TAB expands, RET performs actions
4. **Auto-refresh**: Hub updates after tick without manual `g`
5. **Shell accessible**: RET on chat opens shell buffer

**Definition of Done**: User can monitor agent state via hub while interacting through shell. Hub accurately reflects all persisted state.

## Requirements

### Functional Requirements

- [ ] FR-1: Hub reads status from shell state variables
- [ ] FR-2: Hub reads threads from shell thread storage
- [ ] FR-3: Hub parses chat history from agent-chat.org
- [ ] FR-4: Hub parses scratchpad from scratchpad.org
- [ ] FR-5: Hub parses monologue from monologue.org
- [ ] FR-6: TAB expands chat section to show tick list
- [ ] FR-7: TAB on tick expands to show human/agent exchange
- [ ] FR-8: RET on Chat header opens *amacs-shell* buffer
- [ ] FR-9: Auto-refresh triggers after tick completion
- [ ] FR-10: Thread actions (switch, complete) work with v4 state
- [ ] FR-11: All existing keybindings preserved where applicable

### Non-Functional Requirements

- NFR-1: Byte-compiles without warnings
- NFR-2: Works when shell not yet started (graceful nil handling)
- NFR-3: Refresh is fast (<100ms for typical state)

## Implementation Breakdown

| IMP | Title | Status | Description |
|-----|-------|--------|-------------|
| AI-IMP-043 | Hub State Bridge | **complete** | Connect hub to shell state variables |
| AI-IMP-044 | Org File Parsers | **complete** | Parse chat/scratchpad/monologue org files |
| AI-IMP-045 | Section Rendering | **complete** | Update sections to use new data sources |
| AI-IMP-046 | Shell Integration | **complete** | RET opens shell, auto-refresh hook |
| AI-IMP-047 | Action Handlers | **complete** | Thread/buffer/skill actions with v4 state |

**All 5/5 IMPs complete. EPIC-006 done.**

## Design Notes

### State Bridge

The hub currently calls functions like `agent-current-tick`, `agent-mood`, etc. from agent-consciousness.el. For v4, these should read from:
- `amacs-shell--current-tick`
- `amacs-shell--last-response` (for mood, confidence)
- `amacs-shell--open-threads`, `amacs-shell--active-thread`

Options:
1. **Wrapper functions**: Add `amacs-hub-*` functions that try shell state, fall back to consciousness
2. **Direct access**: Modify hub to read shell variables directly
3. **Shared state**: Extract state to separate module both can use

Recommendation: Wrapper functions for clean separation.

### Org File Parsing

The v4 org file formats:

**agent-chat.org**:
```org
* Tick 1
:PROPERTIES:
:TIMESTAMP: 2025-01-06T10:00:00Z
:END:

** Human
What's the weather?

** Agent
I don't have weather data access.
```

**scratchpad.org**:
```org
* Working Notes
:PROPERTIES:
:THREAD: null
:TICK: 1
:END:
Some notes here...

* Task-specific notes
:PROPERTIES:
:THREAD: my-task
:TICK: 2
:END:
Notes for task...
```

Hub should reuse `agent-persistence.el` parsing functions where possible.

### Auto-refresh

Add hook in `amacs-shell--handle-response` that calls `amacs-hub-refresh` if hub buffer exists.
