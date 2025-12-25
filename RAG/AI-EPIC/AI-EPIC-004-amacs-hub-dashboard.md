---
node_id: AI-EPIC-004
tags: 
  - EPIC
  - AI
  - hub
  - dashboard
  - magit-section
  - observability
date_created: 2025-12-25
date_completed: 
kanban-status: planned
AI_IMP_spawned: 
  - AI-IMP-029
  - AI-IMP-030
  - AI-IMP-031
  - AI-IMP-032
  - AI-IMP-033
---

# AI-EPIC-004: AMACS Hub Dashboard

## Problem Statement/Feature Scope

Currently there is no unified view of agent state. Human must check multiple buffers (chat, monologue, scratchpad) and evaluate elisp to inspect consciousness. Agent has no "body awareness" interface - no way to see its own state at a glance or take meta-actions.

Field testing revealed:
- Human couldn't see agent working except via monologue (requires active attention)
- Agent state scattered across files and variables
- No way for human to quickly attach buffers or switch context
- No visibility into API settings or ability to modify inference parameters

The hub serves two audiences:
1. **Human:** Observability dashboard, quick actions, buffer management
2. **Agent:** Self-awareness interface, API parameter control, state inspection

## Proposed Solution(s)

Build `amacs-hub` - a magit-section based dashboard providing:

**Status Bar:** API parameters (temperature, thinking, model) - viewable and modifiable by both human and agent

**Threads Section:** Active/inactive threads with actions (switch, create, complete, archive)

**Watched Buffers Section:** Current thread's buffers with add/remove capability (like @file in Claude Code)

**Active Skills Section:** Thread-bound skills with bind/unbind

**Chat Section:** Navigation-only - tick numbers grouped by date, RET jumps to tick in chat buffer

**Monologue Section:** Recent entries, RET jumps to line in monologue.org

**Scratchpad Section:** Headings from scratchpad buffers, RET jumps to heading

Additionally, a **chat status line** showing current tick and in-progress monologue as ephemeral footer.

Uses `magit-section` for:
- Granular refresh (update sections independently)
- Built-in collapse/expand
- Consistent keybinding patterns
- Read-only display with action affordances

## Path(s) Not Taken

- **Org-mode hub:** Considered but org conflates display with editing. Hub should be read-only with explicit actions.
- **Full content in hub:** Chat/monologue content stays in source buffers. Hub is navigation, not duplication.
- **Automatic buffer attachment:** Human explicitly adds buffers. Agent can request via chat but human confirms.

## Success Metrics

1. Human can see full agent state in single buffer
2. Human can switch threads, add buffers, trigger ticks without elisp
3. Agent can read and modify API parameters
4. RET navigation works throughout hub
5. Hub refresh is fast (<100ms) and doesn't lose cursor position
6. Chat status line shows real-time tick progress

**Validation:** Use hub as primary interface for 50+ tick session.

## Requirements

### Functional Requirements

- [ ] FR-1: Hub buffer uses magit-section-mode derivative
- [ ] FR-2: Status bar shows API settings (temp, top_p, thinking, model)
- [ ] FR-3: API settings modifiable via hub keybindings
- [ ] FR-4: API settings modifiable via agent elisp
- [ ] FR-5: Threads section shows active thread highlighted
- [ ] FR-6: Threads section shows inactive threads collapsed
- [ ] FR-7: RET on thread switches active thread
- [ ] FR-8: Watched buffers section shows current thread's buffers
- [ ] FR-9: `a` on buffers section prompts to add buffer
- [ ] FR-10: `k` on buffer removes from thread
- [ ] FR-11: RET on buffer jumps to buffer
- [ ] FR-12: Skills section shows thread-bound skills
- [ ] FR-13: Chat section shows ticks grouped by date (no content)
- [ ] FR-14: RET on tick jumps to tick heading in chat buffer
- [ ] FR-15: Monologue section shows recent N entries
- [ ] FR-16: RET on monologue entry jumps to line in file
- [ ] FR-17: Scratchpad section shows headings from scratchpad buffers
- [ ] FR-18: `g` refreshes hub
- [ ] FR-19: TAB toggles section collapse
- [ ] FR-20: Chat buffer has status line showing current tick + ephemeral monologue

### Non-Functional Requirements

- NFR-1: Hub refresh completes in <100ms
- NFR-2: Cursor position preserved across refresh
- NFR-3: magit-section is only new dependency
- NFR-4: Hub works in terminal Emacs (no GUI-only features)
- NFR-5: Keybindings documented in `?` help

## Implementation Breakdown

| IMP | Title | Status | Dependencies |
|-----|-------|--------|--------------|
| AI-IMP-029 | Hub Skeleton | planned | EPIC-003 |
| AI-IMP-030 | Hub Navigation | planned | IMP-029 |
| AI-IMP-031 | Hub Actions | planned | IMP-030 |
| AI-IMP-032 | API Settings Section | planned | IMP-029 |
| AI-IMP-033 | Chat Status Line | planned | EPIC-003 |

**Dependency Graph:**
```
EPIC-003 (Field Test Remediation)
    ↓
IMP-029 (Hub Skeleton)
    ↓
IMP-030 (Navigation) ──→ IMP-031 (Actions)
    ↓
IMP-032 (API Settings)

EPIC-003 ──→ IMP-033 (Chat Status Line) [parallel]
```
