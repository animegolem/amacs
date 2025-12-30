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
date_completed: 2025-12-30
kanban-status: complete
AI_IMP_spawned:
  - AI-IMP-029
  - AI-IMP-030
  - AI-IMP-031
  - AI-IMP-032
  - AI-IMP-033
  - AI-IMP-034
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

**Chat Section:** Ticks grouped by date. TAB expands to show human+agent exchange inline. RET jumps to source.

**Monologue Section (Event Stream):** Each tick shows narrative line. TAB expands to show:
- Narrative (monologue line)
- Eval expression + result
- Git diff from that tick's commit

This makes monologue the true event-sourced view: thought + action in one place.

**Scratchpad Section:** Org headings from scratchpad buffers. TAB expands to show heading content. Uses depth control (`scratchpad-context-depth`) like chat/monologue - last N headings included in context (0 = all).

Additionally, a **chat status line** showing current tick and in-progress monologue as ephemeral header.

Uses `magit-section` for:
- Granular refresh (update sections independently)
- Built-in collapse/expand with inline content (TAB)
- Consistent keybinding patterns
- Read-only display with action affordances

### Key Interaction Pattern

| Key | Action |
|-----|--------|
| TAB | Expand/collapse inline (review without leaving hub) |
| RET | Jump to source (edit, full context) |

This matches magit's actual UX - you can review diffs inline, or jump to the file.

## Path(s) Not Taken

- **Org-mode hub:** Considered but org conflates display with editing. Hub should be read-only with explicit actions.
- **Full source buffer duplication:** Content shown inline via TAB, but editing happens in source buffers via RET.
- **Automatic buffer attachment:** Human explicitly adds buffers. Agent can autonomously watch buffers but human sees in hub.

## Success Metrics

1. Human can see full agent state in single buffer
2. Human can review chat exchanges without leaving hub (TAB)
3. Human can see narrative + code changes together in monologue (TAB)
4. Human can switch threads, add buffers, trigger ticks without elisp
5. Agent can read and modify API parameters
6. Agent can control which scratchpad headings feed into context
7. Hub refresh is fast (<100ms) and doesn't lose cursor position
8. Chat status line shows real-time tick progress

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
- [ ] FR-11: RET on buffer opens buffer in other window
- [ ] FR-12: Skills section shows thread-bound skills
- [ ] FR-13: Chat section shows ticks grouped by date
- [ ] FR-14: TAB on chat tick expands human+agent exchange inline
- [ ] FR-15: RET on chat tick jumps to tick heading in chat buffer
- [ ] FR-16: Monologue section shows recent tick entries
- [ ] FR-17: TAB on monologue tick expands narrative + eval + git diff inline
- [ ] FR-18: RET on monologue tick jumps to entry in monologue.org
- [ ] FR-19: Scratchpad section shows org headings from scratchpad buffers
- [ ] FR-20: TAB on scratchpad heading expands content inline
- [ ] FR-21: RET on scratchpad heading jumps to heading in buffer
- [ ] FR-22: Scratchpad context controlled by `scratchpad-context-depth` (last N headings, 0 = all)
- [ ] FR-23: Hub shows scratchpad depth setting and allows adjustment
- [ ] FR-24: Only last N scratchpad headings (per depth) feed into inference context
- [ ] FR-25: `g` refreshes hub
- [ ] FR-26: Chat buffer has status line showing current tick + ephemeral monologue

### Non-Functional Requirements

- NFR-1: Hub refresh completes in <100ms
- NFR-2: Cursor position preserved across refresh
- NFR-3: magit-section is only new dependency
- NFR-4: Hub works in terminal Emacs (no GUI-only features)
- NFR-5: Keybindings documented in `?` help
- NFR-6: Scratchpad requires only org headings (any level), content is freeform

## Implementation Breakdown

| IMP | Title | Status | Dependencies |
|-----|-------|--------|--------------|
| AI-IMP-029 | Hub Skeleton | **complete** | EPIC-003 |
| AI-IMP-030 | Hub Navigation | **complete** | IMP-029 |
| AI-IMP-031 | Hub Actions | **complete** | IMP-030 |
| AI-IMP-032 | API Settings Section | **complete** | IMP-029 |
| AI-IMP-033 | Chat Status Line | **complete** | EPIC-003 |
| AI-IMP-034 | Git Integration (Tick Commits) | **complete** | EPIC-003 |

**Dependency Graph:**
```
EPIC-003 (Field Test Remediation)
    ├──→ IMP-034 (Git Integration - tick commits)
    │        ↓
    ├──→ IMP-029 (Hub Skeleton + inline expansion) ←─ uses IMP-034 for monologue diffs
    │        ↓
    │    IMP-030 (Navigation - RET to source)
    │        ↓
    │    IMP-031 (Actions + scratchpad depth)
    │        ↓
    │    IMP-032 (API Settings)
    │
    └──→ IMP-033 (Chat Status Line) [parallel]
```

**Note:** IMP-029 can start without IMP-034 (show placeholder for diffs), but full monologue+diff requires git integration.
