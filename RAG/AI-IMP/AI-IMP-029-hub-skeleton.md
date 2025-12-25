---
node_id: AI-IMP-029
tags:
  - IMP-LIST
  - Implementation
  - hub
  - magit-section
  - dashboard
kanban_status: planned
depends_on: 
  - AI-EPIC-003
confidence_score: 0.85
created_date: 2025-12-25
close_date:
--- 

# AI-IMP-029: Hub Skeleton

## Summary

Create the foundational `amacs-hub` buffer using magit-section for collapsible, refreshable dashboard display.

**Current state:** No unified view of agent state. Information scattered across buffers and variables.

**Target state:** `*amacs-hub*` buffer with magit-section structure showing threads, buffers, skills, chat, monologue, and scratchpad sections.

**Why:** 
- magit-section provides granular refresh, collapse/expand, consistent UX
- Foundation for navigation (IMP-030) and actions (IMP-031)
- Both human and agent can inspect full state

**Done when:** `M-x amacs-hub` opens dashboard with all sections populated, `g` refreshes, TAB collapses sections.

### Out of Scope

- Navigation (RET to jump) - IMP-030
- Actions (add/remove/switch) - IMP-031
- API settings section - IMP-032
- Chat status line - IMP-033

### Design/Approach

**Section hierarchy:**
```
amacs-hub-section (root)
├── amacs-status-section (mood, tick, confidence)
├── amacs-threads-section
│   ├── amacs-thread-section (active)
│   └── amacs-thread-section (inactive, collapsed)
├── amacs-buffers-section
│   └── amacs-buffer-section (per buffer)
├── amacs-skills-section
│   └── amacs-skill-section (per skill)
├── amacs-chat-section
│   └── amacs-chat-date-section
│       └── amacs-chat-tick-section
├── amacs-monologue-section
│   └── amacs-monologue-entry-section
└── amacs-scratchpad-section
    └── amacs-scratchpad-heading-section
```

**Mode definition:**
```elisp
(define-derived-mode amacs-hub-mode magit-section-mode "AMACS-Hub"
  "Major mode for AMACS hub dashboard."
  :group 'amacs
  (setq-local revert-buffer-function
              (lambda (_ignore-auto _noconfirm)
                (amacs-hub-refresh))))
```

**Data sources:**
- Threads: `agent-consciousness` alist
- Buffers: active thread's buffer list
- Skills: active thread's bound-skills
- Chat: parse chat buffer for tick headings
- Monologue: read recent lines from monologue.org
- Scratchpad: parse scratchpad buffer headings

### Files to Touch

- `harness/amacs-hub.el`: New file - mode, sections, refresh
- `harness/agent-core.el`: Add require, autoload for amacs-hub
- `harness/agent-consciousness.el`: Ensure accessors work with hub
- `README.md`: Document hub usage

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**? 
</CRITICAL_RULE> 

- [ ] Add magit-section as dependency (document in README)
- [ ] Create `harness/amacs-hub.el` with header/requires
- [ ] Define `amacs-hub-mode` derived from magit-section-mode
- [ ] Define section classes for each section type
- [ ] Implement `amacs-hub` command to open/switch to hub
- [ ] Implement `amacs-hub-refresh` to rebuild all sections
- [ ] Implement `amacs-hub--insert-status` (mood, tick, confidence)
- [ ] Implement `amacs-hub--insert-threads` with active/inactive grouping
- [ ] Implement `amacs-hub--insert-buffers` for current thread
- [ ] Implement `amacs-hub--insert-skills` for bound skills
- [ ] Implement `amacs-hub--insert-chat` with date grouping
- [ ] Implement `amacs-hub--insert-monologue` with recent entries
- [ ] Implement `amacs-hub--insert-scratchpad` with headings
- [ ] Verify TAB toggles section collapse (built-in)
- [ ] Verify `g` refreshes (via revert-buffer)
- [ ] Add autoload and require in agent-core
- [ ] Test with populated consciousness state
- [ ] Test with empty/minimal state

### Acceptance Criteria

**Scenario:** Hub displays current state
**GIVEN** agent is initialized with threads, buffers, skills
**WHEN** `M-x amacs-hub` is executed
**THEN** hub buffer opens with all sections populated
**AND** active thread is visually distinguished
**AND** inactive threads are collapsed by default

**Scenario:** Hub refreshes on `g`
**GIVEN** hub is open
**WHEN** consciousness state changes and user presses `g`
**THEN** hub reflects new state
**AND** cursor position is preserved if possible

**Scenario:** Sections collapse on TAB
**GIVEN** hub is open with cursor on section heading
**WHEN** user presses TAB
**THEN** section collapses/expands
**AND** child sections are hidden/shown

**Scenario:** Hub works with empty state
**GIVEN** fresh agent initialization (no threads, empty monologue)
**WHEN** `M-x amacs-hub` is executed
**THEN** hub opens without errors
**AND** sections show appropriate empty state messages

### Issues Encountered

<!-- Fill during implementation -->
