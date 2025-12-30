---
node_id: AI-IMP-029
tags:
  - IMP-LIST
  - Implementation
  - hub
  - magit-section
  - dashboard
kanban_status: complete
depends_on: 
  - AI-EPIC-003
confidence_score: 0.85
created_date: 2025-12-25
close_date: 2025-12-30
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

**Done when:** `M-x amacs-hub` opens dashboard with all sections populated, `g` refreshes, TAB expands inline content (magit-style).

### Out of Scope

- Navigation to source (RET to jump) - IMP-030
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
│   └── amacs-chat-buffer-section
│       └── amacs-chat-date-section
│           └── amacs-chat-tick-section (expandable: human+agent content)
├── amacs-monologue-section
│   └── amacs-monologue-date-section
│       └── amacs-monologue-tick-section (expandable: narrative+eval+diff)
└── amacs-scratchpad-section
    └── amacs-scratchpad-buffer-section
        └── amacs-scratchpad-heading-section (expandable: heading content)
```

**Inline expansion (magit pattern):**
- TAB on collapsed section shows content inline (not in separate buffer)
- Chat tick: shows human prompt + agent response
- Monologue tick: shows narrative + eval + git diff
- Scratchpad heading: shows content under that org heading

**Monologue as event stream:**
Each tick entry stores:
```elisp
'((tick . 43)
  (timestamp . "2025-12-25T14:32:00Z")
  (mood . "focused")
  (narrative . "Investigating lifetime annotations")
  (eval . "(goto-char 1042)")
  (result . "success")
  (diff . "--- a/main.rs\n+++ b/main.rs\n@@ -42..."))
```

Git diff retrieved from commit associated with that tick.

**Scratchpad parsing:**
Parse org headings only (any level). Content is freeform.
```elisp
(defun amacs-hub--parse-scratchpad-headings (buffer)
  "Extract org headings from BUFFER for hub display."
  (with-current-buffer buffer
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (hl)
        `((title . ,(org-element-property :raw-value hl))
          (level . ,(org-element-property :level hl))
          (begin . ,(org-element-property :begin hl))
          (content-begin . ,(org-element-property :contents-begin hl))
          (content-end . ,(org-element-property :contents-end hl)))))))
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
- Chat: parse chat buffer for tick headings + content
- Monologue: parse monologue.org + retrieve git diffs per tick
- Scratchpad: parse org headings from scratchpad buffers

### Files to Touch

- `harness/amacs-hub.el`: New file - mode, sections, refresh
- `harness/agent-core.el`: Add require, autoload for amacs-hub
- `harness/agent-monologue.el`: Add function to retrieve tick+diff pairs
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
- [ ] Implement `amacs-hub--insert-chat` with date grouping and expandable ticks
- [ ] Implement `amacs-hub--parse-chat-ticks` to extract tick content
- [ ] Implement `amacs-hub--insert-monologue` with expandable tick+diff entries
- [ ] Implement `agent-monologue-get-tick-with-diff` to retrieve narrative+diff
- [ ] Implement `amacs-hub--insert-scratchpad` with expandable headings
- [ ] Implement `amacs-hub--parse-scratchpad-headings` for org heading extraction
- [ ] Store expandable content in section value for TAB display
- [ ] Verify TAB expands content inline (magit-section built-in)
- [ ] Verify `g` refreshes (via revert-buffer)
- [ ] Add autoload and require in agent-core
- [ ] Test with populated consciousness state
- [ ] Test with empty/minimal state
- [ ] Test TAB expansion on chat ticks
- [ ] Test TAB expansion on monologue entries
- [ ] Test TAB expansion on scratchpad headings

### Acceptance Criteria

**Scenario:** Hub displays current state
**GIVEN** agent is initialized with threads, buffers, skills
**WHEN** `M-x amacs-hub` is executed
**THEN** hub buffer opens with all sections populated
**AND** active thread is visually distinguished
**AND** inactive threads are collapsed by default

**Scenario:** TAB expands chat tick inline
**GIVEN** hub open with chat section showing "Tick 43"
**WHEN** user presses TAB on Tick 43
**THEN** human prompt and agent response shown inline below heading
**AND** pressing TAB again collapses the content

**Scenario:** TAB expands monologue with diff
**GIVEN** hub open with monologue section
**WHEN** user presses TAB on a tick entry
**THEN** narrative line shown
**AND** eval expression and result shown
**AND** git diff from that tick's commit shown inline

**Scenario:** TAB expands scratchpad heading
**GIVEN** hub open with scratchpad section showing headings
**WHEN** user presses TAB on "Goals" heading
**THEN** content under that org heading shown inline
**AND** freeform content preserved as-is

**Scenario:** Hub refreshes on `g`
**GIVEN** hub is open
**WHEN** consciousness state changes and user presses `g`
**THEN** hub reflects new state
**AND** cursor position is preserved if possible

**Scenario:** Hub works with empty state
**GIVEN** fresh agent initialization (no threads, empty monologue)
**WHEN** `M-x amacs-hub` is executed
**THEN** hub opens without errors
**AND** sections show appropriate empty state messages

### Issues Encountered

<!-- Fill during implementation -->
