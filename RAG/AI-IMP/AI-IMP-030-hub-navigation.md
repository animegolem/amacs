---
node_id: AI-IMP-030
tags:
  - IMP-LIST
  - Implementation
  - hub
  - navigation
kanban_status: complete
depends_on: 
  - AI-IMP-029
confidence_score: 0.9
created_date: 2025-12-25
close_date: 2025-12-30
--- 

# AI-IMP-030: Hub Navigation

## Summary

Add RET-to-jump navigation throughout the hub, allowing users to navigate from hub items to their source locations for editing or full context.

**Current state:** Hub displays information with TAB inline expansion (IMP-029), but no way to jump to source.

**Target state:** RET on any navigable item jumps to the source buffer/file at the appropriate location.

**Why:**
- TAB shows content inline (review without leaving hub)
- RET jumps to source (edit, see full context)
- Dual pattern matches magit UX users expect

**Done when:** RET works on threads, buffers, skills, chat ticks, monologue entries, and scratchpad headings.

### Out of Scope

- Modifying actions (add/remove) - IMP-031
- API settings - IMP-032
- Mouse support (keyboard-first)

### Design/Approach

**Dual interaction pattern:**
| Key | Action | Use case |
|-----|--------|----------|
| TAB | Expand/collapse inline | Review content without leaving hub |
| RET | Jump to source | Edit, see full context, work in source buffer |

**Navigation targets:**

| Section | RET Action |
|---------|------------|
| Thread | Switch to thread (update consciousness) |
| Buffer | `switch-to-buffer-other-window` |
| Skill | `find-file-other-window` on SKILL.md |
| Chat tick | Jump to `* Tick N` heading in chat buffer |
| Monologue entry | Jump to tick entry in monologue.org |
| Scratchpad heading | Jump to heading in scratchpad buffer |

**Implementation pattern:**
```elisp
(defun amacs-hub-visit-thing-at-point ()
  "Visit the item at point in other window."
  (interactive)
  (let ((section (magit-current-section)))
    (pcase (oref section type)
      ('amacs-thread (amacs-hub--visit-thread section))
      ('amacs-buffer (amacs-hub--visit-buffer section))
      ('amacs-skill (amacs-hub--visit-skill section))
      ('amacs-chat-tick (amacs-hub--visit-chat-tick section))
      ('amacs-monologue-tick (amacs-hub--visit-monologue section))
      ('amacs-scratchpad-heading (amacs-hub--visit-scratchpad section)))))
```

**Section value storage:**
Each section stores its navigation target in `(oref section value)`:
- Thread: thread alist
- Buffer: buffer name string
- Skill: skill directory path
- Chat tick: `(buffer . tick-number)`
- Monologue: `(file . position)`
- Scratchpad: `(buffer . heading-position)`

**Window management:**
- Hub remains visible in current window
- Target opens in `other-window`
- If only one window, split first

### Files to Touch

- `harness/amacs-hub.el`: Add navigation functions, keybindings
- `harness/agent-threads.el`: Ensure `agent-switch-thread` works from hub

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**? 
</CRITICAL_RULE> 

- [ ] Ensure all section insertions store navigation value in section
- [ ] Implement `amacs-hub-visit-thing-at-point` dispatcher
- [ ] Implement `amacs-hub--visit-thread` (calls agent-switch-thread, refreshes hub)
- [ ] Implement `amacs-hub--visit-buffer` (switch-to-buffer-other-window)
- [ ] Implement `amacs-hub--visit-skill` (find-file-other-window on SKILL.md)
- [ ] Implement `amacs-hub--visit-chat-tick` (open chat, goto tick heading)
- [ ] Implement `amacs-hub--visit-monologue` (open monologue.org, goto position)
- [ ] Implement `amacs-hub--visit-scratchpad` (open scratchpad, goto heading)
- [ ] Implement `amacs-hub--ensure-other-window` for window management
- [ ] Bind RET to `amacs-hub-visit-thing-at-point`
- [ ] Test thread navigation updates consciousness and refreshes hub
- [ ] Test buffer navigation opens correct buffer in other window
- [ ] Test skill navigation opens SKILL.md in other window
- [ ] Test chat tick navigation finds heading
- [ ] Test monologue navigation goes to correct entry
- [ ] Test scratchpad navigation finds heading
- [ ] Handle missing targets gracefully (deleted buffer, etc.)

### Acceptance Criteria

**Scenario:** Navigate to thread
**GIVEN** hub open with multiple threads listed
**WHEN** user moves to inactive thread and presses RET
**THEN** thread becomes active
**AND** hub refreshes to show new active thread
**AND** message confirms switch

**Scenario:** Navigate to buffer
**GIVEN** hub open with watched buffers listed
**WHEN** user presses RET on buffer name
**THEN** buffer opens in other window
**AND** hub remains visible in original window

**Scenario:** Navigate to chat tick
**GIVEN** hub shows chat ticks including "Tick 43"
**WHEN** user presses RET on Tick 43
**THEN** chat buffer opens in other window
**AND** point is at `* Tick 43` heading

**Scenario:** Navigate to monologue entry
**GIVEN** hub shows monologue with Tick 43 entry
**WHEN** user presses RET on that entry
**THEN** monologue.org opens in other window
**AND** point is at that tick's entry

**Scenario:** Navigate to scratchpad heading
**GIVEN** hub shows scratchpad headings including "Goals"
**WHEN** user presses RET on "Goals"
**THEN** scratchpad buffer opens in other window
**AND** point is at the "Goals" org heading

**Scenario:** TAB and RET work together
**GIVEN** hub open with chat tick collapsed
**WHEN** user presses TAB to expand, reviews content
**AND** then presses RET
**THEN** chat buffer opens at that tick
**AND** user can edit the content

**Scenario:** Handle missing target
**GIVEN** hub shows buffer that has been killed
**WHEN** user presses RET on that buffer
**THEN** helpful message shown (not error)
**AND** hub suggests refresh

### Issues Encountered

<!-- Fill during implementation -->
