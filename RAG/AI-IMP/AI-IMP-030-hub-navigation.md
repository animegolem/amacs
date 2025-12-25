---
node_id: AI-IMP-030
tags:
  - IMP-LIST
  - Implementation
  - hub
  - navigation
kanban_status: planned
depends_on: 
  - AI-IMP-029
confidence_score: 0.9
created_date: 2025-12-25
close_date:
--- 

# AI-IMP-030: Hub Navigation

## Summary

Add RET-to-jump navigation throughout the hub, allowing users to quickly navigate from hub items to their source locations.

**Current state:** Hub displays information but items are not interactive.

**Target state:** RET on any navigable item jumps to the appropriate location (buffer, file line, org heading).

**Why:**
- Hub becomes navigation center, not just display
- Reduces context switching - see state, jump to details
- Matches magit UX patterns users expect

**Done when:** RET works on threads, buffers, skills, chat ticks, monologue entries, and scratchpad headings.

### Out of Scope

- Modifying actions (add/remove) - IMP-031
- API settings - IMP-032
- Mouse support (keyboard-first)

### Design/Approach

**Navigation targets:**

| Section | RET Action |
|---------|------------|
| Thread | Switch to thread (update consciousness) |
| Buffer | `switch-to-buffer-other-window` |
| Skill | `find-file-other-window` on SKILL.md |
| Chat tick | Jump to `* Tick N` heading in chat buffer |
| Monologue entry | Jump to line in monologue.org |
| Scratchpad heading | Jump to heading in scratchpad buffer |

**Implementation pattern:**
```elisp
(defun amacs-hub-visit-thing-at-point ()
  "Visit the item at point."
  (interactive)
  (let ((section (magit-current-section)))
    (pcase (oref section type)
      ('amacs-thread (amacs-hub--visit-thread section))
      ('amacs-buffer (amacs-hub--visit-buffer section))
      ('amacs-skill (amacs-hub--visit-skill section))
      ('amacs-chat-tick (amacs-hub--visit-chat-tick section))
      ('amacs-monologue-entry (amacs-hub--visit-monologue section))
      ('amacs-scratchpad-heading (amacs-hub--visit-scratchpad section)))))
```

**Section value storage:**
Each section stores its target in `(oref section value)`:
- Thread: thread alist
- Buffer: buffer name string
- Skill: skill directory path
- Chat tick: (buffer . tick-number)
- Monologue: (file . line-number)
- Scratchpad: (buffer . heading-position)

### Files to Touch

- `harness/amacs-hub.el`: Add navigation functions, keybindings
- `harness/agent-threads.el`: Ensure `agent-switch-thread` works from hub

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**? 
</CRITICAL_RULE> 

- [ ] Add `value` slot usage to all section insertions
- [ ] Implement `amacs-hub-visit-thing-at-point` dispatcher
- [ ] Implement `amacs-hub--visit-thread` (calls agent-switch-thread)
- [ ] Implement `amacs-hub--visit-buffer` (switch-to-buffer-other-window)
- [ ] Implement `amacs-hub--visit-skill` (find-file-other-window)
- [ ] Implement `amacs-hub--visit-chat-tick` (jump to org heading)
- [ ] Implement `amacs-hub--visit-monologue` (goto-line in file)
- [ ] Implement `amacs-hub--visit-scratchpad` (jump to heading)
- [ ] Bind RET to `amacs-hub-visit-thing-at-point`
- [ ] Test thread navigation updates consciousness
- [ ] Test buffer navigation opens correct buffer
- [ ] Test skill navigation opens SKILL.md
- [ ] Test chat tick navigation finds heading
- [ ] Test monologue navigation goes to correct line
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
**AND** hub remains visible

**Scenario:** Navigate to chat tick
**GIVEN** hub shows chat ticks including "Tick 43"
**WHEN** user presses RET on Tick 43
**THEN** chat buffer opens in other window
**AND** point is at `* Tick 43` heading

**Scenario:** Handle missing target
**GIVEN** hub shows buffer that has been killed
**WHEN** user presses RET on that buffer
**THEN** helpful message shown (not error)
**AND** hub suggests refresh

### Issues Encountered

<!-- Fill during implementation -->
