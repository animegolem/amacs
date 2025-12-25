---
node_id: AI-IMP-031
tags:
  - IMP-LIST
  - Implementation
  - hub
  - actions
  - buffers
  - threads
kanban_status: planned
depends_on: 
  - AI-IMP-030
confidence_score: 0.85
created_date: 2025-12-25
close_date:
--- 

# AI-IMP-031: Hub Actions

## Summary

Add action keybindings to the hub for managing threads, buffers, skills, and triggering ticks.

**Current state:** Hub displays state and allows navigation, but no modifications.

**Target state:** Context-sensitive keybindings for add (`a`), remove (`k`), complete (`c`), and tick (`t`).

**Why:**
- Human can manage agent context without writing elisp
- Buffer attachment like @file in Claude Code
- Quick thread management during collaborative work
- Trigger ticks from hub for testing/interaction

**Done when:** All action keybindings work, changes reflected in hub after refresh.

### Out of Scope

- API settings modification - IMP-032
- Undo/history for actions
- Bulk operations (multi-select)

### Design/Approach

**Keybinding map:**

| Key | Context | Action |
|-----|---------|--------|
| `a` | Threads section | Create new thread (prompt for concern) |
| `a` | Buffers section | Add buffer to thread (completing-read) |
| `a` | Skills section | Bind skill to thread (completing-read) |
| `k` | On thread | Archive thread |
| `k` | On buffer | Remove buffer from thread |
| `k` | On skill | Unbind skill from thread |
| `c` | On thread | Complete thread (prompt for evidence) |
| `t` | Global | Trigger `agent-think` |
| `T` | Global | Trigger tick with thinking enabled (one-shot) |
| `s` | Global | Switch thread (completing-read) |
| `?` | Global | Show help buffer |

**Context detection:**
```elisp
(defun amacs-hub-add ()
  "Add something based on current section context."
  (interactive)
  (let* ((section (magit-current-section))
         (parent (oref section parent))
         (type (or (oref section type)
                   (and parent (oref parent type)))))
    (pcase type
      ('amacs-threads (amacs-hub--add-thread))
      ('amacs-buffers (amacs-hub--add-buffer))
      ('amacs-skills (amacs-hub--add-skill))
      (_ (user-error "Nothing to add in this context")))))
```

**Refresh after action:**
All modifying actions call `(amacs-hub-refresh)` after completing.

### Files to Touch

- `harness/amacs-hub.el`: Action functions and keybindings
- `harness/agent-threads.el`: Ensure create/complete/archive work
- `harness/agent-skills.el`: Ensure bind/unbind work
- `harness/agent-inference.el`: One-shot thinking parameter

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**? 
</CRITICAL_RULE> 

- [ ] Implement `amacs-hub-add` dispatcher
- [ ] Implement `amacs-hub--add-thread` (read-string, agent-create-thread, agent-add-thread)
- [ ] Implement `amacs-hub--add-buffer` (completing-read from buffer-list)
- [ ] Implement `amacs-hub--add-skill` (completing-read from available skills)
- [ ] Implement `amacs-hub-remove` dispatcher
- [ ] Implement `amacs-hub--archive-thread`
- [ ] Implement `amacs-hub--remove-buffer`
- [ ] Implement `amacs-hub--unbind-skill`
- [ ] Implement `amacs-hub-complete-thread` (prompt for evidence/learned)
- [ ] Implement `amacs-hub-trigger-tick` (calls agent-think)
- [ ] Implement `amacs-hub-trigger-tick-with-thinking` (one-shot param)
- [ ] Implement `amacs-hub-switch-thread` (completing-read)
- [ ] Implement `amacs-hub-help` (describe keybindings)
- [ ] Bind all keys in amacs-hub-mode-map
- [ ] Refresh hub after each modifying action
- [ ] Test add thread flow
- [ ] Test add/remove buffer flow
- [ ] Test bind/unbind skill flow
- [ ] Test complete thread flow
- [ ] Test tick triggering from hub

### Acceptance Criteria

**Scenario:** Add buffer to thread
**GIVEN** hub open with active thread
**WHEN** user navigates to Buffers section and presses `a`
**THEN** completing-read prompts for buffer
**AND** selected buffer added to thread
**AND** hub refreshes showing new buffer

**Scenario:** Create new thread
**GIVEN** hub open
**WHEN** user navigates to Threads section and presses `a`
**THEN** prompt asks for concern
**AND** new thread created and becomes active
**AND** hub refreshes showing new thread

**Scenario:** Trigger tick from hub
**GIVEN** hub open, agent configured
**WHEN** user presses `t`
**THEN** `agent-think` executes
**AND** hub refreshes after tick completes
**AND** new tick visible in monologue section

**Scenario:** Complete thread with evidence
**GIVEN** hub open with cursor on active thread
**WHEN** user presses `c`
**THEN** prompt asks for completion evidence
**AND** prompt asks for learned insights
**AND** thread moved to completed
**AND** hub refreshes

### Issues Encountered

<!-- Fill during implementation -->
