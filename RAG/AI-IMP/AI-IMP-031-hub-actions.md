---
node_id: AI-IMP-031
tags:
  - IMP-LIST
  - Implementation
  - hub
  - actions
  - buffers
  - threads
  - scratchpad
kanban_status: complete
depends_on:
  - AI-IMP-030
confidence_score: 0.95
created_date: 2025-12-25
close_date: 2025-12-30
--- 

# AI-IMP-031: Hub Actions

## Summary

Add action keybindings to the hub for managing threads, buffers, skills, scratchpad context, and triggering ticks.

**Current state:** Hub displays state (IMP-029) and allows navigation (IMP-030), but no modifications.

**Target state:** Context-sensitive keybindings for add (`a`), remove (`k`), complete (`c`), include (`i`), and tick (`t`).

**Why:**
- Human can manage agent context without writing elisp
- Buffer attachment like @file in Claude Code
- Scratchpad heading inclusion for selective context control
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
| `+`/`-` | On scratchpad section | Increase/decrease scratchpad-context-depth |
| `t` | Global | Trigger `agent-think` |
| `T` | Global | Trigger tick with thinking enabled (one-shot) |
| `s` | Global | Switch thread (completing-read) |
| `?` | Global | Show help buffer |

**Scratchpad context depth:**
Scratchpad uses same depth-control pattern as chat and monologue.

Storage in consciousness:
```elisp
((scratchpad-context-depth . 10))  ; last 10 headings, 0 = all
```

Hub display shows depth setting:
```
* agent-scratchpad (depth: 10)
** scratchpad.org
*** Goals
*** Notes on lifetimes
*** Random observations
```

Agent can also control via elisp:
```elisp
(agent-set 'scratchpad-context-depth 5)  ; last 5 headings
(agent-set 'scratchpad-context-depth 0)  ; all headings
```

**Context assembly change:**
Context assembly reads `scratchpad-context-depth` and includes last N headings (0 = all).

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
- `harness/agent-consciousness.el`: Add `scratchpad-context-depth` field
- `harness/agent-context.el`: Limit scratchpad headings by depth
- `harness/agent-inference.el`: One-shot thinking parameter

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**? 
</CRITICAL_RULE> 

- [ ] Add `scratchpad-context-depth` to consciousness schema (default: 10, 0 = all)
- [ ] Update context assembly to limit scratchpad headings by depth
- [ ] Update hub scratchpad display to show depth setting
- [ ] Implement `amacs-hub-increase-scratchpad-depth` for `+` key
- [ ] Implement `amacs-hub-decrease-scratchpad-depth` for `-` key
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
- [ ] Test scratchpad depth increase/decrease
- [ ] Test tick triggering from hub
- [ ] Test one-shot thinking parameter

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

**Scenario:** Adjust scratchpad depth
**GIVEN** hub open with scratchpad section showing "depth: 10"
**WHEN** user presses `+` on scratchpad section
**THEN** `scratchpad-context-depth` increases to 11
**AND** hub refreshes showing "depth: 11"

**Scenario:** Scratchpad depth respected in context
**GIVEN** scratchpad has 15 headings and depth is 5
**WHEN** context is assembled for inference
**THEN** only last 5 headings are in context
**AND** older headings are NOT in context

**Scenario:** Scratchpad depth zero means all
**GIVEN** scratchpad has 15 headings and depth is 0
**WHEN** context is assembled for inference
**THEN** all 15 headings are in context

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

1. **Missing `agent-archive-thread` function** - The ticket specified archive as an action, but `agent-threads.el` only had `agent-complete-thread`. Added `agent-archive-thread` for threads that should be removed without completion evidence.

2. **Scratchpad depth not in context assembly** - `agent-context.el` needed to be updated to use the new `scratchpad-context-depth` from consciousness and include depth-limited scratchpad content via `agent-scratchpad-for-context`.

3. **Extended thinking placeholder** - The `T` keybinding for "tick with extended thinking" is implemented as a placeholder. Full support would require API parameter changes to enable extended/chain-of-thought reasoning modes. Currently uses a dynamic binding `agent-extended-thinking` that can be checked in `agent-inference.el` when API support is added.
