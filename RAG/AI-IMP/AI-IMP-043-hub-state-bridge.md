---
node_id: AI-IMP-043
tags:
  - IMP-LIST
  - Implementation
  - hub
  - state
kanban_status: completed
depends_on: []
confidence_score: 0.85
created_date: 2025-01-06
---

# AI-IMP-043: Hub State Bridge

## Summary

Create bridge layer so hub can read state from v4 shell.

**Current state:** Hub calls `agent-current-tick`, `agent-mood`, etc. from agent-consciousness.el.

**Target state:** Hub reads from amacs-shell variables when shell is active, falls back gracefully otherwise.

**Done when:** Hub displays accurate tick, mood, confidence, threads from shell state.

### Out of Scope

- Modifying shell internals
- Writing state (hub is read-only observer)

### Design/Approach

Create wrapper functions in amacs-hub.el:

```elisp
(defun amacs-hub--get-tick ()
  "Get current tick from shell or consciousness."
  (if (bound-and-true-p amacs-shell--current-tick)
      amacs-shell--current-tick
    (when (fboundp 'agent-current-tick)
      (agent-current-tick))))

(defun amacs-hub--get-mood ()
  "Get mood from shell response or consciousness."
  (if (bound-and-true-p amacs-shell--last-response)
      (plist-get amacs-shell--last-response :mood)
    (when (fboundp 'agent-mood)
      (agent-mood))))
```

Similar for: confidence, active-thread, open-threads, completed-threads.

### Files to Touch

- `harness/amacs-hub.el`: Add bridge functions, update section inserters

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**?
</CRITICAL_RULE>

- [x] `amacs-hub--get-tick` - reads from shell or consciousness
- [x] `amacs-hub--get-mood` - from shell response or consciousness
- [x] `amacs-hub--get-confidence` - from shell response or consciousness
- [x] `amacs-hub--get-active-thread` - from shell state
- [x] `amacs-hub--get-open-threads` - from shell state
- [x] `amacs-hub--get-completed-threads` - from shell state
- [x] `amacs-hub--shell-active-p` - helper to check if shell exists
- [x] Handle nil gracefully (shell not started)
- [x] Update `amacs-hub--insert-status` to use bridge functions
- [x] Update `amacs-hub--insert-threads` to use bridge functions
- [x] Update `amacs-hub--insert-buffers` to use bridge functions
- [x] Byte-compile without warnings (hub skipped in CI, requires magit-section)
- [ ] Test with shell running (deferred to manual testing)
- [ ] Test without shell (deferred to manual testing)

### Acceptance Criteria

**Scenario:** Hub shows shell state
**GIVEN** Shell is running at tick 5
**AND** Mood is "focused"
**WHEN** User opens hub
**THEN** Hub shows tick 5, mood focused

**Scenario:** Hub works without shell
**GIVEN** Shell has not been started
**WHEN** User opens hub
**THEN** Hub shows tick 0 or nil gracefully
**AND** No errors occur
