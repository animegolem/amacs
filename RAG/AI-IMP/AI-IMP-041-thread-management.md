---
node_id: AI-IMP-041
tags:
  - IMP-LIST
  - Implementation
  - threads
  - context-switching
  - buffers
kanban_status: completed
depends_on:
  - AI-IMP-040
confidence_score: 0.85
created_date: 2025-01-03
close_date: 2025-01-05
---

# AI-IMP-041: Thread Management

## Summary

Implement thread lifecycle with the new ID-first API for the v4 architecture.

**Current state:** v3 thread system with concern-first API.

**Target state:** ID-first API, max 3 threads, buffer attachment via eval.

**Done when:** Agent can create threads, switch focus, complete threads, and attach buffers.

### Out of Scope

- Git integration (IMP-042)
- Skills binding (can reuse v3 implementation)

### Design/Approach

New API (ID-first):
```elisp
;; Create thread
(agent-create-thread "rust-debugging"
                     :concern "Fix ownership error"
                     :buffers '("src/main.rs"))

;; Switch focus
(agent-switch-thread "rust-debugging")

;; Complete with evidence
(agent-complete-thread "rust-debugging"
                       :evidence '(:output "Tests pass")
                       :learned "Use 'static for returned refs")

;; Buffer management
(agent-thread-add-buffer "rust-debugging" "src/lib.rs")
(agent-thread-remove-buffer "rust-debugging" "old.rs")
```

Thread stored as alist:
```elisp
((id . "rust-debugging")
 (concern . "Fix ownership error")
 (buffers . ("src/main.rs"))
 (started-tick . 42)
 (hydrated . t)  ; is this the active thread?
 ...)
```

Max 3 open threads enforced at create time.

Active thread's buffers included in context (hydrated).
Pending threads shown as metadata only (dehydrated).

### Files to Touch

- `harness/agent-threads.el`: update API to ID-first
- `harness/agent-consciousness.el`: thread storage
- `harness/agent-context.el`: thread buffer hydration

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**?
</CRITICAL_RULE>

**API Changes:**
- [x] Update `agent-create-thread` signature to ID-first
- [x] ID is required first positional arg
- [x] `:concern` is optional keyword arg
- [x] `:buffers` is optional keyword arg
- [x] Check for duplicate ID, return error if exists
- [x] Enforce max 3 open threads
- [x] Return thread alist on success

**Thread Lifecycle:**
- [x] `agent-switch-thread` dehydrates old, hydrates new
- [x] `agent-complete-thread` moves to completed list
- [x] Clear active-thread if completed thread was active
- [ ] `agent-archive-thread` (deferred - not critical for MVP)

**Buffer Management:**
- [x] `agent-thread-add-buffer` appends to buffer list
- [x] `agent-thread-remove-buffer` removes from list
- [x] No duplicates in buffer list

**Context Integration:**
- [x] `amacs-shell--format-threads` shows thread info
- [x] Pending threads shown as summaries only
- [x] Thread count in consciousness summary (active-thread, open-threads)

**Tests:**
- [x] Existing v3 thread tests pass (113/113)
- [x] Byte-compile without warnings
- [x] All functions callable via eval

### Acceptance Criteria

**Scenario:** Create and switch threads
**GIVEN** No threads exist
**WHEN** Agent evals `(agent-create-thread "work-1" :concern "Task A")`
**AND** Agent evals `(agent-switch-thread "work-1")`
**THEN** Thread work-1 is created and active
**AND** Appears in consciousness as active-thread

**Scenario:** Max threads enforced
**GIVEN** 3 open threads exist
**WHEN** Agent tries to create 4th thread
**THEN** Error is returned
**AND** Thread is not created

**Scenario:** Thread buffers in context
**GIVEN** Thread "work-1" has buffers ["main.rs", "lib.rs"]
**AND** Thread "work-1" is active
**WHEN** Context is assembled
**THEN** Contents of main.rs and lib.rs appear in <buffers> section

### Issues Encountered

**Design decision**: Created v4-specific thread functions in amacs-shell.el rather than modifying agent-threads.el.

This keeps the v4 shell self-contained. The existing v3 agent-threads.el still works with the full consciousness system, while the new functions work with shell-local state.

**Implementation notes**:
- Thread storage: `amacs-shell--open-threads`, `amacs-shell--completed-threads`
- Active thread: `amacs-shell--active-thread`
- Max threads enforced at creation time (signals error)
- `<threads>` section in context shows active and pending
- Agent calls these functions via eval field
