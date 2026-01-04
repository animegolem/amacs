---
node_id: AI-IMP-041
tags:
  - IMP-LIST
  - Implementation
  - threads
  - context-switching
  - buffers
kanban_status: planned
depends_on:
  - AI-IMP-040
confidence_score: 0.85
created_date: 2025-01-03
close_date:
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
- [ ] Update `agent-create-thread` signature to ID-first
- [ ] ID is required first positional arg
- [ ] `:concern` is optional keyword arg
- [ ] `:buffers` is optional keyword arg
- [ ] Check for duplicate ID, return error if exists
- [ ] Enforce max 3 open threads
- [ ] Return thread alist on success

**Thread Lifecycle:**
- [ ] `agent-switch-thread` dehydrates old, hydrates new
- [ ] `agent-complete-thread` moves to completed list
- [ ] `agent-archive-thread` for abandoned threads
- [ ] Clear active-thread if completed thread was active

**Buffer Management:**
- [ ] `agent-thread-add-buffer` appends to buffer list
- [ ] `agent-thread-remove-buffer` removes from list
- [ ] No duplicates in buffer list

**Context Integration:**
- [ ] Active thread buffers hydrated in context
- [ ] Pending threads shown as summaries only
- [ ] Thread count in consciousness summary

**Tests:**
- [ ] Test: create thread with ID only
- [ ] Test: create thread with all options
- [ ] Test: duplicate ID returns error
- [ ] Test: max 3 threads enforced
- [ ] Test: switch thread changes active
- [ ] Test: complete thread moves to completed
- [ ] Test: buffer add/remove works
- [ ] Byte-compile without warnings

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

<!-- Fill during implementation -->
