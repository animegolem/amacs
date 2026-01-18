---
node_id: AI-IMP-052
tags:
  - IMP-LIST
  - Implementation
  - EPIC-007
  - buffers
  - context
kanban_status: backlog
depends_on:
  - AI-IMP-048
  - AI-IMP-049
confidence_score: 0.8
created_date: 2025-01-11
close_date:
---

# AI-IMP-052-buffer-hydration

## Summary

Threads track buffer names but buffer content is not injected into context. This IMP implements the `<buffers>` context section that includes actual buffer contents for the active thread's watched buffers.

**Current state**: Thread alist has `buffers` field with list of buffer names. Context assembly does not include buffer content.

**Target state**: Active thread's buffers are hydrated - their content appears in `<buffers>` section of context.

**Done when**: Add buffer to thread, that buffer's content appears in agent's context.

### Out of Scope

- Buffer diffing (showing only changes)
- Token budget management for large buffers (basic truncation ok)
- Auto-adding buffers based on mode/pattern

### Design/Approach

1. `agent-context.el` already has buffer hydration code from v3
2. Verify `agent--hydrate-buffer` reads buffer content
3. Add `<buffers>` section to context assembly
4. Format: `=== filename.ext (mode) ===\n{content}\n`
5. Only hydrate active thread's buffers (pending threads = metadata only)
6. Handle missing buffers gracefully (buffer killed, file not open)
7. Basic truncation for very large buffers (configurable limit)

Existing v3 code in agent-context.el:
```elisp
(defun agent--hydrate-buffer (buffer-name)
  "Get content of BUFFER-NAME for context inclusion."
  ...)
```

Need to ensure this is called during context assembly.

### Files to Touch

`harness/agent-context.el`: Implement/verify buffer hydration in context
`harness/agent-inference.el`: Ensure context includes buffers section
`harness/agent-consciousness.el`: May need buffer-content-limit setting

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**?
</CRITICAL_RULE>

- [ ] Review `agent-context.el` for existing hydration code
- [ ] Verify `agent--hydrate-buffer` works for open buffers
- [ ] Handle buffer-not-found case (return placeholder or skip)
- [ ] Add `agent--format-buffers-section` function
- [ ] Gets active thread from consciousness
- [ ] Gets buffer list from thread's `buffers` field
- [ ] Hydrates each buffer
- [ ] Formats as `=== name (mode) ===\n{content}`
- [ ] Add buffer content limit setting (default 10000 chars per buffer)
- [ ] Truncate with `[...truncated...]` marker
- [ ] Include `<buffers>` section in context assembly
- [ ] Only for active thread (pending threads don't hydrate)
- [ ] Test: add buffer to thread, verify content in context
- [ ] Test: buffer not open, verify graceful handling
- [ ] Run CI: `./harness/ci-check.sh`

### Acceptance Criteria

**Scenario:** Buffer content in context
**GIVEN** thread "my-project" has buffer "main.rs" in its buffer list
**AND** main.rs is open in Emacs
**WHEN** context is assembled for inference
**THEN** `<buffers>` section contains main.rs content
**AND** content is prefixed with `=== main.rs (rust-mode) ===`

**Scenario:** Buffer not open
**GIVEN** thread has buffer "deleted.txt" in its list
**AND** deleted.txt is not open in Emacs
**WHEN** context is assembled
**THEN** buffers section notes buffer unavailable
**AND** no error occurs

**Scenario:** Large buffer truncation
**GIVEN** thread has buffer with 50000 characters
**WHEN** context is assembled
**THEN** buffer content is truncated to limit
**AND** truncation marker appears

### Issues Encountered

<!-- This section filled during implementation -->
