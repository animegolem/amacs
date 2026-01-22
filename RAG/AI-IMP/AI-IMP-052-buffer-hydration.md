---
node_id: AI-IMP-052
tags:
  - IMP-LIST
  - Implementation
  - EPIC-007
  - buffers
  - context
kanban_status: done
depends_on:
  - AI-IMP-048
  - AI-IMP-049
confidence_score: 0.95
created_date: 2025-01-11
close_date: 2025-01-19
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

- [x] Review `agent-context.el` for existing hydration code (agent-hydrate-buffer:33-44, agent-hydrate-buffers:46-50)
- [x] Verify `agent--hydrate-buffer` works for open buffers (returns alist with content, mode, point)
- [x] Handle buffer-not-found case (return placeholder or skip) (returns nil, filtered by seq-filter)
- [x] Add `agent--format-buffers-section` function (inline in agent-inference.el:108-122)
- [x] Gets active thread from consciousness (agent-context.el:135)
- [x] Gets buffer list from thread's `buffers` field (agent-context.el:99)
- [x] Hydrates each buffer (agent-context.el:101)
- [x] Formats as `=== name (mode) ===\n{content}` (agent-inference.el:121)
- [x] Add buffer content limit setting (default 10000 chars per buffer) (agent-consciousness.el:97)
- [x] Truncate with `[...truncated...]` marker (agent-inference.el:118)
- [x] Include `<buffers>` section in context assembly (agent-inference.el:150-156)
- [x] Only for active thread (pending threads don't hydrate) (agent-context.el:115-117 uses thread-summary)
- [x] Test: add buffer to thread, verify content in context (test-buffer-hydration)
- [x] Test: buffer not open, verify graceful handling (missing-buffer-nil test)
- [x] Run CI: `./harness/ci-check.sh` - 119/119 tests pass

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

**Mostly pre-implemented**: Like IMP-050 and IMP-051, buffer hydration was ~90% complete from v3. The hydration functions existed (`agent-hydrate-buffer`, `agent-hydrate-buffers`), buffers were already being hydrated in context assembly, and the prompt formatter already had the correct `=== name (mode) ===` format.

**Added truncation**: The only functional change was adding buffer content truncation:
- Added `buffer-content-limit` to consciousness defaults (10000 chars)
- Modified `agent--format-buffer-for-prompt` to read the limit and truncate with `[...truncated...]` marker

**Test required thread switching**: Initial test failed because `agent-create-thread` doesn't automatically switch to the new thread. Fixed by calling `agent-add-thread` and `agent-switch-thread` after creation.

**Added 3 tests** (119 total):
- `buffer-content-hydrated`: Verify content appears in context
- `missing-buffer-nil`: Verify graceful handling of missing buffers
- `buffer-truncated`: Verify large buffer truncation works
