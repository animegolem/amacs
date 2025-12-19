---
node_id: AI-IMP-016
tags:
  - IMP
  - bootstrap-skill
  - consciousness
  - schema
status: completed
depends_on:
  - AI-ADR-001
implements: AI-EPIC-001c
created_date: 2025-12-18
related_files:
  - skills/amacs-bootstrap-skill/core/references/consciousness-schema.md
  - harness/agent-consciousness.el
confidence_score: 0.85
---

# AI-IMP-016: consciousness-schema.md Review

## Objective

Review and update consciousness-schema.md to align with the current harness implementation, particularly the thread-centric context changes from ADR-001.

## Review Findings

The current schema is **mostly good** but needs these updates:

### 1. Add `:global-buffers` Field

**Missing from current schema.** Per ADR-001, global buffers are now separate from thread-specific buffers.

**Add to "Context Management" section:**
```elisp
;; Context management
:global-buffers ("*agent-chat*")     ; Always-visible buffers (human interface)
:watching-buffers nil                 ; DEPRECATED - buffers now per-thread
```

**Add to Field Reference table:**
| Field | Type | Description |
|-------|------|-------------|
| `:global-buffers` | list | Buffers always included regardless of active thread |

### 2. Clarify Thread Buffer Ownership

**Update thread structure documentation:**

Current:
```elisp
(:id "rust-debugging"
 ...
 :buffers ("src/main.rs" "Cargo.toml")
 ...)
```

Update to clarify hydration:
```elisp
(:id "rust-debugging"
 :started-tick 142
 :priority 1
 :concern "Ownership error in main.rs"
 
 ;; Thread-owned context
 :buffers ("src/main.rs" "Cargo.toml")  ; Only hydrated when this thread is active
 :primary-mode rust-mode
 :skill-tags ("rust-mode")
 
 ;; Hydration state (managed by harness)
 :hydrated nil    ; t when active, nil when pending
 
 :approach "Trying lifetime annotations"
 :blocking t)
```

### 3. Update `:watching-buffers` Description

**Current (incorrect):**
```elisp
:watching-buffers ("src/main.rs" "Cargo.toml" "*agent-chat*")
```

**Updated (per ADR-001):**
Remove from top-level consciousness or mark deprecated. Buffer watching is now per-thread via `:buffers` field.

### 4. Add Thread Hydration Explanation

**Add new section after "Thread Structure":**

```markdown
### Thread Hydration

Threads have two states:
- **Hydrated** (`:hydrated t`): Full buffer contents included in context
- **Dehydrated** (`:hydrated nil`): Only metadata (concern, approach) in context

Only the active thread is hydrated. Pending threads are dehydrated to save tokens.

Global buffers (`:global-buffers`) are always hydrated regardless of which thread is active.
```

### 5. Minor Field Updates

| Field | Change |
|-------|--------|
| `:watching-buffers` | Mark deprecated, reference thread `:buffers` |
| `:focus` | Keep but note it follows active thread |

## Changes Summary

| Section | Change Type | Description |
|---------|-------------|-------------|
| Full Structure | Add field | `:global-buffers` at consciousness level |
| Full Structure | Deprecate | `:watching-buffers` (now per-thread) |
| Thread Structure | Add fields | `:primary-mode`, `:skill-tags`, `:hydrated` |
| Thread Structure | Clarify | Buffers are thread-owned |
| Field Reference | Add entry | `:global-buffers` |
| Field Reference | Update | `:watching-buffers` deprecated note |
| New Section | Add | "Thread Hydration" explanation |

## Implementation Notes

The harness implementation in `agent-consciousness.el` should be the source of truth. Review that file to ensure schema matches actual structure.

Key functions to verify:
- `agent-consciousness-create` - initial structure
- `agent-create-thread` - thread structure
- `agent-get-active-thread` - hydration handling

## Acceptance Criteria

Given the updated consciousness-schema.md
When compared to the harness implementation
Then:
- [x] All fields in harness are documented in schema
- [x] `:global-buffers` is present and explained
- [x] Thread hydration is documented
- [x] `:watching-buffers` is marked deprecated (removed, note added)
- [x] Thread structure includes `:hydrated` field

## Estimated Effort

15 minutes (review + targeted edits)
