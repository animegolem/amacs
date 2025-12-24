---
node_id: AI-IMP-025
tags:
  - IMP-LIST
  - Implementation
  - data-structures
  - alist
  - migration
kanban_status: planned
depends_on: []
confidence_score: 0.85
created_date: 2025-12-24
close_date:
--- 

# AI-IMP-025: Alist Migration

## Summary

Migrate consciousness and thread data structures from plists to alists throughout the codebase.

**Current state:** Mixed plist usage with `plist-get`/`plist-put` and occasional mutation issues.

**Target state:** Consistent alist usage with symbol keys, `alist-get` accessors, and `setf` for updates.

**Why:** 
- `alist-get` supports default values natively
- `setf` works cleanly with `alist-get`
- Direct mapping to JSON without key translation
- Consistent convention across codebase

**Done when:** All consciousness and thread operations use alist patterns, tests pass, existing state files migrate cleanly.

### Out of Scope

- API response parsing (already returns plists from json-parse, can stay)
- Temporary local plists in functions (not worth changing)
- External skill file formats

### Design/Approach

**Accessor changes:**
```elisp
;; Before
(plist-get agent-consciousness :mood)
(setq agent-consciousness (plist-put agent-consciousness :mood "focused"))

;; After  
(alist-get 'mood agent-consciousness)
(setf (alist-get 'mood agent-consciousness) "focused")
```

**Schema change:**
```elisp
;; Before
'(:identity "amacs" :mood :awakening :confidence 0.5 ...)

;; After
'((identity . "amacs") (mood . "awakening") (confidence . 0.5) ...)
```

**Migration:** Add `agent--migrate-plist-to-alist` for loading old consciousness files.

### Files to Touch

- `harness/agent-consciousness.el`: Schema, accessors, persistence, migration
- `harness/agent-threads.el`: Thread creation, accessors, all plist-get/put calls
- `harness/agent-context.el`: Thread summary, buffer hydration
- `harness/agent-inference.el`: Response processing, state updates
- `harness/agent-monologue.el`: Consciousness access
- `harness/agent-tick.el`: State access
- `harness/agent-chat.el`: Pending chat access
- `harness/agent-skills.el`: Thread skill binding
- `harness/test-harness.el`: Update all test assertions

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**? 
</CRITICAL_RULE> 

- [ ] Create `agent--migrate-plist-to-alist` function for backward compat
- [ ] Update `agent--default-consciousness` to return alist
- [ ] Update `agent-get` to use `alist-get` with symbol keys
- [ ] Update `agent-set` to use `setf` with `alist-get`
- [ ] Update `agent-persist-consciousness` for alist format
- [ ] Update `agent-load-consciousness` with migration detection
- [ ] Update `agent-create-thread` to return alist
- [ ] Update `agent-thread-summary` for alist input
- [ ] Update all `plist-get` calls on threads to `alist-get`
- [ ] Update `agent-get-active-thread` and related accessors
- [ ] Update context assembly for alist consciousness
- [ ] Update inference response processing
- [ ] Remove `agent--plist-to-json-alist` (no longer needed)
- [ ] Update test-harness.el assertions
- [ ] Test cold start (fresh consciousness)
- [ ] Test warm start (migrate existing plist file)
- [ ] Document alist convention in style guide

### Acceptance Criteria

**Scenario:** Fresh initialization creates alist consciousness
**GIVEN** no existing `~/.agent/consciousness.el` file
**WHEN** `agent-init` is called
**THEN** `agent-consciousness` is an alist with symbol keys
**AND** `(alist-get 'identity agent-consciousness)` returns the identity string

**Scenario:** Existing plist consciousness migrates on load
**GIVEN** existing `~/.agent/consciousness.el` with plist format
**WHEN** `agent-init` is called  
**THEN** consciousness is converted to alist format
**AND** all values are preserved
**AND** `agent-get` works correctly

**Scenario:** Thread creation uses alist
**GIVEN** agent is initialized
**WHEN** `(agent-create-thread "test concern")` is called
**THEN** returned thread is an alist
**AND** `(alist-get 'concern thread)` returns "test concern"

### Issues Encountered

<!-- Fill during implementation -->
