---
node_id: AI-IMP-004
tags:
  - IMP-LIST
  - Implementation
  - phase-1
  - threads
  - context
kanban_status: in-progress
depends_on: 
  - AI-IMP-001
  - AI-IMP-002
confidence_score: 0.8
created_date: 2025-12-04
close_date: 
--- 

# AI-IMP-004-thread-centric-context

## Thread-Centric Context Management

Implement thread-centric context: threads own their buffers, skills bind per-thread, switching threads rehydrates context. Replaces global `:watching-buffers` with per-thread `:buffers` + global `:global-buffers`.

**Done when:** Creating a thread captures buffers, switching threads changes which buffers are "hydrated", skills load per-thread, `*agent-chat*` remains globally active.

See: [[AI-ADR-001-thread-centric-context]], [[amacs-rfc-v3.md]] Part 5-6

### Out of Scope 

- LoRA adapter loading (Phase 4)
- Automatic thread creation from buffer changes
- X11-based "user touched buffer" detection
- Skill binding implementation (IMP-003, but we prepare the hooks)

### Design/Approach  

Currently consciousness has `:watching-buffers` as a flat global list. Threads have `:buffers` but it's unused. This IMP makes threads authoritative for their context.

**Key changes:**

1. **Consciousness schema:**
   - Remove `:watching-buffers`
   - Add `:global-buffers` (default: `("*agent-chat*")`)
   - Threads get `:hydrated`, `:primary-mode`, `:skill-tags`

2. **Thread lifecycle:**
   - `agent-create-thread` infers initial buffers, mode, skill-tags
   - `agent-switch-thread` dehydrates old, hydrates new
   - Only active thread's buffers are "watched" for changes

3. **Context assembly:**
   - Active thread: full buffer content + skills
   - Pending threads: metadata summary only
   - Global buffers: always included

4. **Wake logic:**
   - Wake on changes to active thread's buffers OR global buffers
   - Ignore changes to pending thread buffers (they're not watching)

### Files to Touch

```
harness/agent-consciousness.el  # Schema changes, global-buffers
harness/agent-threads.el        # NEW - thread management
harness/agent-context.el        # NEW - context assembly
harness/agent-tick.el           # Integrate new context assembly
harness/agent-core.el           # Thread initialization on startup
test-harness.el                 # Thread context tests
```

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**? 
</CRITICAL_RULE> 

- [x] Update `agent-consciousness.el`:
  - [x] Replace `:watching-buffers` with `:global-buffers` in default schema
  - [x] Add `:global-buffers` to default consciousness (`("*agent-chat*")`)
  - [x] Update thread schema with `:hydrated`, `:primary-mode`, `:skill-tags`
- [x] Implement `agent-threads.el`:
  - [x] Implement `agent-create-thread` (concern, optional buffers → full thread)
  - [x] Implement `agent-get-active-thread` 
  - [x] Implement `agent-get-pending-threads`
  - [x] Implement `agent-switch-thread` (dehydrate old, hydrate new)
  - [x] Implement `agent-complete-thread` (mark completed with evidence)
  - [x] Implement `thread-summary` (metadata-only representation)
  - [x] Implement `infer-skill-tags` (from buffers + mode)
  - [x] Provide `agent-threads` feature
- [x] Implement `agent-context.el`:
  - [x] Implement `hydrate-buffers` (list of buffer names → content plist)
  - [x] Implement `build-thread-context` (active + pending + global)
  - [x] Implement `agent-consciousness-summary` (trimmed for context)
  - [x] Provide `agent-context` feature
- [x] Modify `agent-tick.el`:
  - [x] Use new `build-thread-context` in tick cycle
  - [x] Update wake logic to check active thread buffers + global
- [x] Modify `agent-core.el`:
  - [x] Initialize default thread on cold start if none exists
  - [x] Require new modules
- [ ] Test: create thread captures current buffer
- [ ] Test: switch thread changes which buffers would be hydrated
- [ ] Test: `*agent-chat*` always in context regardless of thread
- [ ] Test: pending threads show as summaries, not full content
- [ ] Test: context assembly produces expected structure
 
### Acceptance Criteria

**Scenario:** Thread creation captures context
**GIVEN** User is in `src/main.rs` buffer (rust-mode)
**WHEN** Code calls `(agent-create-thread "Fix ownership error")`
**THEN** Thread has `:buffers ("src/main.rs")`
**AND** Thread has `:primary-mode 'rust-mode`
**AND** Thread has `:skill-tags ("rust-mode")`
**AND** Thread has `:hydrated nil` (not active yet)

**Scenario:** Thread activation hydrates context
**GIVEN** Thread "rust-fix" exists with `:buffers ("src/main.rs" "Cargo.toml")`
**WHEN** Code calls `(agent-switch-thread "rust-fix")`
**THEN** `:active-thread` becomes "rust-fix"
**AND** Thread's `:hydrated` becomes `t`
**AND** Previous active thread's `:hydrated` becomes `nil`

**Scenario:** Context assembly reflects active thread
**GIVEN** Active thread "rust-fix" has `:buffers ("src/main.rs")`
**AND** Pending thread "docs" has `:buffers ("README.md")`
**WHEN** Code calls `(build-thread-context)`
**THEN** Result `:active-thread :buffers` contains "src/main.rs" content
**AND** Result `:pending-threads` contains "docs" as summary (no README content)
**AND** Result `:global` contains "*agent-chat*" content

**Scenario:** Global buffer always present
**GIVEN** Any thread is active
**WHEN** Code calls `(build-thread-context)`
**THEN** `*agent-chat*` content is in `:global` section

**Scenario:** Thread completion captures evidence
**GIVEN** Thread "rust-fix" is active
**WHEN** Code calls `(agent-complete-thread "rust-fix" :evidence (...) :learned "...")`
**THEN** Thread moves to `:completed-threads`
**AND** Thread has `:completion-tick`, `:completion-evidence`, `:learned`

### Thread Schema Reference

```elisp
(:id "rust-fix"
 :started-tick 142
 :concern "Ownership error in main.rs"
 :goal "Fix ownership error"                    ; Optional
 :deliverable "cargo build passes"              ; Optional
 :thread-type :deliverable                      ; or :exploratory
 
 ;; Context ownership
 :buffers ("src/main.rs" "Cargo.toml")
 :primary-mode 'rust-mode
 :skill-tags ("rust-mode" "project-amacs")
 :hydrated t
 
 ;; Work state
 :priority 1
 :approach "Trying lifetime annotations"
 :blocking t
 
 ;; On completion (filled by agent-complete-thread)
 :completion-tick nil
 :completion-evidence nil
 :learned nil
 
 ;; Future (Phase 4)
 :active-loras nil)
```

### Issues Encountered 

<!-- Fill during implementation -->
