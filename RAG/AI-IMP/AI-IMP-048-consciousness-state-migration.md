---
node_id: AI-IMP-048
tags:
  - IMP-LIST
  - Implementation
  - EPIC-007
  - consciousness
  - refactor
kanban_status: done
depends_on: []
confidence_score: 0.95
created_date: 2025-01-11
close_date: 2025-01-18
---

# AI-IMP-048-consciousness-state-migration

## Summary

The v4 shell (`amacs-shell.el`) maintains its own state variables instead of using the consciousness alist. This IMP migrates state from shell-local variables to `agent-consciousness.el`, making the alist the single source of truth.

**Current state**: Shell has `amacs-shell--current-tick`, `amacs-shell--last-response`, `amacs-shell--active-thread`, `amacs-shell--open-threads`, `amacs-shell--completed-threads`, `amacs-shell--chat-history`, `amacs-shell--last-eval-result`.

**Target state**: Shell reads/writes via `(agent-get 'field)` and `(agent-set 'field value)`. Shell retains only UI state (buffer, process, pending-input).

**Done when**: All agent state flows through consciousness alist. Shell variables reduced to UI-only concerns.

### Out of Scope

- Changing the inference/API call flow (IMP-049)
- Loading system prompt from skill (IMP-050)
- Skill system activation (IMP-051)
- Buffer hydration (IMP-052)

### Design/Approach

1. Add `require 'agent-consciousness` to shell
2. Replace each shell state variable with consciousness accessor
3. Ensure consciousness is initialized before shell use
4. Keep shell-local: `amacs-shell--pending-input`, `amacs-shell--processing`, UI buffer refs
5. Verify persistence works (consciousness.el file saves/loads correctly)

The consciousness alist already has fields for most of what shell tracks. Map:
- `amacs-shell--current-tick` → `(agent-get 'current-tick)`
- `amacs-shell--last-response` → `(agent-get 'mood)`, `(agent-get 'confidence)`
- `amacs-shell--active-thread` → `(agent-get 'active-thread)`
- `amacs-shell--open-threads` → `(agent-get 'open-threads)`
- `amacs-shell--completed-threads` → `(agent-get 'completed-threads)`
- `amacs-shell--last-eval-result` → `(agent-get 'last-eval-result)`
- `amacs-shell--chat-history` → Read from org file via `agent-persistence`

Additional fields to expose (harness-managed, agent-visible):
- `identity` → Instance identifier (e.g., "amacs-instance-1")
- `current-time` → ISO8601 timestamp, updated each tick

### Files to Touch

`harness/amacs-shell.el`: Remove state vars, add require, use accessors
`harness/agent-consciousness.el`: Verify schema supports all needed fields
`harness/test-harness.el`: Update tests that reference shell state

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**?
</CRITICAL_RULE>

- [x] Add `(require 'agent-consciousness)` to amacs-shell.el
- [x] Replace `amacs-shell--current-tick` reads with `(agent-current-tick)`
- [x] Replace tick increment with `(agent-increment-tick)`
- [x] Replace `amacs-shell--active-thread` with `(agent-get 'active-thread)`
- [x] Replace `amacs-shell--open-threads` with `(agent-get 'open-threads)`
- [x] Replace `amacs-shell--completed-threads` with `(agent-get 'completed-threads)`
- [x] Replace `amacs-shell--last-eval-result` with `(agent-get 'last-eval-result)` / `(agent-set ...)`
- [x] Store mood/confidence in consciousness after response: `(agent-set 'mood ...)`, `(agent-set 'confidence ...)`
- [x] Remove chat-history shell var, read from org file via persistence (kept chat-history for now, reads from persistence already)
- [x] Ensure `identity` field is set during init (e.g., "amacs-instance-1" or hostname-based)
- [x] Ensure `current-time` is updated each tick with ISO8601 timestamp (via agent-increment-tick)
- [x] Ensure `(agent-init)` called before shell operations (agent-init-consciousness in shell startup)
- [x] Keep shell-local: `amacs-shell--pending-input`, `amacs-shell--processing`
- [x] Update `amacs-shell--format-consciousness` to read from alist
- [x] Update thread functions to use consciousness: `agent-create-thread`, `agent-switch-thread`, `agent-complete-thread`
- [x] Run CI: `./harness/ci-check.sh` - 113/113 tests passing
- [x] Verify multi-turn conversation still works (warm start test passes)

### Acceptance Criteria

**Scenario:** Shell uses consciousness for state
**GIVEN** the shell is started with `M-x amacs-shell`
**WHEN** the user sends a message and receives a response
**THEN** the tick counter in consciousness increments
**AND** mood and confidence are stored in consciousness
**AND** `(agent-get 'current-tick)` returns correct value
**AND** shell has no `amacs-shell--current-tick` variable

**Scenario:** Thread operations use consciousness
**GIVEN** the shell is active
**WHEN** user creates a thread via eval: `(agent-create-thread "test")`
**THEN** `(agent-get 'open-threads)` includes the new thread
**AND** shell's inline thread list variable is removed

### Issues Encountered

**Scratchpad depths**: The shell had separate `global-scratchpad-depth` and `thread-scratchpad-depth` but consciousness only had a single `scratchpad-context-depth`. Added both to consciousness schema.

**Paren balancing**: Several edits required careful paren counting due to adding new `let` wrappers around existing code. Caught by CI byte-compile.

**Removed defvars**: Removed 8 shell defvars that were migrated to consciousness:
- `amacs-shell--current-tick`
- `amacs-shell--chat-context-depth`
- `amacs-shell--last-eval-result`
- `amacs-shell--open-threads`
- `amacs-shell--completed-threads`
- `amacs-shell--active-thread`
- `amacs-shell--global-scratchpad-depth`
- `amacs-shell--thread-scratchpad-depth`

**Kept for later**: `amacs-shell--chat-history` still exists but could be migrated to consciousness in a future IMP. Currently reads from persistence but keeps a local cache.
