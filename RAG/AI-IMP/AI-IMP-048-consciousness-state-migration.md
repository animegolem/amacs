---
node_id: AI-IMP-048
tags:
  - IMP-LIST
  - Implementation
  - EPIC-007
  - consciousness
  - refactor
kanban_status: backlog
depends_on: []
confidence_score: 0.8
created_date: 2025-01-11
close_date:
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

- [ ] Add `(require 'agent-consciousness)` to amacs-shell.el
- [ ] Replace `amacs-shell--current-tick` reads with `(agent-current-tick)`
- [ ] Replace tick increment with `(agent-increment-tick)`
- [ ] Replace `amacs-shell--active-thread` with `(agent-get 'active-thread)`
- [ ] Replace `amacs-shell--open-threads` with `(agent-get 'open-threads)`
- [ ] Replace `amacs-shell--completed-threads` with `(agent-get 'completed-threads)`
- [ ] Replace `amacs-shell--last-eval-result` with `(agent-get 'last-eval-result)` / `(agent-set ...)`
- [ ] Store mood/confidence in consciousness after response: `(agent-set 'mood ...)`, `(agent-set 'confidence ...)`
- [ ] Remove chat-history shell var, read from org file via persistence
- [ ] Ensure `identity` field is set during init (e.g., "amacs-instance-1" or hostname-based)
- [ ] Ensure `current-time` is updated each tick with ISO8601 timestamp
- [ ] Ensure `(agent-init)` called before shell operations
- [ ] Keep shell-local: `amacs-shell--pending-input`, `amacs-shell--processing`
- [ ] Update `amacs-shell--format-consciousness` to read from alist
- [ ] Update thread functions to use consciousness: `agent-create-thread`, `agent-switch-thread`, `agent-complete-thread`
- [ ] Run CI: `./harness/ci-check.sh`
- [ ] Verify multi-turn conversation still works

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

<!-- This section filled during implementation -->
