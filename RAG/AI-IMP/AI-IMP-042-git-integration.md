---
node_id: AI-IMP-042
tags:
  - IMP-LIST
  - Implementation
  - git
  - commits
  - history
kanban_status: planned
depends_on:
  - AI-IMP-039
confidence_score: 0.90
created_date: 2025-01-03
close_date:
---

# AI-IMP-042: Git Integration

## Summary

Commit all state changes after each tick with the established commit format.

**Current state:** v3 had git commits but needs adaptation for new flow.

**Target state:** Every tick commits consciousness, chat, scratchpad with format `[TICK N][thread][mood] monologue`.

**Done when:** Each inference produces a git commit, history is queryable.

### Out of Scope

- Git log querying UI (future polish)
- Branch management

### Design/Approach

Commit format:
```
[TICK 42][rust-debugging][focused] Fixed the borrow checker issue
```

Components:
- TICK: current tick number
- thread: active thread ID or "no-thread"
- mood: current mood string
- summary: monologue line (truncated to ~80 chars)

Files to commit:
- `consciousness.el`
- `agent-chat.org`
- `scratchpad.org`
- `monologue.org`

Commit happens after:
1. Response parsed successfully
2. Eval executed (if any)
3. Serialization complete

Use existing `agent-git-commit` but ensure it works with new flow.

### Files to Touch

- `harness/agent-tick.el`: ensure git commit function works
- `harness/amacs-shell.el`: call commit after inference complete
- `harness/agent-core.el`: ensure git init on startup

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**?
</CRITICAL_RULE>

- [ ] Verify `agent-git-init` creates repo if needed
- [ ] Verify `.gitignore` excludes credentials
- [ ] Implement `agent-build-commit-message`
- [ ] Format: `[TICK %d][%s][%s] %s`
- [ ] Truncate monologue to 80 chars with "..."
- [ ] Handle missing thread (use "no-thread")
- [ ] Implement `agent-tick-commit` (or update existing)
- [ ] Stage all files in ~/.agent/
- [ ] Create commit with formatted message
- [ ] Handle empty changes gracefully (no error)
- [ ] Store commit hash in consciousness `last-commit`
- [ ] Integrate into inference completion flow
- [ ] Call after serialization, before new prompt
- [ ] Test: first tick creates initial commit
- [ ] Test: subsequent ticks create new commits
- [ ] Test: commit message format correct
- [ ] Test: all expected files staged
- [ ] Test: credentials not committed
- [ ] Byte-compile without warnings

### Acceptance Criteria

**Scenario:** Tick produces commit
**GIVEN** Agent completes inference at tick 42
**AND** Active thread is "rust-debugging"
**AND** Mood is "focused"
**AND** Monologue is "Fixed the borrow checker issue"
**WHEN** Tick completes
**THEN** Git commit is created
**AND** Message is `[TICK 42][rust-debugging][focused] Fixed the borrow checker issue`

**Scenario:** Git log shows history
**GIVEN** Agent has completed 10 ticks
**WHEN** User runs `git log --oneline` in ~/.agent/
**THEN** 10 commits appear with correct format
**AND** Each shows tick number, thread, mood, summary

**Scenario:** Credentials not committed
**GIVEN** ~/.agent/config.el contains API key
**WHEN** Commit is created
**THEN** config.el is NOT included in commit
**AND** .gitignore excludes it

### Issues Encountered

<!-- Fill during implementation -->
