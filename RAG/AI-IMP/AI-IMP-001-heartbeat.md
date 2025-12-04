---
node_id: AI-IMP-001
tags:
  - IMP-LIST
  - Implementation
  - phase-1
  - consciousness
  - persistence
kanban_status: completed
depends_on: []
confidence_score: 0.95
created_date: 2025-11-27
close_date: 2025-11-27
--- 

# AI-IMP-001-heartbeat

## Consciousness Variable + Persistence + Minimal Tick

Implement the foundational loop: define consciousness structure, persist to disk, load on startup, and provide manual tick command that updates timestamp and commits.

**Done when:** Can execute `M-x agent-tick` 10 times, restart Emacs, tick resumes from correct count, and `git log` shows 10 commits with tick numbers.

See: [[amacs-rfc-v3.md]] Part 5-6, [[consciousness-schema.md]]

### Out of Scope 

- Monologue system (AI-IMP-002)
- Skill loading (AI-IMP-003)
- LLM inference
- VSock / network
- Intrusive thoughts

### Design/Approach  

Use the existing `consciousness-helpers.el` from bootstrap skill as reference, but implement fresh in the actual agent codebase. The consciousness variable is an elisp plist. Persistence uses `prin1` to write and `load` to read.

Directory structure:
```
~/.agent/
├── consciousness.el    # Persisted state
├── monologue.org       # (IMP-002)
└── skills/             # (IMP-003)
```

Git repo lives at `~/.agent/` - consciousness.el is committed each tick.

Tick cycle (minimal version):
1. Increment `:current-tick`
2. Update `:current-time`
3. Check for long gap (>1hr since last tick)
4. Persist to disk
5. Git add + commit

### Files to Touch

```
amacs/harness/agent-core.el          # Main entry point
amacs/harness/agent-consciousness.el # Consciousness management
amacs/harness/agent-tick.el          # Tick cycle
amacs/harness/test-harness.el        # Manual test suite
~/.agent/                            # Directory created on init
~/.agent/.gitignore                  # Ignore scratch files
```

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**? 
</CRITICAL_RULE> 

- [x] Create `amacs/harness/` directory structure
- [x] Implement `agent-consciousness.el`:
  - [x] Define `agent-consciousness` variable with full schema (identity, tick, time, mood, confidence, threads, etc.)
  - [x] Implement `agent-init-consciousness` for cold start
  - [x] Implement `agent-persist-consciousness` (write to disk)
  - [x] Implement `agent-load-consciousness` (read from disk)
  - [x] Implement `agent-get` / `agent-set` accessors
- [x] Implement `agent-tick.el`:
  - [x] Implement `agent-increment-tick` (bump counter + timestamps)
  - [x] Implement `agent-check-gap` (detect >1hr gaps)
  - [x] Implement `agent-git-commit` (git add -A + commit with message)
  - [x] Implement `agent-tick` interactive command (the full cycle)
- [x] Implement `agent-core.el`:
  - [x] Implement `agent--ensure-directories` (create ~/.agent if missing)
  - [x] Implement `agent-init` (cold start or warm start based on file existence)
  - [x] Add autoload for `agent-tick`
  - [x] Provide `agent-core` feature
- [x] Create `~/.agent/.gitignore` with sensible defaults
- [x] Initialize git repo in `~/.agent/` 
- [x] Test: cold start creates consciousness, tick 0 commits
- [x] Test: 10 ticks produce 10 commits with incrementing tick numbers
- [x] Test: kill Emacs, restart, warm start resumes from tick 10
- [x] Test: verify `:long-gap-detected` triggers after simulated gap
 
### Acceptance Criteria

**Scenario:** Fresh install, first run ✓
**GIVEN** `~/.agent/` does not exist
**WHEN** User loads agent-core and calls `(agent-init)`
**THEN** `~/.agent/` directory is created
**AND** `~/.agent/consciousness.el` exists with tick 0
**AND** Git repo is initialized
**AND** Initial commit is made

**Scenario:** Normal tick cycle ✓
**GIVEN** Agent is initialized at tick 5
**WHEN** User calls `M-x agent-tick`
**THEN** `:current-tick` becomes 6
**AND** `:current-time` is updated
**AND** Consciousness is persisted to disk
**AND** Git commit is made with message containing "TICK 6"

**Scenario:** Warm start after restart ✓
**GIVEN** Agent ran to tick 10, Emacs was killed
**WHEN** User starts Emacs and calls `(agent-init)`
**THEN** Consciousness loads from disk
**AND** `:current-tick` is 10
**AND** Next `agent-tick` produces tick 11

**Scenario:** Long gap detection ✓
**GIVEN** Agent last ran 2 hours ago (`:last-inference-time` is old)
**WHEN** User calls `agent-tick`
**THEN** `:long-gap-detected` is set to `t`

### Issues Encountered 

1. **File location deviation:** Originally planned `~/.emacs.d/amacs/` but implemented in `amacs/harness/` within repo. This is cleaner - source lives in repo, installed via load-path. No functional impact.

2. **Lexical binding warning:** Generated `consciousness.el` initially lacked `;;; -*- lexical-binding: t; -*-` cookie. Fixed by adding to `agent-persist-consciousness` output.

3. **Git log error handling:** Initial implementation used shell redirect syntax in `call-process` which doesn't work. Fixed by adding `agent--has-commits-p` helper with proper error handling.

### Artifacts Produced

| File | Purpose |
|------|---------|
| `harness/agent-consciousness.el` | Consciousness variable + persistence (~180 lines) |
| `harness/agent-tick.el` | Tick cycle + git commits (~120 lines) |
| `harness/agent-core.el` | Initialization + entry point (~130 lines) |
| `harness/test-harness.el` | Manual test suite (~150 lines) |
