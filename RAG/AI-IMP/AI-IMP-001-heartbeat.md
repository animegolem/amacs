---
node_id: AI-IMP-001
tags:
  - IMP-LIST
  - Implementation
  - phase-1
  - consciousness
  - persistence
kanban_status: planned
depends_on: []
confidence_score: 0.85
created_date: 2025-11-27
close_date: 
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
~/.emacs.d/amacs/agent-core.el       # New file - main entry point
~/.emacs.d/amacs/agent-consciousness.el  # New file - consciousness management
~/.emacs.d/amacs/agent-tick.el       # New file - tick cycle
~/.agent/                            # Directory created on init
~/.agent/.gitignore                  # Ignore scratch files
```

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**? 
</CRITICAL_RULE> 

- [ ] Create `~/.emacs.d/amacs/` directory structure
- [ ] Implement `agent-consciousness.el`:
  - [ ] Define `agent-consciousness` variable with full schema (identity, tick, time, mood, confidence, threads, etc.)
  - [ ] Implement `agent-init-consciousness` for cold start
  - [ ] Implement `agent-persist-consciousness` (write to disk)
  - [ ] Implement `agent-load-consciousness` (read from disk)
  - [ ] Implement `agent-get` / `agent-set` accessors
- [ ] Implement `agent-tick.el`:
  - [ ] Implement `agent-increment-tick` (bump counter + timestamps)
  - [ ] Implement `agent-check-gap` (detect >1hr gaps)
  - [ ] Implement `agent-commit` (git add -A + commit with message)
  - [ ] Implement `agent-tick` interactive command (the full cycle)
- [ ] Implement `agent-core.el`:
  - [ ] Implement `agent-ensure-directories` (create ~/.agent if missing)
  - [ ] Implement `agent-init` (cold start or warm start based on file existence)
  - [ ] Add autoload for `agent-tick`
  - [ ] Provide `agent-core` feature
- [ ] Create `~/.agent/.gitignore` with sensible defaults
- [ ] Initialize git repo in `~/.agent/` 
- [ ] Test: cold start creates consciousness, tick 0 commits
- [ ] Test: 10 ticks produce 10 commits with incrementing tick numbers
- [ ] Test: kill Emacs, restart, warm start resumes from tick 10
- [ ] Test: verify `:long-gap-detected` triggers after simulated gap
 
### Acceptance Criteria

**Scenario:** Fresh install, first run
**GIVEN** `~/.agent/` does not exist
**WHEN** User loads agent-core and calls `(agent-init)`
**THEN** `~/.agent/` directory is created
**AND** `~/.agent/consciousness.el` exists with tick 0
**AND** Git repo is initialized
**AND** Initial commit is made

**Scenario:** Normal tick cycle
**GIVEN** Agent is initialized at tick 5
**WHEN** User calls `M-x agent-tick`
**THEN** `:current-tick` becomes 6
**AND** `:current-time` is updated
**AND** Consciousness is persisted to disk
**AND** Git commit is made with message containing "TICK 6"

**Scenario:** Warm start after restart
**GIVEN** Agent ran to tick 10, Emacs was killed
**WHEN** User starts Emacs and calls `(agent-init)`
**THEN** Consciousness loads from disk
**AND** `:current-tick` is 10
**AND** Next `agent-tick` produces tick 11

**Scenario:** Long gap detection
**GIVEN** Agent last ran 2 hours ago (`:last-inference-time` is old)
**WHEN** User calls `agent-tick`
**THEN** `:long-gap-detected` is set to `t`

### Issues Encountered 

<!-- Fill during implementation -->
