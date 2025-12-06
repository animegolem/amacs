---
node_id: AI-EPIC-001
tags: 
  - EPIC
  - AI
  - phase-1
  - vampire-simulator
date_created: 2025-11-27
date_completed: 
kanban-status: in-progress
AI_IMP_spawned: 
  - AI-IMP-001 (completed)
  - AI-IMP-002 (completed)
  - AI-IMP-003
  - AI-IMP-004
---

# AI-EPIC-001-vampire-simulator-core

## Problem Statement/Feature Scope 

The AMACS project requires a working cognitive loop before any higher-level features (skills, intrusive thoughts, sub-agents) can be tested. Currently no implementation exists - only design documents. We need the foundational "heartbeat" that everything else builds on.

## Proposed Solution(s) 

Build the minimum viable tick cycle for Phase 1 (Vampire Simulator):

1. **Consciousness variable** - Elisp plist holding working memory, persisted to disk
2. **Tick function** - Manual trigger that updates state and commits to git
3. **Monologue system** - Append-only log feeding commit messages
4. **Bootstrap skill** - Minimal orientation loaded on startup

This epic covers the *harness* only - no LLM inference yet. The agent will be "braindead" but the body will function: perceive nothing, do nothing, but track time and commit.

See: [[amacs-rfc-v3.md]] Part 5 (Tick System), Part 6 (Consciousness Variable)

## Path(s) Not Taken 

- VSock separation (Phase 2)
- Intrusive thoughts / critic system (later in Phase 1, separate epic)
- Skills beyond bootstrap (emergent, not pre-built)
- Any actual LLM API calls

## Success Metrics 

1. **Coherent 50-tick run**: Execute `M-x agent-tick` 50 times, git log shows sensible progression
2. **Warm start works**: Kill Emacs, restart, state resumes from last tick
3. **Monologue feeds commits**: Each commit message contains the monologue line
4. **Bootstrap skill loads**: On startup, core skill is in `:active-skills`

Timeline: 1-2 sessions to complete all IMPs.

## Requirements

### Functional Requirements

- [ ] FR-1: System shall define `agent-consciousness` variable per schema in RFC
- [ ] FR-2: System shall persist consciousness to `~/.agent/consciousness.el` each tick
- [ ] FR-3: System shall load persisted consciousness on Emacs startup (warm start)
- [ ] FR-4: System shall provide `M-x agent-tick` command that increments tick counter
- [ ] FR-5: System shall commit to git after each tick with monologue as message
- [ ] FR-6: System shall append to `~/.agent/monologue.org` each tick
- [ ] FR-7: System shall maintain rolling window of recent monologue in consciousness
- [ ] FR-8: System shall load bootstrap skill on initialization
- [ ] FR-9: System shall track skill usage in `:active-skills`

### Non-Functional Requirements 

- Tick execution < 500ms (excluding git push)
- No external dependencies beyond Emacs built-ins
- Works in headless Emacs (`emacs -Q --batch`)
- Git repository must exist but commits are local-only for now

## Implementation Breakdown 

| IMP | Title | Status | Notes |
|-----|-------|--------|-------|
| AI-IMP-001 | Heartbeat (consciousness + persistence + tick) | **completed** | Foundation |
| AI-IMP-002 | Monologue system | **completed** | Depends on 001 |
| AI-IMP-003 | Bootstrap skill loading | **completed** | Depends on 001 |
| AI-IMP-004 | Thread-centric context | planned | Depends on 001, 002. See ADR-001 |
