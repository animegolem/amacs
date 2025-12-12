---
node_id: AI-EPIC-002
tags: 
  - EPIC
  - AI
  - phase-2
  - bicameral-mind
  - vsock
  - proxmox
date_created: 2025-12-06
date_completed: 
kanban-status: planned
AI_IMP_spawned: 
---

# AI-EPIC-002-bicameral-mind

## Problem Statement/Feature Scope 

Phase 1 proved the cognitive loop works on a single machine. However, the AMACS architecture requires security isolation between the "brain" (LLM API access) and "body" (Emacs environment). Without this separation, we cannot safely test real LLM inference or skill emergence - a compromised agent could exfiltrate data or make unauthorized API calls.

Additionally, the current harness lacks CI validation, making it easy for bugs (like the backquote structure-sharing issue) to slip through undetected.

## Proposed Solution(s) 

Deploy AMACS on Proxmox infrastructure with VM isolation:

1. **Brain VM (LXC)**: API access only, communicates with body via vsock
2. **Body VM**: Airgapped Emacs environment, vsock to brain and Gitea
3. **Gitea VM**: Accepts commits, stores history, runs CI pipelines

The epic adds:
- CI pipeline for byte-compilation and test suite validation
- VSock communication layer between brain and body
- Protected core services via systemd quadlets
- Full skills directory with agent-created skills
- Advisory sub-agents (report-only helpers)
- Real budget tracking with cost constraints

See: [[amacs-rfc-v3.md]] Part 7 (Architecture Stack), [[The_10_Minute_Vampire_v2]] Section 3-4

## Path(s) Not Taken 

- Full EXWM desktop control (Phase 3)
- Self-modification of containerfiles (Phase 3)
- LoRA adapter training/routing (Phase 4)
- Multiple model support in single session (future)

## Success Metrics 

1. **CI catches regressions**: Byte-compile errors and test failures block commits (measured by blocked bad commits)
2. **VSock round-trip < 100ms**: Brain-body communication adds acceptable latency
3. **Security isolation holds**: Red-team exercises fail to breach airgap (see RFC Section 7)
4. **Skills get reused**: At least 3 agent-created skills show use-count > 5 over 500 ticks
5. **Budget pressure visible**: Agent monologue references cost/budget at least once per 50 ticks

Timeline: 2-4 weeks

## Requirements

### Functional Requirements

- [ ] FR-1: CI pipeline shall byte-compile all elisp files and fail on warnings
- [ ] FR-2: CI pipeline shall run test-harness.el and report pass/fail
- [ ] FR-3: Brain VM shall communicate with Body VM exclusively via vsock
- [ ] FR-4: Body VM shall have no TCP/IP networking (airgapped)
- [ ] FR-5: Body VM shall communicate with Gitea VM via vsock for git operations
- [ ] FR-6: Agent shall be able to create new skills in `~/.agent/skills/`
- [ ] FR-7: Agent shall be able to bind skills to modes, buffers, or projects
- [ ] FR-8: System shall track real API costs in `:budget` consciousness field
- [ ] FR-9: System shall support advisory sub-agents with read-only access
- [ ] FR-10: Core services (tick loop, consciousness persistence) shall run as systemd quadlets
- [ ] FR-11: System shall support human review flag that pauses autonomous operation

### Non-Functional Requirements 

- VSock round-trip latency < 100ms for typical payloads
- Body VM snapshot/restore < 30 seconds
- CI pipeline execution < 60 seconds
- No shared filesystem between Brain and Body (vsock messages only)
- Gitea runner executes in isolated environment
- All secrets (API keys) stored only in Brain VM

## Implementation Breakdown 

| IMP | Title | Status | Notes |
|-----|-------|--------|-------|
| AI-IMP-005 | CI pipeline (byte-compile + test harness) | planned | First priority |
| AI-IMP-006 | Proxmox infrastructure setup | planned | Brain/Body/Gitea VMs |
| AI-IMP-007 | VSock communication layer | planned | Depends on 006 |
| AI-IMP-008 | Full skills system (create/bind/track) | planned | Extends IMP-003 |
| AI-IMP-009 | Budget tracking with real costs | planned | Depends on 007 |
| AI-IMP-010 | Advisory sub-agents | planned | Report-only helpers |
