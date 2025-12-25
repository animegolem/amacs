---
node_id: AI-EPIC-003
tags: 
  - EPIC
  - AI
  - phase-2
  - remediation
  - field-test
date_created: 2025-12-24
date_completed: 2025-12-25
kanban-status: complete
AI_IMP_spawned: 
  - AI-IMP-025
  - AI-IMP-026
  - AI-IMP-027
  - AI-IMP-028
---

# AI-EPIC-003: Field Test Remediation

## Problem Statement/Feature Scope

Field testing with Sonnet (61 ticks) revealed critical usability issues:

1. **Chat fragmentation** - Agent couldn't reliably read human messages due to buffer/file mismatch and hardcoded buffer names
2. **Context truncation** - 4000 char limit cut off buffer contents, agent reported things being "cut off"
3. **No scratchpad** - Agent had nowhere to persist working notes across ticks
4. **Plist brittleness** - Data structure inconsistency between consciousness and threads
5. **Skill loading gaps** - Only core skill copied, chat skill missing

The agent spent 25+ ticks trying to locate human messages instead of doing useful work.

## Proposed Solution(s)

Comprehensive remediation addressing data structures, buffer discovery, chat interface, and context assembly:

**1. Alist Migration (IMP-025)**
Convert consciousness and threads from plists to alists. Provides cleaner access patterns (`alist-get` with defaults, `setf` support) and direct JSON mapping.

**2. Mode-Based Buffer Discovery (IMP-026)**  
Replace hardcoded buffer names with mode-based discovery. Add `agent-scratchpad-mode` for working notes. Buffers with `amacs-chat-mode` or `agent-scratchpad-mode` are automatically included in context.

**3. Chat Interface Redesign (IMP-027)**
New structure using `#+begin_prompt`/`#+end_prompt` blocks for human input. Agent response function transforms into tick-based org headings. Clean parsing via org-element.

**4. Context Assembly Simplification (IMP-028)**
Remove 4000 char truncation. Include full consciousness state. Agent controls context depths via state variables (`:chat-context-depth`, `:monologue-context-depth`). Copy all bootstrap skills on init.

## Path(s) Not Taken

- **Async API calls** - Would help UX but adds complexity. Deferred to Phase 3.
- **Custom org-element parser** - Sticking with standard org parsing.
- **Thread-local chat** - Chat remains global. Threads bind buffers/skills, not chat history.

## Success Metrics

1. Agent can reliably read human messages on first attempt (no buffer hunting)
2. Full buffer contents visible in context (no truncation complaints)
3. Agent can persist working notes in scratchpad
4. Clean data access patterns throughout codebase
5. All bootstrap skills available on init

**Validation:** Run 20+ tick session with Sonnet, observe no context/chat confusion.

## Requirements

### Functional Requirements

- [ ] FR-1: Consciousness variable uses alist structure with symbol keys
- [ ] FR-2: Thread structures use alist format matching consciousness
- [ ] FR-3: All buffers with `amacs-chat-mode` discovered automatically
- [ ] FR-4: All buffers with `agent-scratchpad-mode` discovered automatically
- [ ] FR-5: `~/.agent/scratchpad.org` created on init with scratchpad mode
- [ ] FR-6: Human input wrapped in `#+begin_prompt`/`#+end_prompt`
- [ ] FR-7: Agent response transforms prompt into `* Tick N` heading structure
- [ ] FR-8: Buffer content included without character truncation
- [ ] FR-9: Agent controls chat depth via `:chat-context-depth`
- [ ] FR-10: Agent controls monologue depth via `:monologue-context-depth`
- [ ] FR-11: All skills in `skills/amacs-bootstrap-skill/` copied on init

### Non-Functional Requirements

- NFR-1: Backward compatibility - existing consciousness files should load (with migration)
- NFR-2: No new external dependencies
- NFR-3: All changes covered by test-harness.el tests
- NFR-4: Style guide compliance (alist conventions documented)

## Implementation Breakdown

| IMP | Title | Status | Dependencies |
|-----|-------|--------|--------------|
| AI-IMP-025 | Alist Migration | complete | none |
| AI-IMP-026 | Mode-Based Discovery + Scratchpad | complete | IMP-025 |
| AI-IMP-027 | Chat Interface Redesign | complete | IMP-026 |
| AI-IMP-028 | Context Assembly + Skill Init | complete | IMP-025, IMP-026 |

**Dependency Graph:**
```
IMP-025 (Alist Migration)
    ↓
IMP-026 (Mode Discovery + Scratchpad)
    ↓
IMP-027 (Chat Redesign) ←── IMP-028 (Context + Skills)
```
