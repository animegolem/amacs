---
node_id: LOG-2025-12-06-001
tags:
  - AI-log
  - development-summary
  - phase-1
  - epic-completion
  - thread-centric
  - elisp-debugging
closed_tickets:
  - AI-IMP-003
  - AI-IMP-004
  - AI-EPIC-001
created_date: 2025-12-06
related_files:
  - harness/agent-threads.el
  - harness/agent-context.el
  - harness/agent-consciousness.el
  - harness/agent-core.el
  - harness/agent-skills.el
  - harness/test-harness.el
  - RAG/AI-ADR/AI-ADR-001-thread-centric-context.md
  - RAG/AI-EPIC/AI-EPIC-001-vampire-simulator-core.md
  - RAG/AI-EPIC/AI-EPIC-001b-first-breath.md
  - RAG/AI-EPIC/AI-EPIC-002-bicameral-mind.md
  - RAG/AI-IMP/AI-IMP-004-thread-centric-context.md
  - RAG/AI-IMP/AI-IMP-005-ci-pipeline.md
  - RAG/AI-IMP/AI-IMP-011-first-inference.md
confidence_score: 0.92
---

# 2025-12-06-LOG-AI-epic-001-completion-thread-context

## Work Completed

Completed Phase 1 (Vampire Simulator) implementation, closing EPIC-001 with all four IMPs passing tests.

**IMP-003 (Bootstrap Skill Loading):** Finalized skill system integration. Created `agent-skills.el` with skill loading, binding, and tracking. Core skill installs from `/amacs/skills/amacs-bootstrap-skill/core/` to `~/.agent/skills/core/` on init.

**IMP-004 (Thread-Centric Context):** Major architectural implementation based on ADR-001. Threads now own their buffers rather than global `:watching-buffers`. Created:
- `agent-threads.el` (~250 lines): Thread lifecycle (create, switch, complete), hydration states, summaries
- `agent-context.el` (~150 lines): Context assembly with active thread hydration, pending thread summaries, global buffers

Updated consciousness schema: `:watching-buffers` → `:global-buffers ("*agent-chat*")`. Cold start now creates default "initial-exploration" thread.

**Test Suite:** Expanded from ~25 to 39 tests covering thread creation, switching, hydration, context assembly, and global buffer handling. All tests passing.

**Planning:** Created EPIC-001b (First Breath) for API integration de-risk before Proxmox work. Created EPIC-002 (Bicameral Mind) for Phase 2. Wrote IMP-005 (CI pipeline) and IMP-011 (first inference) specifications.

## Session Commits

Session worked on harness files but commits happen in `~/.agent/` during test runs. Key file changes:

| File | Change |
|------|--------|
| `agent-threads.el` | NEW - Thread lifecycle, hydration, summaries |
| `agent-context.el` | NEW - Context assembly for inference |
| `agent-skills.el` | NEW - Skill loading and binding system |
| `agent-consciousness.el` | Schema update, timestamp parsing fix |
| `agent-core.el` | Require new modules, init threads, fix `agent--has-commits-p` |
| `test-harness.el` | Added ~15 new tests for skills and threads |

RAG document updates:
- EPIC-001: Marked completed with all FRs checked
- IMP-003, IMP-004: Marked completed with close dates
- Created EPIC-001b, EPIC-002, IMP-005, IMP-011
- RFC v3.2 → v3.3 with audit trail update

## Issues Encountered

**1. Git Commit Detection Bug (`agent--has-commits-p`)**
Original implementation checked if `git rev-parse HEAD` output was empty. On fresh repos, git outputs an error *message* to stdout which is not empty, causing function to return `t` when no commits exist. Fixed by checking return code instead of output.

**2. Timestamp Parsing (`parse-iso8601-time-string`)**
Function was failing silently in some Emacs versions. Added `agent--parse-iso-time` wrapper with fallback to `date-to-time` and proper error handling.

**3. Constant Symbol Assignment (`(lambda (t) ...)`)**
Used `t` as lambda parameter in several places. Elisp's `t` is a constant (truth value), causing "Attempt to set a constant symbol" error. Renamed all instances to `thr`.

**4. Backquote Structure Sharing (MAJOR)**
Most significant bug of the session. `agent-create-thread` used backquote to construct thread plist:
```elisp
`(:id ,thread-id ... :hydrated nil ...)
```
Backquote optimizes by sharing cons cells for "constant" parts. When `plist-put` later mutated `:hydrated` to `t` on one thread, it mutated shared structure affecting all future threads. First thread returned `:hydrated nil`, second returned `:hydrated t` despite identical code.

**Fix:** Replaced backquote with explicit `list` call to ensure fresh cons cells:
```elisp
(list :id thread-id ... :hydrated nil ...)
```

**Lesson:** Avoid backquote for plists that will be destructively modified. Byte-compile doesn't catch this. Documented in IMP-004 Issues Encountered section.

## Tests Added

**IMP-003 Skill Tests (4):**
- `test-skill-system-init`: Skills directory and core skill installed
- `test-skill-loading`: Core skill loads with valid content
- `test-skill-tracking`: `:active-skills` tracks usage with counts
- `test-relevant-skills`: Core skill in relevant list

**IMP-004 Thread/Context Tests (5):**
- `test-default-thread-created`: Cold start creates and activates thread
- `test-thread-creation`: New threads capture buffer, mode, skill-tags, start dehydrated
- `test-thread-switching`: Switch changes active thread, updates hydration state
- `test-global-buffers`: `*agent-chat*` in global-buffers and watched list
- `test-context-assembly`: `agent-build-context` produces expected structure

Total test count: 39/39 passing

## Next Steps

**Immediate (IMP-011 - First Breath):**
- Implement `agent-api.el` for OpenRouter calls
- Implement `agent-inference.el` for prompt assembly
- Create `M-x agent-think` command
- Validate end-to-end with real LLM response

**Then (IMP-005 - CI Pipeline):**
- Create `ci-check.sh` for byte-compile + test validation
- Would have caught the `(lambda (t))` bug
- Prerequisite for safe iteration

**Read Before Continuing:**
- `harness/agent-threads.el` - Thread model
- `harness/agent-context.el` - How context assembles
- `RAG/AI-IMP/AI-IMP-011-first-inference.md` - Full spec for API integration
- `skills/amacs-bootstrap-skill/core/SKILL.md` - What agent sees on startup

**Key Insight:** The thread-centric model enables future training by making each thread a coherent unit with its own context. This was the GPT 5.1 insight about training granularity - now architecturally supported.
