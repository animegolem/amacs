---
node_id: LOG-2025-01-04-core-loop
tags:
  - AI-log
  - development-summary
  - architecture
  - v4-transition
  - comint
closed_tickets:
  - AI-IMP-036
created_date: 2025-01-04
related_files:
  - RAG/RFC/amacs-rfc-v4-transition.md
  - RAG/AI-EPIC/AI-EPIC-005-core-loop-rewrite.md
  - harness/amacs-shell.el
  - RAG/draft-prompt.md
confidence_score: 0.95
---

# 2025-01-04-LOG-core-loop-planning

## Work Completed

Major architecture pivot from v3 (org-mode prompt blocks) to v4 (comint shell).

**Context**: Field testing of v3 revealed that buffer navigation was consuming excessive cognitive overhead. The agent spent 60 ticks trying to find/navigate buffers, creating duplicates (`#agent-chat#`, `##agent-chat##`), and replies were not appearing where expected.

**Solution**: Simplify to a single comint-based shell where I/O is unambiguous - human types, presses enter, agent reply appears inline.

**Deliverables**:
1. Transition RFC (`amacs-rfc-v4-transition.md`) - focused architecture document
2. EPIC-005 with 7 IMP tickets (036-042) for core loop rewrite
3. IMP-036 implementation complete - comint shell with fake process
4. Updated CLAUDE.md to reflect v4 transition
5. Reorganized RFC directory (v3 parts archived to RFC-V3/)

**Design decisions captured via AskUserQuestion**:
- Comint with fake process (no real subprocess)
- Auto-trigger inference on enter
- JSON response format with reply/eval/scratchpad fields
- Hybrid scratchpad scoping (global + thread-specific)
- ID-first thread API
- Chat serialization to org with background persistence
- Formatted display (not raw JSON) in comint

## Session Commits

1. **c659c52** - EPIC-005: Core Loop Rewrite planning complete
   - Created amacs-rfc-v4-transition.md
   - Created AI-EPIC-005-core-loop-rewrite.md with 14 FRs
   - Created AI-IMP-036 through 042 (7 implementation tickets)
   - Updated CLAUDE.md for v4 transition
   - Archived v3 RFC parts to RAG/RFC/RFC-V3/
   - Added draft-prompt.md (system prompt design)

2. **ca6ea1c** - IMP-036: Comint shell implementation complete
   - Created harness/amacs-shell.el (164 lines)
   - Fake process using `cat` for comint satisfaction
   - Input capture via comint-input-sender override
   - Placeholder response mechanism
   - All 113 tests pass, byte-compile clean

## Issues Encountered

**None significant**. The planning session went smoothly with clear requirements gathering via AskUserQuestion. Implementation of IMP-036 had no blockers.

**Minor notes**:
- Chose `cat` as the fake process rather than more complex alternatives
- Deferred agent-core.el integration (not needed for MVP testing)
- v3 tests still pass - the new shell is additive, not replacing v3 code yet

## Tests Added

No new formal tests added to test-harness.el. However, ad-hoc verification was performed:
- Buffer creation: PASS
- Mode setup: PASS
- Prompt display: PASS
- Fake process exists: PASS
- Input capture: PASS
- Response insertion: PASS
- New prompt after response: PASS

Formal test integration can happen once more of the pipeline is wired up (IMP-037+).

## Next Steps

**Immediate (IMP-037: Basic Inference)**:
1. Wire `amacs-shell--trigger-inference` to actual API calls
2. Build minimal system prompt from core skill
3. Parse JSON response, extract `reply` field
4. Implement retry on parse error (max 2 retries)

**Files to read before continuing**:
- `RAG/RFC/amacs-rfc-v4-transition.md` - architecture reference
- `RAG/AI-IMP/AI-IMP-037-basic-inference.md` - next ticket
- `harness/amacs-shell.el` - current implementation
- `harness/agent-api.el` - existing API client to reuse

**Key integration point**: The placeholder `amacs-shell--placeholder-response` needs to be replaced with real API call → JSON parse → display flow.

**Open questions for next session**:
- Should we stream responses or show all at once?
- How to handle multi-line human input in comint?
