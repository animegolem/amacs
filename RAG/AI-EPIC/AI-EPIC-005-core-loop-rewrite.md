---
node_id: AI-EPIC-005
tags:
  - EPIC
  - AI
  - core-loop
  - comint
  - rewrite
date_created: 2025-01-03
date_completed:
kanban-status: planned
AI_IMP_spawned:
  - AI-IMP-036
  - AI-IMP-037
  - AI-IMP-038
  - AI-IMP-039
  - AI-IMP-040
  - AI-IMP-041
  - AI-IMP-042
---

# AI-EPIC-005-core-loop-rewrite

## Problem Statement/Feature Scope

The v3 architecture using org-mode prompt blocks and file buffer navigation proved too complex during field testing. The agent spent 60 ticks navigating buffers, creating duplicate buffer names (`#agent-chat#`, `##agent-chat##`), and replies were not appearing where expected. The cognitive overhead of "where do I write" overwhelmed "what do I write."

We need a simpler, more obvious I/O model where the interaction channel is unambiguous.

## Proposed Solution(s)

Replace the org-mode prompt block system with a comint-based shell interface:

1. **Single comint buffer** (`*amacs-shell*`) - Human types, presses enter, agent reply appears inline
2. **Fake process model** - No subprocess; intercept via `comint-input-sender`, all elisp
3. **Background serialization** - Chat exchanges stored in `agent-chat.org` for persistence and context feeding
4. **Simplified consciousness** - Only agent-controllable fields exposed prominently
5. **JSON response format** - Structured output with `eval`, `reply`, `mood`, `confidence`, `monologue`, `scratchpad`

See: `RAG/RFC/amacs-rfc-v4-transition.md` for full architecture.

## Path(s) Not Taken

- **Keep org-mode blocks**: Too much buffer navigation complexity
- **Real subprocess**: Adds moving parts; fake process sufficient for MVP
- **Streaming responses**: Deferred; get basic loop working first
- **Hub dashboard**: Deferred to EPIC-006
- **Async optimization**: Deferred to EPIC-007

## Success Metrics

1. **Round-trip works**: Human sends message -> agent sees it -> agent replies -> human sees reply in comint
2. **Multi-turn stable**: 10+ exchanges without buffer confusion or lost messages
3. **Eval functional**: Agent can eval elisp, see results next tick, adjust behavior
4. **Persistence works**: Restart Emacs, chat history and scratchpad preserved
5. **Git commits**: Every tick produces a commit with correct format

**Definition of Done**: A human can have a multi-turn conversation with the agent, the agent can create threads, take notes, and execute elisp, all without the buffer navigation issues seen in v3 testing.

## Requirements

### Functional Requirements

- [ ] FR-1: Comint buffer accepts human input and displays agent replies
- [ ] FR-2: Pressing enter triggers inference automatically
- [ ] FR-3: Agent responses are parsed as JSON with error retry
- [ ] FR-4: Reply field content is formatted and displayed in comint
- [ ] FR-5: Consciousness state is assembled and sent as context
- [ ] FR-6: Chat history (completed pairs) is serialized to agent-chat.org
- [ ] FR-7: Pending message (current input) appears in context as K with no V
- [ ] FR-8: Scratchpad supports global and thread-scoped headings
- [ ] FR-9: Scratchpad JSON field appends content under specified heading
- [ ] FR-10: Eval field executes elisp, result appears in last-eval-result
- [ ] FR-11: Thread create/switch/complete APIs work via eval
- [ ] FR-12: Git commit occurs each tick with correct format
- [ ] FR-13: System prompt is the core skill content
- [ ] FR-14: Buffer attachment to threads works via eval

### Non-Functional Requirements

- NFR-1: All elisp byte-compiles without warnings
- NFR-2: Test suite passes (new tests for new architecture)
- NFR-3: Cold start initializes clean state correctly
- NFR-4: Warm start preserves state from disk
- NFR-5: API errors are handled gracefully (shown to user, logged)

## Implementation Breakdown

| IMP | Title | Status | Description |
|-----|-------|--------|-------------|
| AI-IMP-036 | Comint Shell | **complete** | Fake process comint buffer with input capture |
| AI-IMP-037 | Basic Inference | **complete** | API call, JSON parse, display reply |
| AI-IMP-038 | Context Assembly | planned | Consciousness, chat, scratchpad sections |
| AI-IMP-039 | Serialization | planned | Chat.org and scratchpad.org persistence |
| AI-IMP-040 | Eval Execution | planned | Eval field processing and result feedback |
| AI-IMP-041 | Thread Management | planned | Create/switch/complete with new ID-first API |
| AI-IMP-042 | Git Integration | planned | Per-tick commits, format preservation |
