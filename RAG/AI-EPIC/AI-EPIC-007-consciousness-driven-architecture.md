---
node_id: AI-EPIC-007
tags:
  - EPIC
  - AI
  - architecture
  - consciousness
  - refactor
date_created: 2025-01-11
date_completed:
kanban-status: planning
AI_IMP_spawned:
  - AI-IMP-048
  - AI-IMP-049
  - AI-IMP-050
  - AI-IMP-051
  - AI-IMP-052
  - AI-IMP-053
  - AI-IMP-054
---

# AI-EPIC-007-consciousness-driven-architecture

## Problem Statement/Feature Scope

The v4 shell (EPIC-005) solved the buffer navigation problem but accidentally reimplemented the entire inference chain inline. The shell now has its own state variables, tick counter, context builder, and hardcoded system prompt - bypassing the consciousness alist and skill system entirely.

This creates a "chatbot in a buffer" rather than an "agent native to Emacs." The distinction matters: if state lives in a chat buffer, there's no advantage over using Claude Code directly. The value proposition of AMACS is an agent whose state IS the Emacs environment - consciousness as an alist that drives context, skills that modulate capabilities, and eval as motor control.

The current implementation cannot:
- Load skills (skill system not wired to shell)
- Inject buffer contents from threads (buffers tracked but not hydrated)
- Expose agent-controllable settings (api-settings, depth controls)
- Leverage prompt caching (prompt rebuilt from scratch each tick)

## Proposed Solution(s)

Refactor to a clean separation of concerns:

```
                    Tick Triggered (human input OR autonomous)
                                    │
                                    v
                           ┌─────────────────┐
                           │ Inference Layer │
                           └────────┬────────┘
                                    │
        ┌───────────────────────────┼───────────────────────────┐
        │                           │                           │
        v                           v                           v
┌───────────────┐         ┌─────────────────┐         ┌─────────────────┐
│ Consciousness │         │   Core Skill    │         │  Bound Skills   │
│    (alist)    │         │ (system prompt) │         │ (per thread)    │
└───────────────┘         └─────────────────┘         └─────────────────┘
        │                           │                           │
        └───────────────────────────┴───────────────────────────┘
                                    │
                                    v
                            Context Assembly:
                            - Full alist state
                            - Unreplied prompt (if any)
                            - Last N chat K:V pairs
                            - Active thread buffers
                            - Bound skill content
                                    │
                                    v
                              API Call
                                    │
                                    v
                           JSON Response
                    ┌───────────────┴───────────────┐
                    │                               │
                    v                               v
            IF reply present              State updates always:
            → Shell displays it           - mood, confidence
                                          - scratchpad
            IF reply absent               - eval execution
            → Agent worked silently       - monologue → git commit
                                          - thread changes
                                    │
                                    v
                            Hub refreshes
                      (observability for both)
```

### Key Concepts

1. **Shell becomes pure UI**: Captures human input, displays agent output when `reply` present. No state management, no context building, no prompt.

2. **Reply is optional**: Agent can run ticks without replying to human. Autonomous work updates scratchpad, threads, buffers. Human observes via hub.

3. **Consciousness alist is source of truth**: Tick counter, mood, confidence, threads, eval results - all live in the alist, not shell variables.

4. **Core skill = system prompt**: Load from `~/.agent/skills/core/SKILL.md`, leverage prompt caching. Always loaded.

5. **Skills bound to threads**: `agent-bind-skill-to-thread` associates skills. Skill content loads when thread is active. No global skills except core.

6. **Context model**: Agent sees:
   - Full consciousness alist
   - Unreplied human prompt (if any)
   - Last N chat K:V pairs from org file
   - Active thread's buffer contents
   - Bound skills for active thread

7. **Buffer hydration works**: Thread's buffer list actually injects buffer contents into `<buffers>` section.

## Path(s) Not Taken

- **Rewrite from scratch**: Preserve working code where possible, refactor incrementally
- **Async optimization**: Separate concern (EPIC-008), keep blocking for now
- **CRDT/multi-process**: Future consideration per RFC, not this epic
- **Budget/pressure system**: Deferred, adds complexity without immediate value

## Success Metrics

1. **System prompt from skill**: `(agent-build-system-prompt)` loads core skill, not hardcoded string
2. **State in consciousness**: Shell has no state variables except UI state (buffer, process)
3. **Skills bind and load**: Create thread, bind skill, skill content appears in context
4. **Buffers hydrate**: Add buffer to thread, buffer content appears in `<buffers>` section
5. **Draft prompt accurate**: `draft-prompt.md` matches actual system behavior
6. **Tests pass**: All existing tests still pass after refactor
7. **Multi-turn stable**: 10+ exchanges work correctly with new architecture
8. **Autonomous ticks**: Agent can run N ticks without human prompt, updating state silently
9. **Hub observability**: Human can observe agent progress via hub without shell interaction

## Requirements

### Functional Requirements

- [ ] FR-1: Shell shall trigger inference via `agent-tick` or equivalent, not inline API calls
- [ ] FR-2: Shell shall display replies only when `reply` field present (reply is optional)
- [ ] FR-3: Consciousness alist shall be single source of truth for all agent state
- [ ] FR-4: System prompt shall load from core skill file (`SKILL.md`)
- [ ] FR-5: Context assembly shall read depth settings from consciousness alist
- [ ] FR-6: Active thread's buffers shall be hydrated into `<buffers>` context section
- [ ] FR-7: Bound skills shall be included in context when their thread is active
- [ ] FR-8: `agent-list-available-skills` shall return installed skills
- [ ] FR-9: `agent-bind-skill-to-thread` shall associate skill with thread
- [ ] FR-10: Draft prompt (`draft-prompt.md`) shall be updated to match implementation
- [ ] FR-11: Agent shall be able to run ticks autonomously without human prompt
- [ ] FR-12: Context shall include unreplied prompt + last N chat K:V pairs from org file
- [ ] FR-13: Skills shall be thread-bound only (no global skills except core)

### Non-Functional Requirements

- NFR-1: Prompt caching compatibility - system prompt should be stable across ticks to enable caching
- NFR-2: Context size awareness - respect token limits, truncate appropriately
- NFR-3: Error messages shall indicate which layer failed (UI, inference, API)
- NFR-4: Backward compatibility - existing chat history and scratchpad files shall load correctly

## Implementation Breakdown

### Dependency Order

```
IMP-048 (State Migration)
    │
    └──> IMP-049 (Inference Reconnection)
              │
              └──> IMP-050 (Core Skill Loading)
                        │
                        ├──> IMP-051 (Skill System)
                        │
                        └──> IMP-052 (Buffer Hydration)
                                  │
                                  └──> IMP-053 (Draft Prompt Update)
                                            │
                                            └──> IMP-054 (Test Suite)
```

### IMPs

| IMP | Title | Status | Dependencies |
|-----|-------|--------|--------------|
| [AI-IMP-048](../AI-IMP/AI-IMP-048-consciousness-state-migration.md) | Consciousness State Migration | backlog | none |
| [AI-IMP-049](../AI-IMP/AI-IMP-049-inference-layer-reconnection.md) | Inference Layer Reconnection | backlog | IMP-048 |
| [AI-IMP-050](../AI-IMP/AI-IMP-050-core-skill-loading.md) | Core Skill Loading | backlog | IMP-049 |
| [AI-IMP-051](../AI-IMP/AI-IMP-051-skill-system-activation.md) | Skill System Activation | backlog | IMP-048, IMP-050 |
| [AI-IMP-052](../AI-IMP/AI-IMP-052-buffer-hydration.md) | Buffer Hydration | backlog | IMP-048, IMP-049 |
| [AI-IMP-053](../AI-IMP/AI-IMP-053-draft-prompt-update.md) | Draft Prompt Update | backlog | IMP-048-052 |
| [AI-IMP-054](../AI-IMP/AI-IMP-054-test-suite-update.md) | Test Suite Update | backlog | IMP-048-052 |

## References

- `RAG/RFC/amacs-rfc-v4-transition.md` - Current architecture doc
- `RAG/RFC/amacs-rfc-concurrency.md` - Async considerations (separate epic)
- `RAG/draft-prompt.md` - Target system prompt content
- `harness/agent-inference.el` - Existing inference layer (v3)
- `harness/amacs-shell.el` - Current shell (needs refactor)
