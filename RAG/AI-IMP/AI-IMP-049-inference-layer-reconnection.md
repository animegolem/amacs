---
node_id: AI-IMP-049
tags:
  - IMP-LIST
  - Implementation
  - EPIC-007
  - inference
  - refactor
kanban_status: backlog
depends_on:
  - AI-IMP-048
confidence_score: 0.75
created_date: 2025-01-11
close_date:
---

# AI-IMP-049-inference-layer-reconnection

## Summary

The v4 shell builds messages and calls `agent-api-call` directly with a hardcoded system prompt. This IMP reconnects the shell to the proper inference layer (`agent-inference.el`), which handles context assembly, skill loading, and API communication.

**Current state**: Shell has `amacs-shell--build-messages`, `amacs-shell--build-context`, `amacs-shell--system-prompt`, calls `agent-api-call` directly.

**Target state**: Shell calls `agent-tick` or `agent-infer` from `agent-inference.el`. Shell only handles UI (display reply, capture input).

**Done when**: Shell triggers inference via proper layer. No message building or API calls in shell code.

### Out of Scope

- Async API calls (future EPIC)
- Streaming responses (future)
- The specific system prompt content (IMP-050)

### Design/Approach

1. Review `agent-inference.el` to understand current API
2. Create or adapt an entry point that:
   - Takes optional human input (for chat)
   - Triggers full inference cycle
   - Returns response for shell to display
3. Remove `amacs-shell--build-messages`, `amacs-shell--build-context`, `amacs-shell--system-prompt`
4. Shell's `amacs-shell--do-inference` becomes thin wrapper around `agent-infer`
5. Handle reply-optional case: if no reply in response, shell stays quiet

Key functions in agent-inference.el:
- `agent-format-messages` - builds messages array
- `agent-build-system-prompt` - loads core skill
- `agent-build-user-prompt` - assembles context
- Main entry likely needs adaptation for shell's needs

May need new function: `agent-tick-with-input` that accepts human prompt.

### Files to Touch

`harness/amacs-shell.el`: Remove inline inference, call agent-inference
`harness/agent-inference.el`: Adapt/create entry point for shell use
`harness/agent-context.el`: May need updates for chat context assembly

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**?
</CRITICAL_RULE>

- [ ] Review `agent-inference.el` current structure and entry points
- [ ] Create `agent-tick-with-input (input)` or similar entry point
- [ ] Entry point should: accept optional input, assemble context, call API, process response
- [ ] Entry point returns plist with `:reply`, `:mood`, `:confidence`, `:eval`, etc.
- [ ] Add `(require 'agent-inference)` to shell
- [ ] Replace `amacs-shell--do-inference` to call new entry point
- [ ] Remove `amacs-shell--build-messages`
- [ ] Remove `amacs-shell--build-context`
- [ ] Remove `amacs-shell--system-prompt` variable
- [ ] Remove `amacs-shell--format-consciousness` (moved to agent-context)
- [ ] Remove `amacs-shell--format-chat-history` (moved to agent-context)
- [ ] Remove `amacs-shell--format-monologue` (moved to agent-context)
- [ ] Remove `amacs-shell--format-scratchpad` (moved to agent-context)
- [ ] Shell handles reply display only when `:reply` present
- [ ] Shell handles eval execution result feedback
- [ ] Ensure git commit still happens (in inference layer)
- [ ] Verify depth controls from consciousness are respected (chat-context-depth, monologue-context-depth, scratchpad depths)
- [ ] Run CI: `./harness/ci-check.sh`
- [ ] Test multi-turn conversation

### Acceptance Criteria

**Scenario:** Shell triggers inference via proper layer
**GIVEN** the shell is active
**WHEN** user sends a message
**THEN** `agent-inference.el` handles context assembly
**AND** `agent-inference.el` makes the API call
**AND** shell only displays the reply (if present)
**AND** no `amacs-shell--build-messages` function exists

**Scenario:** Reply-optional works
**GIVEN** the shell is active
**WHEN** agent responds with no `reply` field (autonomous work)
**THEN** shell does not display anything
**AND** state updates still occur (mood, scratchpad, etc.)
**AND** hub shows the tick completed

### Issues Encountered

<!-- This section filled during implementation -->
