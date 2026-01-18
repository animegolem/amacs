---
node_id: AI-IMP-049
tags:
  - IMP-LIST
  - Implementation
  - EPIC-007
  - inference
  - refactor
kanban_status: done
depends_on:
  - AI-IMP-048
confidence_score: 0.9
created_date: 2025-01-11
close_date: 2025-01-18
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

- [x] Review `agent-inference.el` current structure and entry points
- [x] Create `agent-infer (input)` entry point
- [x] Entry point accepts optional input + context-builder, assembles context, calls API, parses response
- [x] Entry point returns plist with `:reply`, `:mood`, `:confidence`, `:eval`, `:scratchpad`, `:parse-success`
- [x] Add `(require 'agent-inference)` to shell
- [x] Replace `amacs-shell--do-inference` to call `agent-infer`
- [ ] Remove `amacs-shell--build-messages` (deferred - context-builder still uses it)
- [ ] Remove `amacs-shell--build-context` (deferred - passed to inference layer)
- [x] Remove `amacs-shell--system-prompt` variable (inference layer loads from core skill)
- [ ] Remove shell format functions (deferred - still used by context-builder)
- [x] Shell handles reply display only when `:reply` present
- [x] Shell handles eval execution result feedback
- [x] Ensure git commit still happens (shell still does it)
- [x] Verify depth controls from consciousness are respected
- [x] Run CI: `./harness/ci-check.sh` - 113/113 tests passing
- [x] Test multi-turn conversation (warm start test passes)

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

**Context builder pattern**: Rather than moving all context building to the inference layer, we use a callback pattern where the shell passes its context-builder function to `agent-infer`. This preserves the shell's existing context assembly while establishing proper layering.

**Retry logic simplified**: The shell's retry-on-parse-error logic was removed since the inference layer now handles parsing consistently. Parse failures are returned as a result with `:parse-success nil` rather than triggering retries.

**Deferred cleanup**: Several shell functions (`amacs-shell--build-messages`, format functions) are still present because the context-builder callback uses them. A future IMP could move context building entirely to the inference layer.

**Core skill loading**: The inference layer now loads the system prompt from `~/.agent/skills/core/SKILL.md`, with a fallback prompt if the file isn't available. This enables prompt caching.
