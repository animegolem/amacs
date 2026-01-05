---
node_id: AI-IMP-038
tags:
  - IMP-LIST
  - Implementation
  - context
  - consciousness
  - scratchpad
kanban_status: completed
depends_on:
  - AI-IMP-037
confidence_score: 0.80
created_date: 2025-01-03
close_date: 2025-01-04
---

# AI-IMP-038: Context Assembly

## Summary

Assemble full context for inference: consciousness, chat history, monologue, scratchpad, buffers, skills.

**Current state:** Minimal context (just system prompt + user message).

**Target state:** Full context sections as defined in RFC v4.

**Done when:** Agent sees complete context including chat history and can reference prior exchanges.

### Out of Scope

- Serialization to disk (IMP-039) - context reads from memory/existing files
- Eval execution (IMP-040)

### Design/Approach

Context structure (injected as user message content):
```
<agent-consciousness>
{consciousness alist}
</agent-consciousness>

<chat>
Human: {prior message 1}
Agent: {prior response 1}
...
Human: {current pending message}
</chat>

<monologue>
{last N monologue lines}
</monologue>

<scratchpad>
{global + active thread headings}
</scratchpad>

<buffers>
{active thread buffer contents}
</buffers>

<skills>
{bound skill content}
</skills>
```

Consciousness shows only relevant fields (simplified from v3).

Chat history: completed pairs from in-memory store + pending message.

Scratchpad: hybrid scoping with two depth controls (global vs thread).

### Files to Touch

- `harness/agent-context.el`: rewrite context assembly
- `harness/agent-consciousness.el`: simplify schema, add new depth fields
- `harness/agent-scratchpad.el`: add thread property support, depth filtering
- `harness/amacs-shell.el`: integrate context into inference

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**?
</CRITICAL_RULE>

- [ ] Update consciousness schema with new depth fields (deferred to full harness integration)
  - [ ] `global-scratchpad-depth` (default 5)
  - [ ] `thread-scratchpad-depth` (default 10)
- [x] Create `agent-consciousness-for-prompt` returning minimal alist (amacs-shell--format-consciousness)
- [x] Implement `agent-format-consciousness-section`
- [x] Create in-memory chat history store (list of pairs)
- [x] Implement `agent-add-chat-pair` to record completed exchanges (amacs-shell--record-exchange)
- [x] Implement `agent-format-chat-section` with depth limiting
- [x] Include pending message as final "Human:" without "Agent:"
- [x] Implement `agent-format-monologue-section`
- [ ] Update scratchpad to store `:THREAD:` property on headings (deferred - needs IMP-039)
- [ ] Implement `agent-scratchpad-global-headings` (thread: null) (deferred)
- [ ] Implement `agent-scratchpad-thread-headings` (matching thread) (deferred)
- [ ] Implement `agent-format-scratchpad-section` with both depths (deferred)
- [ ] Implement `agent-format-buffers-section` (active thread only) (deferred - needs threads)
- [ ] Implement `agent-format-skills-section` (deferred - needs skills)
- [x] Create `agent-build-full-context` combining all sections (amacs-shell--build-context)
- [x] Integrate into `amacs-shell--trigger-inference`
- [x] Test: consciousness appears in context
- [x] Test: chat history with 3+ pairs shows correctly
- [x] Test: pending message appears without response
- [ ] Test: scratchpad global vs thread filtering works (deferred)
- [x] Byte-compile without warnings

### Acceptance Criteria

**Scenario:** Full context in inference
**GIVEN** Agent has prior chat history and scratchpad notes
**WHEN** Human sends new message
**THEN** Context includes all sections with correct content
**AND** Chat shows prior pairs + pending message
**AND** Scratchpad shows global + active thread headings

**Scenario:** Depth limiting works
**GIVEN** chat-context-depth is 3 and there are 10 exchanges
**WHEN** Context is assembled
**THEN** Only last 3 completed pairs are included
**AND** Current pending message is always included

### Issues Encountered

Implementation focused on MVP for basic loop:
- Consciousness, chat history, and monologue sections implemented
- Scratchpad/buffers/skills sections deferred (need thread/skill infrastructure)
- Chat history stored in-memory (persistence in IMP-039)
- Had to reverse history list for chronological display (push creates newest-first)
- Depth limiting tested and working
