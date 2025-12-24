---
node_id: AI-IMP-027
tags:
  - IMP-LIST
  - Implementation
  - chat
  - org-mode
  - interface
kanban_status: planned
depends_on: 
  - AI-IMP-026
confidence_score: 0.85
created_date: 2025-12-24
close_date:
--- 

# AI-IMP-027: Chat Interface Redesign

## Summary

Redesign chat structure using `#+begin_prompt`/`#+end_prompt` blocks for human input, with agent response function transforming into tick-based org headings.

**Current state:** Chat uses `* Human Input` / `* Agent Response` headings. Human must create heading. Buffer/file sync issues cause message loss.

**Target state:** Human wraps input in prompt block. Agent response transforms block into structured `* Tick N` heading with `** Human Prompt` and `** Agent Response` subheadings.

**Why:**
- Human only deals with simple block syntax
- Tick numbers applied when agent responds (accurate timing)
- Structure emerges from response, not human action
- Clear parsing boundaries via org blocks

**Done when:** Human can type in prompt blocks, agent response transforms to tick structure, parsing extracts exchanges correctly.

### Out of Scope

- Multiple concurrent prompt blocks (one at a time)
- Rich metadata beyond tick/timestamp
- Chat history search/filtering (future)

### Design/Approach

**Human input:**
```org
#+begin_prompt
Human types here freely
with whatever **formatting** they want
- lists work
- etc
#+end_prompt
```

**After agent responds:**
```org
* Tick 43
:PROPERTIES:
:COMPLETED: 2025-12-24T09:00:00Z
:END:

** Human Prompt
#+begin_prompt
Human types here freely
with whatever **formatting** they want
- lists work
- etc
#+end_prompt

** Agent Response
Agent's response text here
```

**Keybinding:** `<h TAB` expands to prompt block template (org-tempo style).

**Response function:**
```elisp
(defun agent-chat-respond (response-text)
  "Transform pending prompt into tick structure and add response."
  ;; 1. Find #+begin_prompt ... #+end_prompt
  ;; 2. Wrap in * Tick N heading with properties
  ;; 3. Add ** Human Prompt subheading (keeping block)
  ;; 4. Add ** Agent Response with response-text
  ;; 5. Save buffer
  )
```

**Parsing:** Extract exchanges by finding `* Tick N` headings, then `** Human Prompt` and `** Agent Response` subheadings.

### Files to Touch

- `harness/agent-chat.el`: Complete rewrite of structure
- `skills/amacs-bootstrap-skill/chat/SKILL.md`: Update documentation
- `skills/amacs-bootstrap-skill/core/SKILL.md`: Update chat section
- `harness/test-harness.el`: Chat parsing tests

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**? 
</CRITICAL_RULE> 

- [ ] Add prompt block template to org-tempo or custom expansion
- [ ] Implement `agent-chat-find-pending-prompt` to locate block
- [ ] Implement `agent-chat-respond` transformation function
- [ ] Implement `agent-chat-read-exchanges` to parse tick headings
- [ ] Update `agent-chat-last-human-input` for new structure
- [ ] Update `amacs-chat-send` (C-c C-c) for new flow
- [ ] Update `agent-create-chat-buffer` initial content
- [ ] Remove old `* Human Input` / `* Agent Response` parsing
- [ ] Update chat SKILL.md documentation
- [ ] Update core SKILL.md chat section
- [ ] Add test for prompt block parsing
- [ ] Add test for response transformation
- [ ] Add test for exchange reading
- [ ] Test with multi-line formatted human input

### Acceptance Criteria

**Scenario:** Human prompt captured correctly
**GIVEN** chat buffer with prompt block containing formatted text
**WHEN** `agent-chat-find-pending-prompt` is called
**THEN** returns the full content between begin/end markers
**AND** formatting is preserved

**Scenario:** Response transforms structure
**GIVEN** chat buffer with pending prompt block
**WHEN** `agent-chat-respond "test response"` is called
**THEN** prompt block wrapped in `* Tick N` heading
**AND** heading has `:COMPLETED:` property with timestamp
**AND** `** Human Prompt` contains original block
**AND** `** Agent Response` contains response text
**AND** buffer is saved

**Scenario:** Exchanges parsed correctly
**GIVEN** chat buffer with multiple tick headings
**WHEN** `agent-chat-read-exchanges 3` is called
**THEN** returns last 3 human/agent pairs
**AND** each pair has correct content extracted

**Scenario:** Expansion shortcut works
**GIVEN** user in chat buffer types `<h` then TAB
**THEN** expands to `#+begin_prompt` / `#+end_prompt` with cursor inside

### Issues Encountered

<!-- Fill during implementation -->
