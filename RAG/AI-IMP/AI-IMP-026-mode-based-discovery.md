---
node_id: AI-IMP-026
tags:
  - IMP-LIST
  - Implementation
  - buffers
  - modes
  - scratchpad
kanban_status: planned
depends_on: 
  - AI-IMP-025
confidence_score: 0.9
created_date: 2025-12-24
close_date:
--- 

# AI-IMP-026: Mode-Based Buffer Discovery + Scratchpad

## Summary

Replace hardcoded buffer names with mode-based discovery and add scratchpad mode for agent working notes.

**Current state:** `agent-get-global-buffers` returns hardcoded `("*agent-chat*")` which doesn't match actual buffer names, causing discovery failures.

**Target state:** Buffers discovered by checking for `amacs-chat-mode` or `agent-scratchpad-mode`. No hardcoded names.

**Why:**
- Buffer naming is fragile (file-visiting buffers named differently)
- Mode is the semantic marker for purpose
- Scratchpad gives agent persistent working memory
- Multiple scratchpads possible if agent creates them

**Done when:** Chat and scratchpad buffers discovered by mode, default scratchpad created on init, tests verify discovery.

### Out of Scope

- Thread-bound buffers (still explicitly tracked per thread)
- Auto-save hooks for scratchpad (uses standard buffer save)
- Multiple chat buffers (one chat is sufficient)

### Design/Approach

**Mode-based discovery:**
```elisp
(defun agent-find-buffers-by-mode (mode)
  "Find all buffers with MODE enabled."
  (seq-filter (lambda (buf)
                (buffer-local-value mode buf))
              (buffer-list)))

(defun agent-get-global-buffers ()
  "Return list of global buffer names (chat + scratchpad)."
  (mapcar #'buffer-name
          (append (agent-find-buffers-by-mode 'amacs-chat-mode)
                  (agent-find-buffers-by-mode 'agent-scratchpad-mode))))
```

**Scratchpad mode:**
```elisp
(define-minor-mode agent-scratchpad-mode
  "Minor mode for AMACS scratchpad buffers."
  :lighter " Scratch"
  :keymap nil)
```

**Default scratchpad:**
- Created at `~/.agent/scratchpad.org` on init
- Visits file with `agent-scratchpad-mode` enabled
- Basic org structure with guidance header

### Files to Touch

- `harness/agent-scratchpad.el`: New file - mode definition, creation, discovery
- `harness/agent-context.el`: Replace `agent-get-global-buffers` with mode-based
- `harness/agent-core.el`: Add scratchpad init to `agent-init`
- `harness/agent-chat.el`: Ensure mode enables on buffer creation
- `skills/amacs-bootstrap-skill/core/SKILL.md`: Document scratchpad usage
- `harness/test-harness.el`: Tests for mode discovery

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**? 
</CRITICAL_RULE> 

- [ ] Create `harness/agent-scratchpad.el` with mode definition
- [ ] Add `agent-find-buffers-by-mode` helper function
- [ ] Add `agent-create-scratchpad` function
- [ ] Add `agent-ensure-scratchpad` for init
- [ ] Update `agent-get-global-buffers` to use mode discovery
- [ ] Update `agent-init` to call scratchpad initialization
- [ ] Update `agent-create-chat-buffer` to ensure mode is set
- [ ] Add require for agent-scratchpad in agent-core
- [ ] Update core SKILL.md with scratchpad documentation
- [ ] Add test for mode-based chat discovery
- [ ] Add test for mode-based scratchpad discovery
- [ ] Add test for scratchpad creation on init
- [ ] Verify chat buffer found regardless of name

### Acceptance Criteria

**Scenario:** Chat buffer discovered by mode
**GIVEN** a buffer exists with `amacs-chat-mode` enabled
**WHEN** `agent-get-global-buffers` is called
**THEN** the buffer name is in the returned list
**AND** it works regardless of the buffer's actual name

**Scenario:** Scratchpad created on init
**GIVEN** no existing `~/.agent/scratchpad.org`
**WHEN** `agent-init` is called
**THEN** `~/.agent/scratchpad.org` exists
**AND** a buffer visits it with `agent-scratchpad-mode` enabled
**AND** buffer is discovered by `agent-get-global-buffers`

**Scenario:** Multiple scratchpads discovered
**GIVEN** agent creates additional scratchpad buffer
**WHEN** `agent-get-global-buffers` is called
**THEN** all scratchpad buffers are included

### Issues Encountered

<!-- Fill during implementation -->
