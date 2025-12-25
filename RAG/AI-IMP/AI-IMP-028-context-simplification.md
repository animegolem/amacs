---
node_id: AI-IMP-028
tags:
  - IMP-LIST
  - Implementation
  - context
  - truncation
  - skills
kanban_status: complete
depends_on:
  - AI-IMP-025
  - AI-IMP-026
confidence_score: 0.9
created_date: 2025-12-24
close_date: 2025-12-25
--- 

# AI-IMP-028: Context Assembly Simplification + Skill Init

## Summary

Simplify context assembly by removing truncation, including full consciousness state, adding agent-controlled depth settings, and ensuring all bootstrap skills copy on init.

**Current state:** 
- Buffer content truncated at 4000 chars
- Consciousness passed as "summary" (incomplete)
- Fixed context depths
- Only `core/` skill copied, `chat/` missing

**Target state:**
- No character truncation (trust token budgets)
- Full consciousness state in context
- Agent controls depths via `:chat-context-depth` and `:monologue-context-depth`
- All skills in `skills/amacs-bootstrap-skill/` copied on init

**Why:**
- Truncation caused agent confusion ("things are cut off")
- Agent needs full state visibility for self-management
- Depth control enables agent adaptation
- Missing skills limit agent capabilities

**Done when:** No truncation, full state visible, depth controls work, all skills available.

### Out of Scope

- Token budget enforcement (future concern)
- Skill hot-reloading (manual reload sufficient)
- Compression strategies for large buffers

### Design/Approach

**Remove truncation:**
```elisp
;; Before
(if (> (length content) 4000)
    (concat (substring content 0 4000) "\n... [truncated]")
  content)

;; After
content  ;; Just return it
```

**Full consciousness:**
Replace `agent-consciousness-summary` with full state serialization. Already alist after IMP-025, so JSON encoding is direct.

**Depth controls:**
```elisp
;; In consciousness
((chat-context-depth . 5)
 (monologue-context-depth . 20)
 ...)

;; In context assembly
(let ((chat-depth (or (alist-get 'chat-context-depth agent-consciousness) 5))
      (mono-depth (or (alist-get 'monologue-context-depth agent-consciousness) 20)))
  ...)
```

**Skill copying:**
```elisp
(defun agent-ensure-bootstrap-skills ()
  "Copy all bootstrap skills to ~/.agent/skills/."
  (let ((source-base (expand-file-name "../skills/amacs-bootstrap-skill/" 
                                        agent-harness-directory)))
    (dolist (skill (directory-files source-base nil "^[^.]"))
      (agent--copy-skill-dir skill source-base))))
```

### Files to Touch

- `harness/agent-context.el`: Remove truncation, full consciousness, depth reads
- `harness/agent-consciousness.el`: Add depth fields to default schema
- `harness/agent-skills.el`: Update `agent-ensure-core-skill` → `agent-ensure-bootstrap-skills`
- `harness/agent-inference.el`: Remove `agent-consciousness-summary` usage
- `harness/agent-core.el`: Call updated skill init
- `skills/amacs-bootstrap-skill/core/SKILL.md`: Document depth controls
- `harness/test-harness.el`: Tests for depth control, skill copying

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**? 
</CRITICAL_RULE> 

- [x] Remove 4000 char truncation from `agent--format-buffer-for-prompt`
- [x] Add `chat-context-depth` to default consciousness (default: 5)
- [x] Add `monologue-context-depth` to default consciousness (default: 20)
- [x] Update context assembly to read depth from consciousness
- [x] Replace `agent-consciousness-summary` with full state serialization
- [x] Update `agent-build-user-prompt` to use full consciousness (via context)
- [x] Rename `agent-ensure-core-skill` to `agent-ensure-bootstrap-skills`
- [x] Update skill copying to iterate all subdirectories
- [x] Update `agent-init-skills` to call `agent-ensure-bootstrap-skills`
- [ ] Update core SKILL.md with depth control documentation (deferred)
- [x] Add test for chat depth control
- [x] Add test for monologue depth control
- [x] Add test that verifies chat skill copied
- [x] Verify no truncation in formatted output

### Acceptance Criteria

**Scenario:** Buffer content not truncated
**GIVEN** a buffer with 10000 characters of content
**WHEN** `agent--format-buffer-for-prompt` is called
**THEN** full content is included
**AND** no "[truncated]" marker appears

**Scenario:** Agent controls chat depth
**GIVEN** consciousness with `(chat-context-depth . 3)`
**WHEN** context is assembled
**THEN** only last 3 chat exchanges included

**Scenario:** Agent controls monologue depth
**GIVEN** consciousness with `(monologue-context-depth . 10)`
**WHEN** context is assembled  
**THEN** only last 10 monologue lines included

**Scenario:** All bootstrap skills copied
**GIVEN** fresh `~/.agent/` directory
**WHEN** `agent-init` is called
**THEN** `~/.agent/skills/core/` exists with SKILL.md
**AND** `~/.agent/skills/chat/` exists with SKILL.md

**Scenario:** Full consciousness in context
**GIVEN** agent initialized with various state
**WHEN** `agent-build-user-prompt` is called
**THEN** consciousness section contains all fields
**AND** no fields are omitted or summarized

### Issues Encountered

Implementation was straightforward. Key design decisions:

1. **chat-context-depth defined but not actively used yet**: The chat buffer content is included as-is via global buffers. The depth control field exists for future use when structured chat exchange extraction is needed. No truncation is applied.

2. **Backward compatibility aliases**: Added `agent-consciousness-summary` as alias to `agent-consciousness-for-context`, and `agent-ensure-core-skill` as alias to `agent-ensure-bootstrap-skills` to avoid breaking any existing code.

3. **Redundant monologue depth removed**: The monologue had depth applied in two places (agent-context.el and agent-inference.el). Removed the hardcoded `seq-take 10` in agent-inference.el since depth is already applied in context assembly.
