---
node_id: AI-IMP-051
tags:
  - IMP-LIST
  - Implementation
  - EPIC-007
  - skills
  - threads
kanban_status: done
depends_on:
  - AI-IMP-050
  - AI-IMP-048
confidence_score: 0.95
created_date: 2025-01-11
close_date: 2025-01-19
---

# AI-IMP-051-skill-system-activation

## Summary

Activate the skill system so skills can be bound to threads. When a thread is active, its bound skills' content is included in context. Skills are thread-bound only (no global skills except core).

**Current state**: `agent-skills.el` exists with skill loading code but is not wired to shell or inference. Functions like `agent-list-available-skills` may exist but are unused.

**Target state**:
- `(agent-list-available-skills)` returns installed skills
- `(agent-bind-skill-to-thread "skill-name")` binds skill to active thread
- Active thread's bound skills appear in `<skills>` context section

**Done when**: Agent can bind a skill, and that skill's SKILL.md content appears in context.

### Out of Scope

- Auto-binding skills by mode/buffer (v3 feature, deferred)
- Skill scripts/helpers execution
- Creating new skills (agent can do this via eval)

### Design/Approach

1. Review `agent-skills.el` for existing implementation
2. Ensure `agent-list-available-skills` scans `~/.agent/skills/` directory
3. Ensure `agent-bind-skill-to-thread` updates thread's `bound-skills` field
4. In context assembly (`agent-context.el`), include bound skill content for active thread
5. Skills format in context: `<skills>\n## skill-name\n{SKILL.md content}\n</skills>`

Thread alist already has `bound-skills` field per v3 design. Need to:
- Populate it when skill bound
- Read it during context assembly
- Load skill content from disk

### Files to Touch

`harness/agent-skills.el`: Verify/implement listing and binding
`harness/agent-threads.el`: Ensure bound-skills field in thread alist
`harness/agent-context.el`: Include skills in context assembly
`harness/agent-inference.el`: Ensure context includes skills section

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**?
</CRITICAL_RULE>

- [x] Review `agent-skills.el` current implementation (already complete from v3)
- [x] Verify `agent-list-available-skills` scans `~/.agent/skills/` subdirectories (agent-skills.el:85-96)
- [x] Each subdirectory with SKILL.md is a valid skill (confirmed)
- [x] Return list of skill names (directory names) (excludes "core")
- [x] Implement/verify `agent-bind-skill-to-thread (skill-name &optional thread-id)` (agent-skills.el:135-148)
- [x] Defaults to active thread if thread-id nil (confirmed)
- [x] Updates thread's `bound-skills` list in consciousness (via agent--update-thread)
- [x] Errors if skill doesn't exist or thread doesn't exist (tested)
- [x] Implement `agent-unbind-skill-from-thread (skill-name &optional thread-id)` (agent-skills.el:150-159)
- [x] Implement `agent-load-skill-content (skill-name)` - reads SKILL.md (agent-skills.el:100-112)
- [x] Update `agent-context.el` to include `<skills>` section (fixed line 103: use agent--load-thread-skills)
- [x] Get active thread's bound-skills list (agent--load-thread-skills)
- [x] For each bound skill, load content and format (agent-skills.el:169-183)
- [x] Core skill NOT included here (it's the system prompt) (confirmed)
- [x] Test: bind skill, verify it appears in context (test-harness.el: 3 new tests)
- [x] Run CI: `./harness/ci-check.sh` - 116/116 tests pass

### Acceptance Criteria

**Scenario:** List available skills
**GIVEN** skills exist in `~/.agent/skills/`
**WHEN** `(agent-list-available-skills)` is called
**THEN** it returns list of skill names
**AND** "core" is included (but noted as always-loaded)

**Scenario:** Bind skill to thread
**GIVEN** a thread "my-project" is active
**WHEN** `(agent-bind-skill-to-thread "javascript")` is called
**THEN** the thread's bound-skills includes "javascript"
**AND** subsequent context includes javascript skill content

**Scenario:** Skill appears in context
**GIVEN** thread has skill "rust-mode" bound
**WHEN** context is assembled for inference
**THEN** `<skills>` section contains rust-mode SKILL.md content
**AND** core skill is NOT in `<skills>` (it's the system prompt)

### Issues Encountered

**Mostly pre-implemented**: The skill system was ~95% complete from v3 development. All binding/unbinding functions existed and worked. The thread alist already had the `bound-skills` field.

**One-line bug in context assembly**: The only code change needed was in `agent-context.el` line 103. The function `agent--build-active-thread-context` was calling `agent-skills-for-context` (which uses mode/buffer-based skill resolution) instead of `agent--load-thread-skills` (which uses thread-bound skills). Changed to:
```elisp
(skills-content (agent--load-thread-skills))
```

**Tests added**: Added 3 new tests to `test-skill-binding`:
- `skill-bound-to-thread`: Verify binding adds skill to thread
- `bound-skill-loads-content`: Verify `agent--load-thread-skills` returns content
- `skill-in-context`: Verify skill content appears in `agent-build-context`
