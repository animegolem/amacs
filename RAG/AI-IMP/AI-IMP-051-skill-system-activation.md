---
node_id: AI-IMP-051
tags:
  - IMP-LIST
  - Implementation
  - EPIC-007
  - skills
  - threads
kanban_status: backlog
depends_on:
  - AI-IMP-050
  - AI-IMP-048
confidence_score: 0.7
created_date: 2025-01-11
close_date:
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

- [ ] Review `agent-skills.el` current implementation
- [ ] Verify `agent-list-available-skills` scans `~/.agent/skills/` subdirectories
- [ ] Each subdirectory with SKILL.md is a valid skill
- [ ] Return list of skill names (directory names)
- [ ] Implement/verify `agent-bind-skill-to-thread (skill-name &optional thread-id)`
- [ ] Defaults to active thread if thread-id nil
- [ ] Updates thread's `bound-skills` list in consciousness
- [ ] Errors if skill doesn't exist or thread doesn't exist
- [ ] Implement `agent-unbind-skill-from-thread (skill-name &optional thread-id)`
- [ ] Implement `agent-load-skill-content (skill-name)` - reads SKILL.md
- [ ] Update `agent-context.el` to include `<skills>` section
- [ ] Get active thread's bound-skills list
- [ ] For each bound skill, load content and format
- [ ] Core skill NOT included here (it's the system prompt)
- [ ] Test: bind skill, verify it appears in context
- [ ] Run CI: `./harness/ci-check.sh`

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

<!-- This section filled during implementation -->
