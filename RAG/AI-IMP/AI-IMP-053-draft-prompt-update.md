---
node_id: AI-IMP-053
tags:
  - IMP-LIST
  - Implementation
  - EPIC-007
  - documentation
  - system-prompt
kanban_status: backlog
depends_on:
  - AI-IMP-048
  - AI-IMP-049
  - AI-IMP-050
  - AI-IMP-051
  - AI-IMP-052
confidence_score: 0.9
created_date: 2025-01-11
close_date:
---

# AI-IMP-053-draft-prompt-update

## Summary

Update `RAG/draft-prompt.md` to match the implemented system. This becomes the source for the core skill (`~/.agent/skills/core/SKILL.md`). Must accurately reflect the v4 architecture post-EPIC-007.

**Current state**: Draft prompt documents features that don't exist (budget, api-settings) and misses implemented features.

**Target state**: Draft prompt accurately documents:
- Response format (JSON with optional fields)
- State schema (full alist, what agent can read/modify)
- Thread API (ID-first, bind skills)
- Skills system (thread-bound only)
- Context structure (consciousness, chat, monologue, scratchpad, buffers, skills)

**Done when**: Draft prompt matches implementation. Copy to SKILL.md produces working system prompt.

### Out of Scope

- Budget/pressure system (deferred)
- API settings exposure (deferred for v2)
- Reference documents in skill (separate task)

### Design/Approach

Based on earlier gap analysis, update draft-prompt.md:

**Remove/defer:**
- Budget section (not implemented)
- API settings (temperature, etc.) - deferred
- `identity` and `current-time` fields (not exposed)

**Update:**
- State schema to match actual consciousness alist
- Thread API to match v4 implementation (ID-first)
- Skills section: thread-bound only, core always loaded
- Context sections: add `<threads>`, `<buffers>`
- Response format: clarify `reply` is optional

**Add:**
- Autonomous operation (reply optional)
- Hub observability mention
- Updated file paths (shell vs org buffers)

### Files to Touch

`RAG/draft-prompt.md`: Main update
`skills/amacs-bootstrap-skill/core/SKILL.md`: Copy updated content

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**?
</CRITICAL_RULE>

- [ ] Review current draft-prompt.md against implementation
- [ ] Update introduction to reflect v4 shell architecture
- [ ] Update state schema section:
  - [ ] Remove budget, api-settings, identity, current-time
  - [ ] Show actual alist structure from agent-consciousness.el
  - [ ] Document which fields agent can modify vs read-only
- [ ] Update response format section:
  - [ ] Clarify `reply` is optional (autonomous work)
  - [ ] Ensure all fields documented with correct types
  - [ ] Remove `thought` field if not used
- [ ] Update thread API section:
  - [ ] ID-first: `(agent-create-thread ID &key concern buffers)`
  - [ ] Document `agent-thread-add-buffer`, `agent-thread-remove-buffer`
  - [ ] Document `agent-list-threads`
- [ ] Update skills section:
  - [ ] Skills bound to threads only (no global except core)
  - [ ] `agent-list-available-skills`, `agent-bind-skill-to-thread`
  - [ ] Core skill always loaded, not listed
- [ ] Update context structure section:
  - [ ] Add `<threads>` section documentation
  - [ ] Add `<buffers>` section documentation
  - [ ] Show full alist in `<agent-consciousness>`
- [ ] Add section on autonomous operation:
  - [ ] Agent can run ticks without human prompt
  - [ ] Reply optional - hub provides observability
- [ ] Update file paths (shell is `*amacs-shell*`, org files in ~/.agent/)
- [ ] Update "First Boot" section for v4
- [ ] Copy final content to `skills/amacs-bootstrap-skill/core/SKILL.md`
- [ ] Test: agent receives prompt, responds correctly
- [ ] Run CI: `./harness/ci-check.sh`

### Acceptance Criteria

**Scenario:** Prompt accurately describes system
**GIVEN** draft-prompt.md is updated
**WHEN** an AI reads the prompt and interacts with the system
**THEN** all documented features work as described
**AND** no documented features are missing from implementation

**Scenario:** Prompt becomes core skill
**GIVEN** draft-prompt.md content is copied to SKILL.md
**WHEN** agent is initialized and runs inference
**THEN** system prompt loaded is the SKILL.md content
**AND** agent can use documented APIs successfully

### Issues Encountered

<!-- This section filled during implementation -->
