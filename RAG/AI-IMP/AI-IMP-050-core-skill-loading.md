---
node_id: AI-IMP-050
tags:
  - IMP-LIST
  - Implementation
  - EPIC-007
  - skills
  - system-prompt
kanban_status: backlog
depends_on:
  - AI-IMP-049
confidence_score: 0.85
created_date: 2025-01-11
close_date:
---

# AI-IMP-050-core-skill-loading

## Summary

Ensure the system prompt is loaded from the core skill file (`~/.agent/skills/core/SKILL.md`) rather than hardcoded. The v3 `agent-inference.el` already has `agent-build-system-prompt` and `agent--load-core-skill` - this IMP verifies they work correctly and updates the skill content to match `draft-prompt.md`.

**Current state**: `agent-inference.el` has skill loading code but shell bypasses it. Core skill file exists at `skills/amacs-bootstrap-skill/core/SKILL.md` but may be outdated.

**Target state**: System prompt loaded from `~/.agent/skills/core/SKILL.md`. Content matches `draft-prompt.md` (updated for v4 reality).

**Done when**: `(agent-build-system-prompt)` returns content from skill file. Prompt caching viable (stable across ticks).

### Out of Scope

- Bound skills per thread (IMP-051)
- Full draft-prompt.md update (IMP-053) - just ensure loading works here

### Design/Approach

1. Verify `agent--load-core-skill` reads from correct path
2. Ensure skill file is copied to `~/.agent/skills/core/` during init
3. Test `agent-build-system-prompt` returns file content
4. Basic sanity check that prompt contains expected sections
5. Ensure prompt is cached (not re-read every tick)

The existing code in `agent-inference.el`:
```elisp
(defun agent--load-core-skill ()
  "Load core skill content, caching if already loaded."
  ...)

(defun agent-build-system-prompt ()
  "Build system prompt from core skill content with current state."
  ...)
```

Need to verify these work and are called by inference layer.

### Files to Touch

`harness/agent-inference.el`: Verify/fix core skill loading
`harness/agent-core.el`: Ensure skill directory setup in init
`skills/amacs-bootstrap-skill/core/SKILL.md`: May need path/content review

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**?
</CRITICAL_RULE>

- [ ] Verify `agent--load-core-skill` function exists and works
- [ ] Check skill file path: should be `~/.agent/skills/core/SKILL.md`
- [ ] Ensure `agent-core.el` init copies/links core skill to runtime directory
- [ ] Test `(agent-build-system-prompt)` returns non-empty string
- [ ] Verify prompt contains key sections (response format, thread API, etc.)
- [ ] Verify caching: second call returns same content without file read
- [ ] Add `(agent-reload-core-skill)` command for development
- [ ] Ensure inference layer uses `agent-build-system-prompt` not hardcoded string
- [ ] Run CI: `./harness/ci-check.sh`
- [ ] Test inference produces valid response with skill-based prompt

### Acceptance Criteria

**Scenario:** System prompt loaded from file
**GIVEN** the agent is initialized
**WHEN** `(agent-build-system-prompt)` is called
**THEN** it returns content from `~/.agent/skills/core/SKILL.md`
**AND** the content includes "AMACS" and response format documentation

**Scenario:** Prompt is cached
**GIVEN** the core skill has been loaded once
**WHEN** `(agent-build-system-prompt)` is called again
**THEN** it returns cached content
**AND** no file I/O occurs

**Scenario:** Skill can be reloaded
**GIVEN** the skill file is modified
**WHEN** `(agent-reload-core-skill)` is called
**THEN** subsequent prompts use the new content

### Issues Encountered

<!-- This section filled during implementation -->
