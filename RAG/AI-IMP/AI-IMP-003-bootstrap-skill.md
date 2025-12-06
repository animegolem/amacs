---
node_id: AI-IMP-003
tags:
  - IMP-LIST
  - Implementation
  - phase-1
  - skills
  - bootstrap
kanban_status: completed
depends_on: 
  - AI-IMP-001
confidence_score: 0.85
created_date: 2025-12-04
close_date: 2025-12-06
--- 

# AI-IMP-003-bootstrap-skill

## Bootstrap Skill Loading

Integrate skill system into harness: copy bootstrap skill to `~/.agent/skills/core/`, load skill bindings on init, track active skills in consciousness, provide skill content for context assembly.

**Done when:** On init, core skill is available. `agent-get-relevant-skills` returns skills for current context. Skills tracked in `:active-skills`. Skill content loadable via `agent-load-skill`.

See: [[amacs-rfc-v3.md]] Part 8, `skills/amacs-bootstrap-skill/`

### Out of Scope 

- Per-thread skill binding (IMP-004)
- Skill creation by agent
- Skill usage statistics
- Progressive disclosure based on relevance scoring

### Design/Approach  

The bootstrap skill already exists at `skills/amacs-bootstrap-skill/core/` with:
- `SKILL.md` - Agent orientation
- `references/` - Schema docs, creating skills guide, tick system
- `scripts/skill-binding.el` - Complete binding system

IMP-003 integrates this into the harness:

1. **On `agent-init`:**
   - Ensure `~/.agent/skills/` exists
   - Copy/symlink core skill if not present
   - Load `skill-binding.el` functions
   - Load saved bindings if any
   - Bind core skill to always load

2. **Skill tracking in consciousness:**
   ```elisp
   :active-skills (("core" :loaded-tick 0 :use-count 5))
   ```

3. **Context assembly integration:**
   - `load-relevant-skills` returns skill content for current context
   - Core skill always included
   - Mode/buffer/project skills added based on bindings

### Files to Touch

```
harness/agent-skills.el         # NEW - skill system integration
harness/agent-core.el           # Init skill system
harness/agent-consciousness.el  # Add :active-skills tracking
harness/test-harness.el         # Skill tests
~/.agent/skills/core/           # Copied from bootstrap skill
```

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**? 
</CRITICAL_RULE> 

- [x] Implement `agent-skills.el`:
  - [x] Include/adapt `skill-binding.el` functions
  - [x] Implement `agent-ensure-core-skill` (copy if missing)
  - [x] Implement `agent-init-skills` (load bindings, bind core)
  - [x] Implement `agent-load-relevant-skills` (for context assembly)
  - [x] Implement `agent-track-skill-use` (update :active-skills)
  - [x] Implement `agent-skills-for-context` (returns skill content strings)
  - [x] Provide `agent-skills` feature
- [x] Modify `agent-consciousness.el`:
  - [x] Add `:active-skills` to default schema
  - [x] Implement `agent-active-skills` accessor
  - [x] Implement `agent-record-skill-use` (increment use count)
- [x] Modify `agent-core.el`:
  - [x] Call `agent-init-skills` in `agent-init`
  - [x] Require `agent-skills`
- [x] Copy bootstrap skill:
  - [x] Create `~/.agent/skills/core/` directory
  - [x] Copy SKILL.md and references/
  - [x] Scripts go into harness (not ~/.agent)
- [x] Test: init creates skills directory
- [x] Test: core skill loads and returns content
- [x] Test: `agent-get-relevant-skills` works for rust-mode buffer
- [x] Test: `:active-skills` tracks skill loading
 
### Acceptance Criteria

**Scenario:** Cold start initializes skills
**GIVEN** `~/.agent/` is fresh (no skills directory)
**WHEN** User calls `(agent-init)`
**THEN** `~/.agent/skills/core/SKILL.md` exists
**AND** `:active-skills` contains `("core" ...)`

**Scenario:** Core skill always loads
**GIVEN** Agent is initialized
**WHEN** Code calls `(agent-load-relevant-skills)`
**THEN** Result includes core skill content
**AND** Core skill use count increments

**Scenario:** Mode-bound skill loads
**GIVEN** Skill "rust-mode" exists and is bound to `rust-mode`
**WHEN** Current buffer is in `rust-mode`
**AND** Code calls `(agent-get-relevant-skills)`
**THEN** Result includes "rust-mode"

**Scenario:** Skills tracked in consciousness
**GIVEN** Skills "core" and "rust-mode" have been loaded
**WHEN** User inspects `:active-skills`
**THEN** Both skills appear with `:loaded-tick` and `:use-count`

### Bootstrap Skill Source

Copy from `skills/amacs-bootstrap-skill/core/` to `~/.agent/skills/core/`:
- `SKILL.md`
- `references/consciousness-schema.md`
- `references/creating-skills.md`
- `references/tick-system.md`

Scripts (`skill-binding.el`, `consciousness-helpers.el`) are adapted into harness code, not copied to ~/.agent.

### Issues Encountered 

<!-- Fill during implementation -->
