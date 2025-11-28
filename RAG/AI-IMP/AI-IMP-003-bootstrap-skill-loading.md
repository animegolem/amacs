---
node_id: AI-IMP-003
tags:
  - IMP-LIST
  - Implementation
  - phase-1
  - skills
  - bootstrap
kanban_status: planned
depends_on: 
  - AI-IMP-001
confidence_score: 0.85
created_date: 2025-11-27
close_date: 
--- 

# AI-IMP-003-bootstrap-skill-loading

## Skill System Foundation + Bootstrap Skill

Implement skill loading infrastructure and load the bootstrap (core) skill on initialization. This provides the agent with orientation documentation when inference begins.

**Done when:** On init, core skill SKILL.md is read and tracked in `:active-skills`. Skill binding functions work. Agent can query which skills are relevant for current context.

See: [[amacs-rfc-v3.md]] Part 8 (Skills System), [[creating-skills.md]]

### Out of Scope 

- Agent creating new skills (emergent, not coded)
- Skill usage tracking beyond load count (Phase 2)
- Mode/buffer binding triggers (infrastructure only, no auto-loading yet)
- Reference file loading (agent does this explicitly)

### Design/Approach  

Skills are directories with SKILL.md entrypoint. The bootstrap skill already exists at `amacs/skills/amacs-bootstrap-skill/core/`.

For Phase 1, skill loading is explicit - agent (or init) calls load function. Later phases add automatic loading based on context.

Skill tracking in consciousness:
```elisp
:active-skills
  ((:name "core" :loaded-at-tick 0 :uses 1))
```

Directory structure:
```
~/.agent/skills/           # Agent's skill directory (can create new ones)
└── core -> /path/to/amacs/skills/amacs-bootstrap-skill/core/  # Symlink to bootstrap
```

We symlink rather than copy so bootstrap skill updates flow through.

### Files to Touch

```
~/.emacs.d/amacs/agent-skills.el     # New file - skill loading infrastructure
~/.emacs.d/amacs/agent-core.el       # Modify - load core skill on init
~/.emacs.d/amacs/agent-consciousness.el  # Verify :active-skills in schema
~/.agent/skills/                     # Directory created on init
~/.agent/skills/core                 # Symlink to bootstrap skill
```

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**? 
</CRITICAL_RULE> 

- [ ] Implement `agent-skills.el`:
  - [ ] Define `agent-skills-directory` (`~/.agent/skills/`)
  - [ ] Define `agent-builtin-skills-directory` (path to repo's skills/)
  - [ ] Implement `agent-skill-exists-p` (check for SKILL.md)
  - [ ] Implement `agent-load-skill` (read SKILL.md content)
  - [ ] Implement `agent-load-skill-reference` (read from references/)
  - [ ] Implement `agent-record-skill-use` (update `:active-skills`)
  - [ ] Implement `agent-get-relevant-skills` (based on mode/buffer bindings)
  - [ ] Implement `agent-list-available-skills` (scan directory)
  - [ ] Implement binding functions from `skill-binding.el`:
    - [ ] `bind-skill-to-mode`
    - [ ] `bind-skill-to-buffer`
    - [ ] `bind-skill-to-project`
  - [ ] Implement `agent-save-skill-bindings` / `agent-load-skill-bindings`
  - [ ] Provide `agent-skills` feature
- [ ] Modify `agent-core.el`:
  - [ ] Create `~/.agent/skills/` on init
  - [ ] Create symlink `~/.agent/skills/core` -> bootstrap skill
  - [ ] Load core skill on init
  - [ ] Record core skill in `:active-skills`
- [ ] Verify consciousness schema includes `:active-skills`
- [ ] Test: init creates skills directory and symlink
- [ ] Test: `agent-load-skill "core"` returns SKILL.md content
- [ ] Test: `agent-load-skill-reference "core" "consciousness-schema.md"` works
- [ ] Test: `:active-skills` shows core skill after init
- [ ] Test: binding functions add to appropriate alists
- [ ] Test: `agent-get-relevant-skills` returns core for any context (always relevant)
 
### Acceptance Criteria

**Scenario:** Fresh init loads bootstrap skill
**GIVEN** `~/.agent/skills/` does not exist
**WHEN** User calls `(agent-init)`
**THEN** `~/.agent/skills/` directory is created
**AND** `~/.agent/skills/core` symlink points to bootstrap skill
**AND** `:active-skills` contains `(:name "core" :loaded-at-tick 0 :uses 1)`

**Scenario:** Load skill content
**GIVEN** Agent is initialized
**WHEN** Code calls `(agent-load-skill "core")`
**THEN** Returns string containing SKILL.md content
**AND** Content includes "AMACS Core" header

**Scenario:** Load reference file
**GIVEN** Agent is initialized
**WHEN** Code calls `(agent-load-skill-reference "core" "consciousness-schema.md")`
**THEN** Returns string containing consciousness schema documentation

**Scenario:** Skill binding
**GIVEN** Agent is initialized
**WHEN** Code calls `(bind-skill-to-mode "rust-helpers" 'rust-mode)`
**AND** Current buffer is in rust-mode
**THEN** `(agent-get-relevant-skills)` includes "rust-helpers"

**Scenario:** Nonexistent skill
**GIVEN** Agent is initialized
**WHEN** Code calls `(agent-load-skill "nonexistent")`
**THEN** Returns nil
**AND** Message logged about missing skill

### Issues Encountered 

<!-- Fill during implementation -->
