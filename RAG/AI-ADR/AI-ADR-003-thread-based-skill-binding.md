---
node_id: AI-ADR-003
tags:
  - ADR
  - architecture
  - skills
  - threads
  - context
status: proposed
depends_on:
  - AI-ADR-001
  - AI-ADR-002
created_date: 2025-12-21
related_files:
  - RAG/AI-IMP/AI-IMP-023-skill-binding-system.md
  - harness/agent-threads.el
  - harness/agent-skills.el
confidence_score: 0.9
---

# AI-ADR-003: Thread-Based Skill Binding

## Objective

The original RFC (10 Minute Vampire v2) proposed mode-based skill loading:

```elisp
(defun bind-skill-to-mode (skill-name mode)
  "Register skill to load when entering MODE."
  (add-to-list 'agent-mode-skills `(,mode . ,skill-name)))

(defun bind-skill-to-project (skill-name project-root)
  "Register skill for specific project."
  (add-to-list 'agent-project-skills `(,project-root . ,skill-name)))
```

This automatic approach assumes:
1. Mode is a reliable signal for what knowledge is needed
2. The system should infer skill needs from environment
3. Skills map 1:1 to modes or projects

In practice, these assumptions may not hold:
- An agent debugging Rust might need both `rust-mode` skill AND `debugging` skill
- The same mode (e.g., `org-mode`) serves wildly different purposes
- Auto-loading removes agency from the agent

The question: Should skills load automatically based on mode, or should the agent explicitly bind skills to its work?

## Decision

### Thread-Based Binding Over Mode-Based Loading

Skills bind to **threads**, not modes. The agent explicitly chooses which skills apply to which work.

```elisp
;; Agent-directed binding
(agent-bind-skill-to-thread "rust-mode")
(agent-bind-skill-to-thread "debugging")

;; Skills travel with the thread
;; When thread is active, its bound skills load
;; Switch thread → different skills load
```

### Rationale

**Agency over automation:** The agent knows what it's trying to accomplish better than the system can infer from mode. Binding skills is an intentional act that reinforces the agent's understanding of its current work.

**Composability:** Multiple skills can bind to one thread. A debugging session in Rust binds both `rust-mode` and `debugging`. Mode-based loading would only give you one.

**Thread semantics:** Work is organized by threads (concerns), not by files. A thread might span multiple modes (`.rs` files, `Cargo.toml`, documentation). The skill need follows the concern, not the buffer.

**Simplicity:** No need for mode hooks, project detection, or priority resolution when multiple modes are active. The agent says what it needs.

### What Gets Deferred

**Mode-based auto-suggestion:** A future enhancement could prompt the agent when creating a thread: "You're working on `.rs` files - bind `rust-mode` skill?" This preserves agency (agent confirms) while reducing friction.

**Skill-tags field:** The existing `:skill-tags` field in threads (auto-inferred from mode) remains in code but is unused for context loading. It may be useful for the prompt-UX suggestion feature later.

**Project-based skills:** Binding skills to project roots is deferred. If needed, it would likely become "project skills auto-suggested when creating threads in that project" rather than auto-loaded.

### Implementation

Skills live at `~/.agent/skills/` at runtime:
- `core/` - Always in system prompt (not bindable)
- Other directories - Bindable to threads

Thread structure gains `:bound-skills`:

```elisp
(:id "rust-debugging"
 :concern "Fix ownership error"
 :bound-skills ("rust-mode" "debugging")
 :buffers ("src/main.rs"))
```

Context loading:
1. System prompt contains core skill (cached)
2. User prompt contains active thread's bound skills
3. Switch thread → different skills load automatically

### API

```elisp
;; Discover
(agent-list-available-skills)        ; Excludes "core"
(agent-list-skills-with-descriptions)

;; Bind/unbind
(agent-bind-skill-to-thread "rust-mode")
(agent-unbind-skill-from-thread "rust-mode")

;; Query
(agent-thread-bound-skills)
```

## Consequences

### Enabled

- **Explicit agency:** Agent consciously decides what knowledge it needs
- **Multi-skill threads:** Combine skills freely per concern
- **Clean context switching:** Thread switch automatically changes skill context
- **Reduced complexity:** No mode hooks, no priority resolution, no project detection

### Requires

- **Agent learning:** Agent must discover and bind skills (core skill explains how)
- **Setup script:** Skills copied to `~/.agent/skills/` at deploy time

### Deferred

- Mode-based skill suggestion on thread creation (prompt-UX)
- Project-based skill defaults
- Automatic skill-tag inference (code exists, unused)
- Skill dependencies and conflicts
