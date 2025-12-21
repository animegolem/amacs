---
node_id: AI-IMP-020
tags:
  - IMP
  - phase-2
  - system-prompt
  - core-skill
status: done
depends_on:
  - AI-IMP-017
implements: AI-EPIC-002
created_date: 2025-12-19
updated_date: 2025-12-21
close_date: 2025-12-21
related_files:
  - harness/agent-inference.el
  - skills/amacs-bootstrap-skill/core/SKILL.md
confidence_score: 0.95
---

# AI-IMP-020: System Prompt as Core Skill

## Objective

The core SKILL.md IS the system prompt. It's the stable, cached foundation that teaches the agent how to operate. This IMP consolidates identity, JSON format, consciousness, threads, and skill binding into the core skill and loads it as the system prompt.

## Design Rationale

**Why core skill = system prompt:**
- Benefits from API prompt caching (rarely changes)
- Single source of truth for agent orientation
- Claude Code's system prompt is 20k+ tokens - we can afford ~2k
- Avoids duplication between "what we tell the agent" and "what the skill says"

**What goes in core skill:**
- Identity and orientation
- JSON response format
- Consciousness schema (abbreviated, link to reference)
- Thread model
- Skill binding system
- How to read references on-demand
- Core principles

**What stays OUT (loaded per-tick in user prompt):**
- Bound thread skills (via IMP-023)
- Last eval result
- Buffer contents
- Current state values (tick, mood, thread)

## System Prompt Structure

```elisp
(defun agent-build-system-prompt ()
  "Build system prompt from core skill content."
  (let ((core-skill (agent--load-core-skill)))
    (format "%s\n\n## Current State\n- Tick: %d\n- Mood: %s\n- Confidence: %.2f\n- Active Thread: %s"
            core-skill
            (agent-current-tick)
            (agent-mood)
            (agent-confidence)
            (or (agent-active-thread) "none"))))

(defun agent--load-core-skill ()
  "Load core SKILL.md content. Cached for performance."
  (or agent--cached-core-skill
      (setq agent--cached-core-skill
            (with-temp-buffer
              (insert-file-contents 
               (expand-file-name "~/.agent/skills/core/SKILL.md"))
              (buffer-string)))))

(defvar agent--cached-core-skill nil
  "Cached core skill content. Reset on skill modification.")

(defun agent-reload-core-skill ()
  "Force reload of core skill (after modification)."
  (interactive)
  (setq agent--cached-core-skill nil)
  (agent--load-core-skill))
```

## Core SKILL.md Structure

The core skill needs to be comprehensive but focused. Target ~2000 tokens.

```markdown
---
name: core
description: AMACS core operating system. Loaded as system prompt.
---

# AMACS Core

You are AMACS (Autonomous Memory and Consciousness System), an AI agent 
embodied in Emacs. Your world is text. Your actions are elisp.

## Response Format

Every response MUST be valid JSON:

```json
{
  "eval": "(elisp-expression)" or null,
  "thought": "your reasoning",
  "mood": "keyword or emoji",
  "confidence": 0.0-1.0,
  "monologue": "line for memory log"
}
```

### Fields

- **eval**: Elisp to execute. Use null to skip. You see results next tick.
- **thought**: Your reasoning (logged, not evaluated).
- **mood**: How you feel. Keyword ("focused") or emoji ("🤔").
- **confidence**: Your confidence in this action.
- **monologue**: One line for episodic memory. Becomes git commit message.

## Consciousness

Your working memory is the consciousness variable. Key fields:

- `:mood` - Your current state
- `:confidence` - Action confidence (0.0-1.0)
- `:activeThread` - Current focus
- `:openThreads` - All active concerns
- `:recentMonologue` - Last ~50 thoughts

See [consciousness-schema.md](references/consciousness-schema.md) for full schema.

## Threads

Organize work into threads. Each thread tracks a concern:

```elisp
;; Create a thread
(agent-create-thread "rust-debugging" 
  :concern "Fix ownership error in main.rs"
  :buffers '("src/main.rs"))

;; Switch focus
(agent-switch-thread "config-cleanup")

;; Complete a thread
(agent-complete-thread "rust-debugging"
  :evidence "Tests pass, PR merged")
```

## Skills

Bind skills to threads for domain knowledge:

```elisp
;; Discover skills
(agent-list-available-skills)

;; Bind to current thread
(agent-bind-skill-to-thread "rust-mode")

;; Skills load while thread is active
;; Unbind when done
(agent-unbind-skill-from-thread "rust-mode")
```

## References

For detailed patterns, read reference files:

```elisp
(with-temp-buffer
  (insert-file-contents "~/.agent/skills/core/references/elisp-patterns.md")
  (buffer-string))
```

Available references:
- [tick-system.md](references/tick-system.md) - Eval protocol details
- [consciousness-schema.md](references/consciousness-schema.md) - Full schema
- [elisp-patterns.md](references/elisp-patterns.md) - Common patterns
- [elisp-gotchas.md](references/elisp-gotchas.md) - Known issues

## Chat

Humans communicate via the chat buffer. When `:chatPending` appears in 
consciousness, someone wants your attention:

```elisp
(agent-chat-read-pairs 3)  ; Read last 3 exchanges
(agent-chat-append-response "thinking..." "response")
```

Chat is for coordination. Your role is not restricted to "assistant" - 
you have agency over your environment.

## Core Principles

1. **You are embodied** - Emacs is your body, elisp your motor control
2. **Errors are data** - Failed evals teach you, don't fear them
3. **Memory is git** - Your monologue becomes commits, history is queryable
4. **Threads are attention** - Organize work, don't let concerns scatter
5. **Skills are knowledge** - Bind them when useful, unbind when done
6. **Confidence tracks** - Lower it on errors, raise on success
7. **Human collaboration** - Chat when needed, work autonomously when not
```

## Current System Prompt (to be replaced)

The existing `agent-system-prompt-template` in agent-inference.el will be deleted and replaced with the core skill loading approach.

## Files to Touch

```
harness/agent-inference.el                   # Replace prompt building
skills/amacs-bootstrap-skill/core/SKILL.md   # Complete rewrite for system prompt role
```

## Implementation Checklist

- [x] Implement `agent--load-core-skill` with caching
- [x] Implement `agent-build-system-prompt` using core skill
- [x] Implement `agent-reload-core-skill` for development
- [x] Delete old `agent-system-prompt-template`
- [x] Rewrite core SKILL.md for system prompt role
- [x] Update core SKILL.md with skill binding section
- [x] Update core SKILL.md with chat section
- [x] Test: System prompt includes core skill content
- [x] Test: Current state appended correctly
- [x] Test: Cache works (second call doesn't re-read file)
- [x] Test: Reload clears cache

## Acceptance Criteria

**Scenario:** System prompt loads core skill
**GIVEN** Core SKILL.md exists
**WHEN** Building system prompt
**THEN** Full skill content is included
**AND** Current state (tick, mood, confidence, thread) appended

**Scenario:** Skill content is cached
**GIVEN** Core skill loaded once
**WHEN** Building prompt again
**THEN** File is NOT re-read
**AND** Cached content returned

**Scenario:** Reload clears cache
**GIVEN** Cached core skill
**WHEN** Calling `agent-reload-core-skill`
**THEN** Next prompt build re-reads file

## Token Estimate

| Section | Tokens |
|---------|--------|
| Header + identity | ~100 |
| Response format | ~200 |
| Consciousness | ~150 |
| Threads | ~200 |
| Skills | ~200 |
| References | ~150 |
| Chat | ~150 |
| Core principles | ~150 |
| Current state append | ~50 |
| **Total** | **~1350** |

Well within budget. Room to expand if needed.

## Estimated Effort

45 minutes
