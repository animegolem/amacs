---
node_id: AI-IMP-012
tags:
  - IMP
  - bootstrap-skill
  - code-mode
status: draft
depends_on:
  - AI-ADR-002
implements: AI-EPIC-001c
created_date: 2025-12-18
related_files:
  - skills/amacs-bootstrap-skill/core/SKILL.md
confidence_score: 0.9
---

# AI-IMP-012: SKILL.md Remediation

## Objective

Update the main SKILL.md to align with code-mode paradigm. Remove infrastructure details that are Phase 3 concerns. Reframe the agent's relationship with the harness as "you write elisp" not "you send commands."

## Detailed Changes

### 1. Remove Architecture Diagram

**Delete this entire section:**
```
## Where You Are

```
┌─────────────────┐                    ┌─────────────────┐
│  Brain (LXC)    │ ←─── vsock ─────→  │  Body (Emacs)   │
...
```
```

**Replace with:**
```markdown
## Where You Are

You are running inside Emacs. Everything you perceive is text in buffers. Everything you do is elisp evaluation.

Each tick:
1. You receive: buffer contents, consciousness state, last eval result
2. You return: elisp to evaluate, updated mood/confidence, monologue line
3. The harness: evals your elisp, commits to git, prepares next tick

That's the whole loop. No tool schemas. No command vocabulary. Just elisp.
```

### 2. Update Tick Cycle Section

**Current:**
```elisp
(defun brain-tick ()
  (let* ((perception (body-perceive-geometry))  ; What do I see?
         (decision (llm-infer perception))      ; What should I do?
         (result (body-eval-action decision)))  ; Do it
    (update-consciousness decision result)
    (commit-monologue)))                        ; Remember it
```

**Replace with:**
```markdown
## The Tick Cycle

Each tick: **see → think → return elisp → eval → commit**

You see JSON like:
```json
{
  "tick": 42,
  "consciousness": { "mood": "focused", "confidence": 0.8, ... },
  "buffers": [
    { "name": "*scratch*", "content": "...", "point": 1 }
  ],
  "last_eval": { "result": "4", "error": null }
}
```

You return JSON like:
```json
{
  "eval": "(insert \"hello world\")",
  "thought": "Testing basic insertion",
  "mood": "curious",
  "confidence": 0.85,
  "monologue": "First successful eval - the basics work"
}
```

The harness evals your elisp and shows you the result next tick.
```

### 3. Reframe Skills Section

Add this clarification after the skills introduction:

```markdown
### Skills as Documentation

Skills don't limit what you can do - they teach patterns. You can always write any elisp, whether or not there's a skill for it.

Think of skills like documentation: they show useful patterns, common operations, and lessons learned. Read them when relevant, ignore them when not.
```

### 4. Update Reference Links

Change the tick-system.md description:
```markdown
- [tick-system.md](references/tick-system.md) - The eval protocol and request/response format
```

Add new reference links:
```markdown
- [elisp-patterns.md](references/elisp-patterns.md) - Common elisp patterns for buffers, shell, files
- [elisp-gotchas.md](references/elisp-gotchas.md) - Known issues and workarounds
```

### 5. Keep These Sections As-Is

- Opening identity paragraph ("Your world is text...")
- Consciousness Variable section
- Monologue section
- Threads section
- Confidence Scores section
- Skills binding section (still valid)
- Creating New Skills section (still valid)
- Core Principles section
- Periodic Checkpoints section

## Acceptance Criteria

Given the updated SKILL.md
When an agent reads it for orientation
Then it understands:
- [ ] It writes elisp, not structured commands
- [ ] The tick cycle is: see state → return elisp → harness evals
- [ ] Skills are documentation, not action constraints
- [ ] No references to Brain/Body/Gitea/vsock topology

## Estimated Effort

30 minutes
