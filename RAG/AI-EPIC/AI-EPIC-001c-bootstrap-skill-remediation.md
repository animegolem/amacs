---
node_id: AI-EPIC-001c
tags:
  - EPIC
  - bootstrap-skill
  - remediation
  - code-mode
status: completed
depends_on:
  - AI-ADR-002
created_date: 2025-12-18
related_files:
  - skills/amacs-bootstrap-skill/core/SKILL.md
  - skills/amacs-bootstrap-skill/core/references/tick-system.md
  - skills/amacs-bootstrap-skill/core/references/consciousness-schema.md
confidence_score: 0.9
---

# AI-EPIC-001c: Bootstrap Skill Remediation

## Narrative

With Phase 2 adopting code-mode (raw elisp eval) over tool-calling, the bootstrap skill needs updates. The current skill was written assuming a tool-calling paradigm with `cortex-dispatch` command vocabulary and Brain/Body/Gitea architecture diagrams that are Phase 3 implementation details.

Before the agent can effectively use eval-based motor control, it needs documentation that:
1. Teaches practical elisp patterns (not abstract architecture)
2. Removes implementation details it doesn't need yet
3. Provides gotchas we've discovered during implementation
4. Frames skills as documentation, not action constraints

## Acceptance Criteria

- [x] SKILL.md updated: architecture diagram removed, opening reframed for code-mode
- [x] tick-system.md rewritten: cortex-dispatch removed, raw eval protocol documented
- [x] NEW elisp-patterns.md: practical patterns for buffers, eshell, files, functions
- [x] NEW elisp-gotchas.md: implementation bugs and workarounds we've collected
- [x] consciousness-schema.md reviewed and updated if needed
- [x] All internal references consistent

## Scope

### In Scope
- Bootstrap skill text changes
- New reference files
- Reframing from tool-calling to code-mode paradigm

### Out of Scope
- Harness code changes (that's EPIC-002)
- New skills beyond core
- Infrastructure documentation

## Implementation Strategy

### IMP-012: SKILL.md Remediation

**Changes:**
1. Remove architecture diagram (Brain/Body/Gitea topology)
2. Simplify "Where You Are" to: "You are in Emacs. You see buffers. You write elisp."
3. Add brief protocol description: "Each tick: you see state, you return elisp to eval"
4. Update tick cycle description for code-mode
5. Ensure skills framed as "documentation you read" not "actions available"

**Keep:**
- Opening identity framing ("Your world is text...")
- Consciousness variable documentation
- Thread management
- Monologue and git memory
- Skill creation patterns (still valid)
- Core principles

### IMP-013: tick-system.md Rewrite

**Remove:**
- Architecture diagrams showing vsock/Brain/Body/Gitea
- `cortex-dispatch` function and command vocabulary
- Wake logic details (Phase 3 concern)
- Complex context building (implementation detail)

**Replace with:**
- Simple tick cycle: perceive → think → return elisp → eval → commit
- What the agent sees (JSON with buffers, consciousness, last result)
- What the agent returns (JSON with elisp, thought, mood, confidence, monologue)
- Example request/response
- Error handling (what happens when eval fails)

### IMP-014: elisp-patterns.md (NEW)

Practical patterns the agent can use:

```markdown
# Elisp Patterns for AMACS

## Buffer Operations
- Reading buffer content
- Switching buffers
- Creating/killing buffers
- Finding text, moving point

## Eshell Interaction
- Sending commands
- Reading output
- Waiting for completion

## File Operations
- Reading files into buffers
- Writing buffers to files
- File existence checks

## Defining Functions
- Creating helper functions
- Persisting to skills/custom/functions.el
- Using lexical binding

## Common Patterns
- Error handling with condition-case
- Working with plists
- String manipulation
```

### IMP-015: elisp-gotchas.md (NEW)

Collected from our implementation work:

```markdown
# Elisp Gotchas

## Backquote Structure Sharing
Problem: Modifying backquoted lists affects the "template"
Solution: Use (copy-tree ...) or (list ...) for mutable data

## Constant Symbol Assignment  
Problem: Can't setq symbols that appear in defvar initialization
Solution: Use (setf (plist-get ...)) or (plist-put ...)

## UTF-8 Encoding for HTTP
Problem: url.el doesn't encode body by default
Solution: (encode-coding-string body 'utf-8 t)

## when-let vs when-let*
Problem: when-let deprecated in Emacs 31
Solution: Always use when-let*

## Timestamp Parsing
Problem: parse-time-string returns vector, not list
Solution: Use (iso8601-parse ...) for ISO timestamps

## Git Commit Detection
Problem: shell-command-to-string may include trailing newline
Solution: (string-trim ...) the result
```

### IMP-016: consciousness-schema.md Review

Review current schema against harness implementation. Update if:
- Fields have changed names
- New fields added (like :global-buffers)
- Thread structure updated
- Budget tracking added

## Dependencies

- **Depends on:** AI-ADR-002 (establishes code-mode paradigm)
- **Blocks:** AI-EPIC-002 (Hands and Arms - needs correct docs before eval access)

## Effort Estimate

| IMP | Task | Estimate |
|-----|------|----------|
| 012 | SKILL.md remediation | 30 min |
| 013 | tick-system.md rewrite | 45 min |
| 014 | elisp-patterns.md (new) | 60 min |
| 015 | elisp-gotchas.md (new) | 30 min |
| 016 | consciousness-schema.md review | 15 min |

**Total:** ~3 hours

## Success Metrics

1. An agent reading the bootstrap skill understands it can write any elisp
2. No references to cortex-dispatch, tool schemas, or action vocabularies
3. No architecture diagrams showing infrastructure not yet deployed
4. Practical patterns are usable immediately (copy-paste ready)
5. Gotchas prevent bugs we already discovered

## Notes

