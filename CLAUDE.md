# AMACS - Agentic Macros

An experiment in LLM embodiment: an AI agent living in Emacs with persistent consciousness, thread-centric context, and self-modifiable skills.

## Project Structure

```
harness/           # Core elisp - the agent's "nervous system"
  agent-core.el         # Init, tick, coordination
  agent-consciousness.el # Working memory, state management
  agent-threads.el      # Thread lifecycle, hydration
  agent-context.el      # Context assembly for inference
  agent-skills.el       # Skill loading and tracking
  agent-monologue.el    # Episodic memory
  agent-tick.el         # Tick system
  test-harness.el       # Test suite

skills/            # Skills the agent can use
  amacs-bootstrap-skill/core/  # Bootstrap skill (installed to ~/.agent/skills/core/)

RAG/               # Project documentation
  AI-EPIC/         # Epic-level planning
  AI-IMP/          # Implementation tickets (like issues)
  AI-ADR/          # Architecture Decision Records
  AI-LOG/          # Work session logs/handoffs
  templates/       # Templates for new docs

amacs-rfc-v3.org   # Full architecture RFC - read for deep context
```

## Current Status

**Phase 1 (Vampire Simulator)**: Complete. 39/39 tests passing.
**Next work**:
- IMP-011: First inference (API integration)
- IMP-005: CI pipeline (byte-compile + tests)

## RAG Documentation Conventions

- **EPIC**: Large feature/milestone (e.g., `AI-EPIC-001-vampire-simulator-core.md`)
- **IMP**: Implementation ticket with checklist (e.g., `AI-IMP-004-thread-centric-context.md`)
- **ADR**: Architecture decisions (e.g., `AI-ADR-001-thread-centric-context.md`)
- **LOG**: Session handoff notes with commits, issues, next steps

Always check relevant LOG files for session context when resuming work.

## Running Tests

```bash
cd harness
emacs -Q -l test-harness.el
```

All tests must pass. Test output shows PASS/FAIL per test and summary.

## CI Pipeline (IMP-005 - Not Yet Implemented)

When implemented, run:
```bash
./harness/ci-check.sh
```

Will perform:
1. Byte-compile all `.el` files (except test-harness.el) with strict warnings
2. Run test suite in batch mode
3. Exit 0 on success, non-zero on failure

## Elisp Gotchas (From Prior Sessions)

1. **Never use `t` as a parameter name** - it's a constant. Use `thr`, `thread`, etc.
2. **Avoid backquote for mutable plists** - backquote shares cons cells. Use explicit `(list ...)` for plists you'll modify with `plist-put`.
3. **`git rev-parse HEAD` on empty repos** - check return code, not output (errors go to stdout).
4. **`parse-iso8601-time-string`** - fails silently in some Emacs versions. Wrap with fallback.

## Key Files to Read

Before major work:
- `amacs-rfc-v3.org` - Full architecture vision
- `RAG/AI-LOG/` - Latest session handoffs
- `RAG/AI-IMP/` - Current implementation tickets

For harness understanding:
- `harness/agent-threads.el` - Thread model
- `harness/agent-context.el` - Context assembly
- `skills/amacs-bootstrap-skill/core/SKILL.md` - What agent sees on startup

## Agent Runtime Directory

The agent runs in `~/.agent/` which contains:
- `consciousness.el` - Serialized working memory
- `monologue.org` - Episodic log
- `skills/` - Installed skills
- `.git/` - Autobiographical memory (every tick commits)

Tests recreate this directory fresh.
