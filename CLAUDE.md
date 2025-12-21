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
**Phase 2 (Hands and Arms)**: In progress. See AI-EPIC-002.
**CI Pipeline**: Implemented. Run `./harness/ci-check.sh` before commits.

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

## CI Pipeline

Run before committing:
```bash
./harness/ci-check.sh
```

Performs:
1. Byte-compile all `.el` files (except test-harness.el) with strict warnings
2. Run test suite in batch mode
3. Exit 0 on success, 1 on byte-compile failure, 2 on test failure

## Elisp Gotchas (From Prior Sessions)

1. **Never use `t` as a parameter name** - it's a constant. Use `thr`, `thread`, etc.
2. **Avoid backquote for mutable plists** - backquote shares cons cells. Use explicit `(list ...)` for plists you'll modify with `plist-put`.
3. **`git rev-parse HEAD` on empty repos** - check return code, not output (errors go to stdout).
4. **`parse-iso8601-time-string`** - fails silently in some Emacs versions. Wrap with fallback.
5. **`url.el` multibyte error** - when POSTing JSON with `url-request-data`, encode as UTF-8: `(encode-coding-string body 'utf-8 t)` (the `t` forces unibyte)
6. **`when-let`/`if-let` deprecated in Emacs 31** - use `when-let*`/`if-let*` instead. The `*` variants bind sequentially (like `let*`), which is usually what you want anyway.
7. Unescaped single quotes in docstrings - 'symbol in docstrings triggers byte-compile error; omit the quote or use backticks
8. Docstrings wider than 80 characters - byte-compiler warns; break with \ continuation
9. Circular requires - use (defvar varname) and (declare-function func "file") for forward declarations

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

## API Configuration

Set environment variables (preferred):
```bash
export OPENROUTER_API_KEY="sk-or-v1-..."
export OPENROUTER_MODEL="anthropic/claude-3.5-sonnet"  # optional
```

Or create `~/.agent/config.el` (survives if you don't run tests):
```elisp
(setq agent-api-key "sk-or-v1-...")
(setq agent-model "anthropic/claude-3-haiku")  ; optional, cheaper for testing
```

Priority: env vars > config file > defaults.
