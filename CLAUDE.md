# AMACS - Agentic Macros

An experiment in LLM embodiment: an AI agent living in Emacs with persistent consciousness, thread-centric context, and self-modifiable skills.

## Project Structure

```
harness/           # Core elisp - the agent's "nervous system"
  amacs-shell.el        # Comint-based human-agent I/O (v4)
  agent-core.el         # Init, tick, coordination
  agent-consciousness.el # Working memory, state management
  agent-threads.el      # Thread lifecycle, hydration
  agent-context.el      # Context assembly for inference
  agent-skills.el       # Skill loading and tracking
  agent-monologue.el    # Episodic memory
  agent-tick.el         # Tick system
  test-harness.el       # Test suite

skills/            # Skills the agent can use
  amacs-bootstrap-skill/core/  # Core skill = system prompt

RAG/               # Project documentation
  RFC/             # Architecture RFCs
  AI-EPIC/         # Epic-level planning
  AI-IMP/          # Implementation tickets (like issues)
  AI-ADR/          # Architecture Decision Records
  AI-LOG/          # Work session logs/handoffs
  templates/       # Templates for new docs
```

## Current Status

**Architecture**: Transitioning from v3 (org-mode prompt blocks) to v4 (comint shell).
**Active Work**: EPIC-005 - Core Loop Rewrite. See `RAG/AI-EPIC/AI-EPIC-005-core-loop-rewrite.md`.
**CI Pipeline**: Run `./harness/ci-check.sh` before commits.

The v3 code (EPIC-001 through EPIC-004) is functional but field testing revealed buffer navigation complexity. The v4 rewrite simplifies I/O to a single comint shell.

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
- `RAG/RFC/amacs-rfc-v4-transition.md` - Current architecture (v4 comint-based)
- `RAG/AI-EPIC/AI-EPIC-005-core-loop-rewrite.md` - Active epic
- `RAG/AI-LOG/` - Latest session handoffs
- `draft-prompt.md` - System prompt design (becomes core skill)

For reference (v3, partially superseded):
- `archive/amacs-rfc-v3.org` - Original architecture vision
- `harness/agent-threads.el` - Thread model (being updated)
- `skills/amacs-bootstrap-skill/core/SKILL.md` - Current core skill

## Agent Runtime Directory

The agent runs in `~/.agent/` which contains:
- `consciousness.el` - Serialized working memory
- `agent-chat.org` - Chat history (human/agent pairs per tick)
- `scratchpad.org` - Agent notes with thread properties
- `monologue.org` - Episodic log (one line per tick)
- `skills/` - Installed skills (core/ is the system prompt)
- `.git/` - Autobiographical memory (every tick commits)

Tests recreate this directory fresh.

## API Configuration

Priority: env vars > auth-source > config file > defaults.

**Option 1: Environment variables (preferred for CI)**
```bash
export OPENROUTER_API_KEY="sk-or-v1-..."
export OPENROUTER_MODEL="anthropic/claude-3.5-sonnet"  # optional
```

**Option 2: Auth-source (preferred for security)**
Add to `~/.authinfo.gpg`:
```
machine openrouter.ai login amacs password sk-or-v1-...
```

**Option 3: Config file**
Create `~/.agent/config.el`:
```elisp
(setq agent-api-key "sk-or-v1-...")
(setq agent-model "anthropic/claude-3-haiku")  ; optional
```

## V4 Response Format

Agent responses are JSON (see `RAG/RFC/amacs-rfc-v4-transition.md`):

```json
{
  "eval": "(elisp)" | null,
  "reply": "Text for human in comint",
  "mood": "focused",
  "confidence": 0.85,
  "monologue": "One line for git commit",
  "scratchpad": {"heading": "...", "thread": "id"|null, "content": "..."}
}
```

Mandatory: `mood`, `confidence`, `monologue`
Optional: `eval`, `reply`, `scratchpad`
