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

**Architecture**: v4 (comint shell) is functional but needs refactoring - see EPIC-007.
**Completed**: EPIC-005 (Core Loop), EPIC-006 (Human Interface/Hub).
**Active Work**: EPIC-007 - Consciousness-Driven Architecture. See `RAG/AI-EPIC/AI-EPIC-007-consciousness-driven-architecture.md`.
**CI Pipeline**: Run `./harness/ci-check.sh` before commits.

The v4 shell works but accidentally reimplemented the inference chain inline, bypassing the consciousness alist and skill system. EPIC-007 will refactor to proper separation: shell as pure UI, consciousness alist as source of truth.

## Implementation Status Map

**ACTIVE (v4 shell uses directly):**
| Module | Status | Notes |
|--------|--------|-------|
| `amacs-shell.el` | Active | Human I/O, but has inline state/inference (needs refactor) |
| `amacs-hub.el` | Active | Observability dashboard, reads from org files |
| `agent-api.el` | Active | HTTP calls to OpenRouter |
| `agent-persistence.el` | Active | Chat/scratchpad org file I/O |

**V3 LEGACY (exists but not wired to v4 shell):**
| Module | Status | Notes |
|--------|--------|-------|
| `agent-consciousness.el` | Unused by shell | Full alist state management - shell has own vars |
| `agent-context.el` | Unused by shell | Context assembly with buffer hydration |
| `agent-skills.el` | Unused by shell | Skill loading/binding - not connected |
| `agent-threads.el` | Unused by shell | Thread lifecycle - shell reimplemented inline |
| `agent-inference.el` | Unused by shell | Proper inference layer with skill loading |
| `agent-tick.el` | Unused by shell | Tick management - shell has own counter |
| `agent-chat.el` | Deprecated | v3 org-mode chat buffer interface |
| `agent-monologue.el` | Partial | Shell writes monologue but doesn't use module |
| `agent-core.el` | Unused by shell | Init/coordination - shell self-initializes |
| `agent-scratchpad.el` | Partial | Used via persistence, not directly |

**EPIC-007 TARGET STATE:**
- Shell → pure UI (triggers tick, displays reply)
- `agent-consciousness.el` → source of truth for all state
- `agent-inference.el` → context assembly + API call
- `agent-skills.el` → core skill = system prompt, bound skills load with threads
- `agent-context.el` → buffer hydration for active thread

**KEY ARCHITECTURAL INSIGHT:**
The agent is not a chatbot in a buffer. The agent lives in the consciousness alist. The shell is one I/O channel. The agent can run multiple ticks without replying (autonomous work), updating scratchpad/threads/buffers. The hub provides observability for both human and agent.

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
- `RAG/AI-EPIC/AI-EPIC-007-consciousness-driven-architecture.md` - Active epic
- `RAG/RFC/amacs-rfc-v4-transition.md` - v4 architecture (partially implemented)
- `RAG/RFC/amacs-rfc-concurrency.md` - Async options (future consideration)
- `RAG/AI-LOG/` - Latest session handoffs
- `RAG/draft-prompt.md` - Target system prompt (needs updating post-EPIC-007)

For reference:
- `harness/amacs-shell.el` - Current working code (but needs refactor)
- `harness/agent-inference.el` - v3 inference layer (target to reconnect)
- `skills/amacs-bootstrap-skill/core/SKILL.md` - v3 core skill (reference)

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
