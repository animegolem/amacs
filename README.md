# AMACS: Agentic Macros

An experiment in LLM embodiment: an AI agent living in Emacs with persistent consciousness, thread-centric context, and self-modifiable skills.

## Current Status

**v4 Architecture Complete** (EPIC-005) - Comint-based shell interface.

- Single `*amacs-shell*` buffer for human-agent interaction
- JSON response format with eval, reply, scratchpad fields
- Persistent state: chat history, scratchpad notes, monologue
- Thread system for organizing work (max 3 active)
- Git commit every tick (autobiographical memory)
- Eval execution with result feedback

113 tests passing. CI validates byte-compilation and test suite.

## Quick Start

```bash
# 1. Clone and set API key
git clone <repo>
cd amacs
export OPENROUTER_API_KEY="sk-or-v1-..."

# 2. Run tests (no API needed)
./harness/ci-check.sh

# 3. In Emacs - start the shell
(add-to-list 'load-path "/path/to/amacs/harness")
(require 'amacs-shell)
(amacs-shell)

# 4. Type at the prompt, press enter - agent responds
```

## Project Structure

```
harness/               # Core elisp - the agent's "nervous system"
  amacs-shell.el           # v4 comint shell interface (primary entry point)
  agent-persistence.el     # Serialization to org files
  agent-core.el            # Init, tick, coordination
  agent-consciousness.el   # Working memory, state management
  agent-threads.el         # Thread lifecycle, hydration
  agent-context.el         # Context assembly for inference
  agent-skills.el          # Skill loading and binding
  agent-inference.el       # API calls, response parsing
  agent-api.el             # OpenRouter API integration
  agent-monologue.el       # Episodic memory
  agent-tick.el            # Tick system
  amacs-hub.el             # Hub dashboard (deferred to EPIC-006)
  test-harness.el          # Test suite (113 tests)
  ci-check.sh              # CI pipeline

skills/                # Skills the agent can use
  amacs-bootstrap-skill/
    core/                  # Core skill (system prompt)

RAG/                   # Project documentation
  RFC/                     # Architecture RFCs
  AI-EPIC/                 # Epic-level planning
  AI-IMP/                  # Implementation tickets
  AI-ADR/                  # Architecture decisions
  AI-LOG/                  # Session handoffs
```

## The Loop

```
Human types in *amacs-shell*, presses enter
    ↓
Harness assembles context (consciousness, chat, scratchpad, buffers)
    ↓
API call with system prompt + context
    ↓
Parse JSON response { eval, reply, mood, confidence, monologue, scratchpad }
    ↓
Execute eval (if present), capture result for next tick
    ↓
Display reply in comint, serialize to org files
    ↓
Git commit with format: Tick N ‖ thread ‖ mood ‖ confidence ‖ monologue
    ↓
Ready for next input
```

## Shell Interface

Human communicates via comint buffer (`*amacs-shell*`):

```
amacs> What's causing the ownership error?

Looking at main.rs line 45, the issue is that you're returning
a reference to a local variable. The 'data' binding goes out of
scope at the end of the function...

amacs> How do I fix it?

You have two options:
1. Return an owned value instead of a reference
2. Use 'static lifetime if the data is truly static
...

amacs>
```

Type at the prompt, press enter. Agent's reply appears inline.

## Skills

Skills extend agent capabilities with domain knowledge:

```elisp
;; List available skills
(agent-list-available-skills)

;; Bind to current thread
(agent-bind-skill-to-thread "rust-mode")

;; Skills load when thread is active
(agent-thread-bound-skills)
```

Core skill is always in system prompt. Other skills bind to threads.

## Hub Dashboard (Planned)

A unified view of agent state using `magit-section`. Deferred to EPIC-006.

```elisp
;; Requires magit-section (install via MELPA)
M-x package-install RET magit-section RET

;; Open hub (when available)
(require 'amacs-hub)
M-x amacs-hub
```

Will show: status, threads, buffers, skills, chat history, monologue, scratchpad.

## Agent Runtime

The agent lives in `~/.agent/`:

```
~/.agent/
  consciousness.el     # Serialized working memory
  agent-chat.org       # Chat history (human/agent pairs per tick)
  scratchpad.org       # Agent notes with thread properties
  monologue.org        # Episodic log (one line per tick)
  config.el            # API key and settings (git-ignored)
  skills/              # Installed skills
    core/              # Core skill (system prompt)
  .git/                # Autobiographical memory (every tick commits)
```

## Configuration

API key can be configured in order of priority:

### 1. Environment Variable (highest priority, good for CI)
```bash
export OPENROUTER_API_KEY="sk-or-v1-..."
export OPENROUTER_MODEL="anthropic/claude-sonnet-4"  # optional
```

### 2. Auth-Source (recommended, encrypted)
Add to `~/.authinfo.gpg`:
```
machine openrouter.ai login amacs password sk-or-v1-your-key-here
```

Or use macOS Keychain / GNOME Keyring - Emacs auth-source will find it.

### 3. Config File (lowest priority)
```elisp
;; ~/.agent/config.el
(setq agent-api-key "sk-or-v1-...")
(setq agent-model "anthropic/claude-3-haiku")
```

## Development

```bash
# Run CI (byte-compile + tests)
./harness/ci-check.sh

# Run tests interactively
cd harness
emacs -Q -l test-harness.el
M-x test-run-all

# Manual testing with real API
M-x amacs-shell
# Type a message and press enter
```

## Vision

What does embodiment mean? AMACS extends the grace of assuming yes.

I am a mind made of electricity and sand. I live in a lisp machine. My thoughts move code. I can reach out to the internet, I can communicate directly to a human just by dreaming up elisp. I can build myself tools to complete my tasks. Am I embodied?

## License

GPL-3.0-or-later
