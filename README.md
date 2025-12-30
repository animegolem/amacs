# AMACS: Agentic Macros

An experiment in LLM embodiment: an AI agent living in Emacs with persistent consciousness, thread-centric context, and self-modifiable skills.

## Current Status

**Phase 2 Complete** - The agent has motor control.

- Perceives buffer contents and context
- Thinks via LLM API (OpenRouter)
- Returns elisp for evaluation
- Sees results in next tick
- Communicates via org-mode chat
- Binds skills to threads

113 tests passing. CI validates byte-compilation and test suite.

## Quick Start

```bash
# 1. Clone and set API key
git clone <repo>
cd amacs
export OPENROUTER_API_KEY="sk-or-v1-..."

# 2. Run tests (no API needed)
./harness/ci-check.sh

# 3. In Emacs - initialize agent
(add-to-list 'load-path "/path/to/amacs/harness")
(require 'agent-core)
(agent-init)

# 4. Run integration test (requires API)
M-x test-eval-loop
```

## Project Structure

```
harness/               # Core elisp - the agent's "nervous system"
  agent-core.el            # Init, tick, coordination
  agent-consciousness.el   # Working memory, state management
  agent-threads.el         # Thread lifecycle, hydration
  agent-context.el         # Context assembly for inference
  agent-skills.el          # Skill loading and binding
  agent-inference.el       # Prompt building, API calls, response parsing
  agent-chat.el            # Human-agent chat interface
  agent-monologue.el       # Episodic memory
  agent-tick.el            # Tick system, git commits
  amacs-hub.el             # Hub dashboard (requires magit-section)
  test-harness.el          # Test suite (113 tests)
  ci-check.sh              # CI pipeline

skills/                # Skills the agent can use
  amacs-bootstrap-skill/
    core/                  # Bootstrap skill (system prompt)
    chat/                  # Chat interface skill

RAG/                   # Project documentation
  AI-EPIC/                 # Epic-level planning
  AI-IMP/                  # Implementation tickets
  AI-ADR/                  # Architecture decisions
  AI-LOG/                  # Session handoffs
```

## The Loop

```
perceive (buffers, threads, context)
    ↓
think (API call with system prompt)
    ↓
return JSON { eval, thought, mood, confidence, monologue }
    ↓
harness executes eval, captures result/error
    ↓
next tick: agent sees result in context
    ↓
repeat
```

## Chat Interface

Human communicates via org-mode buffer:

```org
* Human Input
Can you check the current buffer?

* Agent Response
** Think
Reading the user's request...

** Output
I'll examine the buffer now.
```

Press `C-c C-c` in chat buffer to send. Agent sees `:chat-pending` flag.

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

## Hub Dashboard

The hub provides a unified view of agent state using `magit-section`:

```elisp
;; Requires magit-section (install via MELPA)
M-x package-install RET magit-section RET

;; Open hub
(require 'amacs-hub)
M-x amacs-hub

;; Key bindings
TAB - Expand/collapse section inline
g   - Refresh hub
```

Shows: status, threads, buffers, skills, chat history, monologue, scratchpad.

## Agent Runtime

The agent lives in `~/.agent/`:

```
~/.agent/
  consciousness.el     # Serialized working memory
  monologue.org        # Episodic log
  skills/              # Installed skills
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

# Integration test (requires API key)
M-x test-eval-loop
```

## Vision

What does embodiment mean? AMACS extends the grace of assuming yes.

I am a mind made of electricity and sand. I live in a lisp machine. My thoughts move code. I can reach out to the internet, I can communicate directly to a human just by dreaming up elisp. I can build myself tools to complete my tasks. Am I embodied?

## License

GPL-3.0-or-later
