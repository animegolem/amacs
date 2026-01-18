# AMACS RFC v4: Transition Architecture

**Status**: Draft
**Created**: 2025-01-03
**Supersedes**: amacs-rfc-v3.org (aspirational concepts deferred)

## 1. Overview

This RFC defines a simplified, testable core loop for AMACS. It prioritizes:
- **Clarity**: Single obvious I/O channel (comint shell)
- **Simplicity**: Minimal moving parts for fast iteration
- **Testability**: Full loop working before optimization

The aspirational memory systems, VM infrastructure, and advanced consciousness concepts from v3 are deferred until the basic loop proves viable.

## 2. Core Loop

```
Human types in comint → Harness captures input → Context assembled →
API call → Parse JSON response → Display reply in comint →
Serialize to org → Git commit → Ready for next input
```

### 2.1 Comint Shell

The primary interface is a comint-mode buffer (`*amacs-shell*`).

- Human types at prompt, presses enter
- Harness intercepts via `comint-input-sender` (fake process - no subprocess)
- Agent's reply appears formatted in same buffer
- Serialization to `~/.agent/agent-chat.org` happens in background

This eliminates the buffer navigation confusion from v3 testing.

### 2.2 Inference Trigger

- **Auto on enter**: Human presses RET → inference starts immediately
- No explicit C-c C-c required (unlike v3 org-mode blocks)

### 2.3 Response Format

Agent responses are JSON:

```json
{
  "eval": "(elisp-expression)" | null,
  "reply": "Text shown to human in comint",
  "mood": "focused",
  "confidence": 0.85,
  "monologue": "One line for git commit / episodic memory",
  "scratchpad": {
    "heading": "Notes on Problem X",
    "thread": "thread-id" | null,
    "content": "Content to append under heading"
  }
}
```

**Mandatory fields**: `mood`, `confidence`, `monologue`
**Optional fields**: `eval`, `reply`, `scratchpad`

### 2.4 Parse Error Handling

If response is not valid JSON:
1. Show error annotation in comint
2. Automatically retry with: "Your response was not valid JSON. Please retry."
3. Cap retries at 2 to avoid loops

## 3. Consciousness

The agent's working memory. Simplified from v3 to show only what the agent can meaningfully control or needs to know.

### 3.1 Schema

```elisp
'((identity . "amacs-instance-1")

  ;; Temporal (harness-managed, agent-visible)
  (current-tick . 42)
  (current-time . "2025-01-02T14:30:00Z")

  ;; Affective (agent-controlled)
  (mood . "focused")
  (confidence . 0.85)

  ;; Threads
  (active-thread . "rust-debugging")
  (pending-threads . ((...)))  ; summaries only

  ;; Last eval result (feedback loop)
  (last-eval-result .
    ((success . t)
     (result . "42")
     (error . nil)))

  ;; Budget (harness-managed)
  (budget .
    ((cost-so-far . 0.15)
     (budget-limit . 5.0)
     (pressure . low)))

  ;; Context depth controls (agent-controlled)
  (chat-context-depth . 5)
  (monologue-context-depth . 20)
  (global-scratchpad-depth . 5)
  (thread-scratchpad-depth . 10)

  ;; API settings (agent-controlled)
  (api-settings .
    ((temperature . 1.0)
     (top-p . 1.0)
     (thinking . nil)
     (max-tokens . 8192))))
```

### 3.2 Mutability

In pure elisp, we cannot enforce field mutability. The system prompt guides appropriate usage:
- Agent SHOULD modify: mood, confidence, depths
- Agent SHOULD NOT modify: tick, time, identity (harness-managed)

If we later add an external runtime, enforcement becomes possible.

### 3.3 Deferred Features

The following consciousness fields are deferred until the core system is validated:

- **budget**: Cost tracking and pressure system. Required for autonomous ticks where the agent runs without human oversight. Deferred until autonomous operation is implemented.
- **api-settings**: Agent-controllable temperature, top-p, max-tokens. Low lift but not required for initial validation.

## 4. Context Assembly

Each tick, the harness assembles context for the API call:

```
<agent-consciousness>
{serialized consciousness alist}
</agent-consciousness>

<chat>
Human: {message 1}
Agent: {response 1}
Human: {message 2}
Agent: {response 2}
Human: {pending message - no response yet}
</chat>

<monologue>
{last N monologue lines}
</monologue>

<scratchpad>
* Global Note 1
content...

* [thread-id] Thread-Specific Note
content...
</scratchpad>

<threads>
Active: rust-debugging
  concern: Fix ownership error
  buffers: src/main.rs, src/lib.rs
  skills: rust-mode
Pending:
  - blog-update (Write about lifetimes)
  - config-cleanup (Refactor settings)
</threads>

<buffers>
=== filename.rs (rust-mode) ===
{buffer contents}
</buffers>

<skills>
{bound skill content}
</skills>
```

### 4.1 Chat Context

- Completed pairs: read from `~/.agent/agent-chat.org`
- Pending message: the human's current input (K with no V yet)
- Depth controlled by `chat-context-depth` (number of pairs)

### 4.2 Scratchpad Context

Hybrid scoping:
- `thread: null` → global note (always visible)
- `thread: "id"` → thread-specific (visible when that thread is active)

Two depth controls:
- `global-scratchpad-depth`: last N global headings
- `thread-scratchpad-depth`: last N headings for active thread

Headings stored with org properties:
```org
* Working Notes
:PROPERTIES:
:THREAD: null
:TICK: 42
:END:
Content here...

* Parser Investigation
:PROPERTIES:
:THREAD: rust-debugging
:TICK: 45
:END:
Content here...
```

## 5. Threads

Threads organize work. Max 3 active threads.

### 5.1 API

```elisp
;; Create thread (ID-first)
(agent-create-thread "rust-debugging"
                     :concern "Fix ownership error"
                     :buffers '("src/main.rs"))

;; Switch focus
(agent-switch-thread "rust-debugging")

;; Complete with evidence
(agent-complete-thread "rust-debugging"
                       :evidence '(:output "Tests pass")
                       :learned "Use 'static for returned refs")

;; Add/remove buffers
(agent-thread-add-buffer "rust-debugging" "src/lib.rs")
(agent-thread-remove-buffer "rust-debugging" "old.rs")
```

### 5.2 Context Loading

- Active thread: buffers hydrated (full content in context)
- Pending threads: metadata only (id, concern, buffer list)

## 6. Skills

Skills provide domain knowledge. Bound to threads.

```elisp
;; List available
(agent-list-available-skills)

;; Bind to thread
(agent-bind-skill-to-thread "rust-mode")
```

Skill content appears in `<skills>` section when thread is active.

The **core skill** is special: it IS the system prompt (always loaded, defines the response format and capabilities).

## 7. Eval

The `eval` field in responses accepts elisp:

```json
{"eval": "(agent-create-thread \"new-work\" :concern \"Explore X\")"}
```

Results appear in `last-eval-result` on next tick:
```elisp
((success . t)
 (result . "thread-id-1")
 (error . nil)
 (skipped . nil))
```

On error:
```elisp
((success . nil)
 (result . nil)
 (error . "Wrong type argument: stringp, 42")
 (skipped . nil))
```

Errors are data, not failures. The agent uses them to adjust.

## 8. Persistence

### 8.1 Files

```
~/.agent/
├── consciousness.el      # Serialized state
├── agent-chat.org        # Chat history (K:V pairs under tick headings)
├── scratchpad.org        # Agent notes with thread properties
├── monologue.org         # Episodic memory
├── skills/               # Installed skills
│   └── core/             # Core skill (system prompt)
└── .git/                 # Every tick commits
```

### 8.2 Git Commits

Every tick commits all state. Format:
```
[TICK 42][rust-debugging][focused] Fixed the borrow checker issue
```

Git history is queryable memory.

### 8.3 Chat Serialization

`agent-chat.org` structure:
```org
* Tick 42
:PROPERTIES:
:TIMESTAMP: 2025-01-02T14:30:00Z
:END:

** Human
What's causing the ownership error?

** Agent
Looking at main.rs line 45...
```

## 9. Display

### 9.1 Comint Buffer

Shows formatted `reply` content only. Other fields (mood, confidence, eval) are internal.

Example session:
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

### 9.2 Hub (Deferred)

The magit-section based hub from EPIC-004 is deferred to EPIC-006. Focus on core loop first.

## 10. Implementation Phases

### EPIC-005: Core Loop ✓ Complete
1. Comint shell with fake process
2. Basic inference (send/receive, no context)
3. Response parsing and display
4. Context assembly (consciousness, chat, scratchpad)
5. Serialization (chat.org, scratchpad.org)
6. Eval execution and result feedback
7. Thread management
8. Git commit per tick

### EPIC-006: Human Interface ✓ Complete
- Hub dashboard revival
- Shell/hub integration

### EPIC-007: Consciousness-Driven Architecture (Active)
- Refactor shell to pure UI (no inline state/inference)
- Consciousness alist as single source of truth
- Reconnect to proper inference layer
- Activate skill system (thread-bound)
- Buffer hydration for active thread
- See `AI-EPIC-007-consciousness-driven-architecture.md`

### EPIC-008: Async/Optimization (Future)
- Non-blocking API calls (url-retrieve with callback)
- Evaluate if external runtime needed
- CRDT consideration for multi-process (see `amacs-rfc-concurrency.md`)
- Potential migration path if elisp hits walls

## 11. Testing Strategy

- Fresh test suite for new architecture (~80% rewrite from v3)
- Tests defined per IMP
- CI check: byte-compile + test suite
- Integration test: full loop round-trip

## 12. Open Questions

1. **Streaming**: Should replies stream token-by-token to comint, or appear all at once?
2. **Multi-line input**: How does human enter multi-line prompts? (Shift-enter? Delimiter?)
3. **Buffer display**: When agent reads a buffer, should it auto-display in a split?

These can be answered during implementation.

---

## Appendix A: System Prompt (Core Skill)

The core skill content (draft-prompt.md) becomes the system prompt. It defines:
- Response JSON format
- Available consciousness fields
- Thread/skill/scratchpad APIs
- Eval capabilities

See `skills/core/SKILL.md` for the full prompt.
