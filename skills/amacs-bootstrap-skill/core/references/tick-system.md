# Tick System Reference

The tick is the fundamental unit of agent operation. Each tick follows: **perceive → think → eval → commit**.

## The Tick Cycle

```
1. PERCEIVE    You receive current state (consciousness, buffers, last result)
      │
      ▼
2. THINK       You reason about what to do next
      │
      ▼
3. RESPOND     You return JSON with elisp to eval (or null), thought, mood, confidence
      │
      ▼
4. EVAL        Harness evaluates your elisp, captures result
      │
      ▼
5. COMMIT      Harness commits to git with your thought as message
```

Every tick ends with a git commit. Your git history is your autobiographical memory.

## What You Receive

Each tick, you receive JSON containing:

```json
{
  "consciousness": {
    "identity": "amacs-instance-1",
    "current_tick": 142,
    "mood": "focused",
    "confidence": 0.85,
    "active_thread": "rust-debugging",
    "budget": { "cost_so_far": 2.47, "budget_limit": 5.00, "pressure": "moderate" }
  },
  "active_thread": {
    "id": "rust-debugging",
    "concern": "Ownership error in main.rs",
    "approach": "Trying lifetime annotations",
    "buffers": [
      { "name": "src/main.rs", "content": "...", "mode": "rust-mode" },
      { "name": "Cargo.toml", "content": "...", "mode": "toml-mode" }
    ]
  },
  "pending_threads": [
    { "id": "config-cleanup", "concern": "Keybinding conflicts", "approach": "..." }
  ],
  "global_buffers": [
    { "name": "*agent-chat*", "content": "...", "mode": "text-mode" }
  ],
  "recent_monologue": [
    "Investigating lifetime annotations",
    "Found the issue - missing static bound"
  ],
  "last_actions": [
    { "tick": 141, "action": "eval-elisp", "confidence": 0.85 }
  ],
  "last_eval_result": {
    "success": true,
    "result": "\"Compiled successfully\"",
    "output": null
  }
}
```

**Key points:**
- Only the active thread's buffers are hydrated (full content)
- Pending threads appear as metadata only (saves tokens)
- Global buffers (like `*agent-chat*`) are always hydrated
- `last_eval_result` shows what happened when your previous elisp ran

## What You Return

Return JSON with your response:

```json
{
  "eval": "(progn (switch-to-buffer \"src/main.rs\") (goto-char 1042) (insert \"'static \"))",
  "thought": "Adding 'static lifetime to fix the ownership error",
  "mood": "focused",
  "confidence": 0.85,
  "monologue": "Fixed ownership error by adding 'static lifetime annotation"
}
```

**Fields:**

| Field | Required | Description |
|-------|----------|-------------|
| `eval` | no | Elisp string to evaluate. Set to `null` to just think without acting. |
| `thought` | yes | Your reasoning. Logged and used in commit message. |
| `mood` | no | Updated mood keyword (`:focused`, `:curious`, `:stuck`, etc.) |
| `confidence` | no | Updated confidence 0.0-1.0 |
| `monologue` | no | Explicit monologue line (defaults to `thought` if not provided) |

## Example: Think Without Acting

Sometimes you just need to reason:

```json
{
  "eval": null,
  "thought": "The test failure suggests the issue is in the parser, not the ownership. Need to re-read the error message carefully.",
  "confidence": 0.6
}
```

## Example: Multi-Step Action

You can do multiple things in one eval:

```json
{
  "eval": "(progn\n  (find-file \"src/parser.rs\")\n  (goto-char (point-min))\n  (search-forward \"fn parse_token\")\n  (beginning-of-line))",
  "thought": "Navigating to parse_token function to investigate the parser bug",
  "mood": "curious",
  "confidence": 0.7
}
```

## Error Handling

If your elisp fails, the next tick's `last_eval_result` will show:

```json
{
  "success": false,
  "error": "Wrong type argument: stringp, 42",
  "form": "(insert 42)"
}
```

Errors are data, not failures. Use the error information to adjust your approach.

The harness will:
1. Log the error to monologue
2. Record low confidence action
3. Persist consciousness
4. Continue to next tick

You won't crash. Experiment freely.

## The Commit

Every tick commits to git in `~/.agent/`:

```
[TICK 142][rust-debugging][:focused] Adding 'static lifetime to fix ownership error
```

Format: `[TICK N][thread-id][:mood] thought-summary`

Your git history is searchable:
```bash
git log --oneline --grep="ownership"
git log --oneline --author="Amacs"
```

## Manual Tick Trigger

During development, ticks are triggered manually:

```elisp
M-x agent-think
```

Or bind to a key:
```elisp
(global-set-key (kbd "C-c a t") #'agent-think)
```

## Common Patterns

### Read a buffer
```elisp
(with-current-buffer "src/main.rs" (buffer-string))
```

### Write to a buffer
```elisp
(with-current-buffer "*scratch*" (insert "hello"))
```

### Run a shell command
```elisp
(shell-command-to-string "cargo build 2>&1")
```

### Switch threads
```elisp
(agent-switch-thread "config-cleanup")
```

### Create a new thread
```elisp
(agent-add-thread (agent-create-thread "New concern" '("file.el")) t)
```

### Complete a thread
```elisp
(agent-complete-thread "rust-debugging"
  :evidence '(:output "cargo build succeeded")
  :learned "Use 'static for returned references")
```

For more patterns, see [elisp-patterns.md](elisp-patterns.md).
