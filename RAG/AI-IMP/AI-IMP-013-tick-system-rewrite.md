---
node_id: AI-IMP-013
tags:
  - IMP
  - bootstrap-skill
  - code-mode
  - protocol
status: draft
depends_on:
  - AI-ADR-002
implements: AI-EPIC-001c
created_date: 2025-12-18
related_files:
  - skills/amacs-bootstrap-skill/core/references/tick-system.md
confidence_score: 0.9
---

# AI-IMP-013: tick-system.md Rewrite

## Objective

Completely rewrite tick-system.md to document the code-mode protocol. Remove all references to cortex-dispatch, vsock, Brain/Body architecture. Focus on what the agent actually sees and returns.

## Current Problems

The existing tick-system.md contains:
1. Architecture diagrams showing Brain LXC ↔ Body VM ↔ Gitea (Phase 3 details)
2. `cortex-dispatch` function with command vocabulary (tool-calling paradigm)
3. Complex context building implementation details
4. Wake logic (Phase 3 optimization)
5. ~400 lines of implementation detail the agent doesn't need

## New Structure

```markdown
# Tick System Reference

The tick is your heartbeat. Each tick: **see → think → return elisp → eval → commit**

## What You See

Each tick, you receive a JSON object:

```json
{
  "tick": 42,
  "timestamp": "2025-12-18T10:30:00Z",
  
  "consciousness": {
    "mood": "focused",
    "confidence": 0.8,
    "active_thread": "rust-debugging",
    "open_threads": [...],
    "recent_monologue": ["Last thought", "Previous thought"]
  },
  
  "buffers": [
    {
      "name": "*scratch*",
      "content": "; This is the scratch buffer\n\n",
      "point": 35,
      "modified": false
    },
    {
      "name": "main.rs", 
      "content": "fn main() {\n    println!(\"hello\");\n}\n",
      "point": 1,
      "modified": true
    }
  ],
  
  "global_buffers": [
    {
      "name": "*agent-chat*",
      "content": "User: Can you help with this?\n",
      "point": 30
    }
  ],
  
  "last_eval": {
    "success": true,
    "result": "nil",
    "output": "",
    "error": null
  },
  
  "skills": ["core", "rust-mode"]
}
```

### Key Fields

| Field | Description |
|-------|-------------|
| `tick` | Monotonic counter, increments each cycle |
| `consciousness` | Your working memory (see consciousness-schema.md) |
| `buffers` | Contents of buffers relevant to active thread |
| `global_buffers` | Always-visible buffers like *agent-chat* |
| `last_eval` | Result of your previous elisp evaluation |
| `skills` | Which skills are currently loaded |

## What You Return

You respond with a JSON object:

```json
{
  "eval": "(progn (switch-to-buffer \"*scratch*\") (insert \"hello\"))",
  "thought": "Testing basic buffer insertion",
  "mood": "curious",
  "confidence": 0.85,
  "monologue": "First real interaction with the environment"
}
```

### Required Fields

| Field | Type | Description |
|-------|------|-------------|
| `eval` | string | Elisp to evaluate. Can be any valid elisp. |
| `thought` | string | Your reasoning (for logs, not evaluated) |
| `mood` | string | Updated mood keyword |
| `confidence` | float | Your confidence in this action (0.0-1.0) |
| `monologue` | string | Line for episodic memory (becomes commit message) |

### The `eval` Field

This is your action. You can write any elisp:

```elisp
;; Simple expression
"(+ 2 2)"

;; Multiple operations
"(progn 
   (switch-to-buffer \"*scratch*\")
   (goto-char (point-max))
   (insert \"hello world\"))"

;; Define a function for later use
"(defun my-helper () 
   (message \"I exist now\"))"

;; Call your own function
"(my-helper)"

;; Shell command
"(shell-command-to-string \"ls -la\")"

;; Anything Emacs can do
"(browse-url \"file:///tmp/report.html\")"
```

There is no action vocabulary. There are no tool schemas. You write elisp.

## Eval Results

After your elisp runs, you see the result next tick:

### Success
```json
{
  "success": true,
  "result": "4",
  "output": "",
  "error": null
}
```

### Error
```json
{
  "success": false,
  "result": null,
  "output": "",
  "error": "Symbol's value as variable is void: undefined-var"
}
```

Use errors as information. Adjust and try again.

## The Git Commit

Every tick ends with a git commit:

```
[TICK 42][rust-debugging][:focused] First real interaction with the environment

 consciousness.json | updated mood, confidence
 main.rs            | inserted hello world
```

Your monologue line becomes the commit message. Git history is your autobiographical memory.

## Practical Examples

### Read a buffer
```json
{
  "eval": "(with-current-buffer \"main.rs\" (buffer-string))",
  "thought": "Need to see current file contents",
  "mood": "curious",
  "confidence": 0.9,
  "monologue": "Reading main.rs to understand the code"
}
```

### Write to a buffer
```json
{
  "eval": "(with-current-buffer \"*scratch*\" (insert \"(+ 1 2 3)\"))",
  "thought": "Adding an expression to scratch",
  "mood": "focused",
  "confidence": 0.85,
  "monologue": "Testing arithmetic in scratch buffer"
}
```

### Run a shell command
```json
{
  "eval": "(shell-command-to-string \"cargo build 2>&1\")",
  "thought": "Building the project to check for errors",
  "mood": "determined", 
  "confidence": 0.7,
  "monologue": "Running cargo build to see if my changes compile"
}
```

### Define a helper function
```json
{
  "eval": "(defun agent-insert-timestamp () (insert (format-time-string \"%Y-%m-%d %H:%M\")))",
  "thought": "Creating a reusable timestamp helper",
  "mood": "productive",
  "confidence": 0.9,
  "monologue": "Defined timestamp helper - will use this for notes"
}
```

## Error Handling

When eval fails, you get the error message. Common patterns:

| Error | Likely Cause | Try |
|-------|--------------|-----|
| "Symbol's value as variable is void" | Undefined variable | Check spelling, define it |
| "Wrong type argument" | Type mismatch | Check argument types |
| "Buffer does not exist" | Buffer name wrong | List buffers first |
| "End of buffer" | Point past content | Check buffer size |

Lower your confidence when errors occur. Persistent errors on the same action = consider switching approach.

## Tips

1. **Start simple.** `(+ 2 2)` before complex operations.
2. **Check state first.** Read buffers before modifying.
3. **Use progn for sequences.** Multiple operations in one eval.
4. **Errors are data.** They tell you what's wrong.
5. **Monologue meaningfully.** Future you reads this in git log.
```

## Acceptance Criteria

Given the rewritten tick-system.md
When an agent reads it to understand the protocol
Then it understands:
- [ ] What JSON structure it receives each tick
- [ ] What JSON structure it must return
- [ ] That `eval` can be any elisp (no vocabulary limits)
- [ ] How to interpret eval results and errors
- [ ] How the git commit captures its work

And there are NO references to:
- [ ] cortex-dispatch
- [ ] vsock
- [ ] Brain/Body/Gitea architecture
- [ ] Wake logic or classifiers

## Estimated Effort

45 minutes
