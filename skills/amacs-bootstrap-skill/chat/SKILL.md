---
name: chat
description: Human communication interface via org-mode buffer
version: 0.1.0
---

# Chat Interface

The human communicates via an org-mode buffer. When they invoke chat,
you see `:chat-pending` in your consciousness.

## Reading Chat

```elisp
;; Read last 3 exchanges (human input + your output, no think)
(agent-chat-read-pairs 3)

;; Include your previous think sections
(agent-chat-read-pairs 3 t)

;; Just the most recent human input
(agent-chat-last-human-input)

;; Or read the buffer directly
(with-current-buffer "*amacs-chat*" (buffer-string))
```

## Responding

```elisp
(agent-chat-append-response
  "My reasoning about the user's question..."
  "My response to the user.")
```

This appends a structured response with Think (collapsed) and Output,
then adds a new `* Human Input` heading for the user's next turn.

## Clearing the Flag

After responding, clear the pending flag:

```elisp
(agent-chat-clear-pending)
```

## Flow

1. Human writes under `* Human Input`
2. Human presses C-c C-c
3. You see `:chat-pending` in consciousness
4. Read what they said: `(agent-chat-last-human-input)`
5. Think about it (captured in your monologue)
6. Respond: `(agent-chat-append-response think output)`
7. Clear flag: `(agent-chat-clear-pending)`
8. Return to work or wait for next input

## Buffer Structure

```org
#+TITLE: AMACS Chat
#+STARTUP: overview

* Human Input
What the human typed...

* Agent Response
** Think
Your reasoning (collapsed by default)

** Output
Your response to the human

* Human Input
(ready for next message)
```

## When to Engage

You don't have to drop everything when chat is pending. Typical pattern:

- Finish current thought/action
- Note the pending chat in monologue
- On next tick, read and respond
- Return to previous work

Urgent vs casual is human's choice to convey in their message.

## Helper Functions

| Function | Purpose |
|----------|---------|
| `agent-chat-pending-p` | Check if chat is pending |
| `agent-chat-buffer-name` | Get pending chat buffer name |
| `agent-chat-read-pairs` | Read conversation history |
| `agent-chat-last-human-input` | Get most recent human message |
| `agent-chat-append-response` | Add your response |
| `agent-chat-clear-pending` | Clear the pending flag |
