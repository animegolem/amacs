---
node_id: AI-IMP-022
tags:
  - IMP
  - phase-2
  - chat
  - human-interface
  - org-mode
status: done
depends_on:
  - AI-IMP-018
implements: AI-EPIC-002
created_date: 2025-12-19
updated_date: 2025-12-21
close_date: 2025-12-21
related_files:
  - harness/agent-chat.el
  - skills/amacs-bootstrap-skill/chat/SKILL.md
confidence_score: 0.85
---

# AI-IMP-022: Chat Interface

## Objective

Create a minimal human-agent chat interface using org-mode. The agent can read and write to this buffer like any other - no special parsing infrastructure, just helper functions and a thin skill teaching the pattern.

## Design Philosophy

- **Org-mode native** - leverage existing structure (headings, folding, code blocks)
- **Agent reads like any buffer** - no magic, just elisp helpers
- **Think collapsed** - reasoning visible but not intrusive
- **Manual invocation (Phase 2)** - defer autonomous attention for Phase 3
- **Thin skill** - teach pattern, provide helpers, let behavior emerge

## Chat Buffer Structure

```org
#+TITLE: AMACS Chat
#+STARTUP: overview

* Human Input
Working on the harness. Can you check if the eval loop is working?

* Agent Response
** Think
Reading the question. User wants verification of eval loop functionality.
I should run a simple test and report results.

** Output
I'll test the eval loop now. Running (+ 2 2)...

* Human Input  
Great, that worked. Now try reading a buffer.

* Agent Response
** Think
User confirmed previous test passed. Now requesting buffer read test.

** Output
Testing buffer read with (buffer-list)...
```

### Structure Rules

- `* Human Input` - human's turn (can have sub-headings, code blocks, whatever)
- `* Agent Response` - agent's turn
  - `** Think` - reasoning trace (collapsed by default via `#+STARTUP: overview`)
  - `** Output` - response to human
- Alternating turns, but no enforcement - conversation can vary

## Minor Mode

```elisp
(define-minor-mode amacs-chat-mode
  "Minor mode for AMACS chat buffers."
  :lighter " AMACS"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") #'amacs-chat-send)
            map))

(defun amacs-chat-send ()
  "Queue chat for agent attention and trigger think."
  (interactive)
  (agent-set :chat-pending
             (list :buffer (buffer-name)
                   :file (buffer-file-name)
                   :queued-at (current-time)))
  (message "Chat queued for agent attention")
  ;; In Phase 2, immediately trigger think
  (agent-think))
```

## Helper Functions (in harness)

```elisp
(defun agent-chat-read-pairs (n &optional include-think)
  "Read last N human/agent pairs from chat buffer.
If INCLUDE-THINK is non-nil, include Think headings."
  (when-let* ((chat-info (agent-get :chat-pending))
              (buf (plist-get chat-info :buffer)))
    (with-current-buffer buf
      (org-element-map (org-element-parse-buffer) 'headline
        (lambda (hl)
          (let ((title (org-element-property :raw-value hl))
                (level (org-element-property :level hl)))
            (when (= level 1)
              (cond
               ((string= title "Human Input")
                (list :type :human
                      :content (agent--org-heading-content hl)))
               ((string= title "Agent Response")
                (list :type :agent
                      :think (when include-think
                               (agent--org-subheading-content hl "Think"))
                      :output (agent--org-subheading-content hl "Output")))))))
        nil nil 'headline))))

(defun agent-chat-append-response (think-text output-text)
  "Append agent response to chat buffer."
  (when-let* ((chat-info (agent-get :chat-pending))
              (buf (plist-get chat-info :buffer)))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert "\n* Agent Response\n")
      (insert "** Think\n" think-text "\n")
      (insert "** Output\n" output-text "\n")
      (insert "\n* Human Input\n")
      ;; Fold the Think section
      (save-excursion
        (re-search-backward "^\\*\\* Think" nil t)
        (org-cycle)))))

(defun agent-chat-clear-pending ()
  "Clear chat pending flag after agent has responded."
  (agent-set :chat-pending nil))
```

## Consciousness Integration

```elisp
;; In consciousness
:chat-pending nil
;; When human invokes:
:chat-pending (:buffer "*amacs-chat*"
               :file "~/.agent/chat.org"  
               :queued-at (24811 12345))
```

Agent sees flag in context. Decides whether/when to engage. After responding, clears flag.

## Chat Skill

Create `skills/amacs-bootstrap-skill/chat/SKILL.md`:

```markdown
---
name: chat
description: Human communication interface. Use when :chat-pending is set
  in consciousness, or when you need to communicate with the human.
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

;; Or just read the buffer directly
(with-current-buffer "*amacs-chat*" (buffer-string))
```

## Responding

```elisp
(agent-chat-append-response
  "My reasoning about the user's question..."
  "My response to the user.")
```

This appends a structured response and adds a new `* Human Input` heading
for the user's next turn.

## Flow

1. Human writes under `* Human Input`
2. Human presses C-c C-c
3. You see `:chat-pending` in consciousness
4. Read what they said: `(agent-chat-read-pairs 1)`
5. Think about it (captured in monologue)
6. Respond: `(agent-chat-append-response think output)`
7. Clear flag: `(agent-chat-clear-pending)`
8. Return to work or wait for next input

## When to Engage

You don't have to drop everything when chat is pending. Typical pattern:

- Finish current thought/action
- Note the pending chat in monologue
- On next tick, read and respond
- Return to previous work

Urgent vs casual is human's choice to convey in their message.
```

## Files to Create/Touch

```
harness/agent-chat.el                           # NEW - chat functions
skills/amacs-bootstrap-skill/chat/SKILL.md      # NEW - chat skill
harness/agent-consciousness.el                  # Add :chat-pending field
```

## Implementation Checklist

- [x] Create `amacs-chat-mode` minor mode
- [x] Implement `amacs-chat-send` (sets flag, triggers think)
- [x] Implement `agent-chat-read-pairs` (org parsing helper)
- [x] Implement `agent-chat-append-response` (structured output)
- [x] Implement `agent-chat-clear-pending`
- [x] Add `:chat-pending` to consciousness schema
- [x] Create chat skill SKILL.md
- [x] Create `agent-create-chat-buffer` (template creation)
- [x] Test: consciousness has :chat-pending field
- [x] Test: setting/clearing pending works
- [x] Test: chat buffer creation with modes
- [ ] Test: Think heading collapsed (needs interactive - deferred)

## Acceptance Criteria

**Scenario:** Human initiates chat
**GIVEN** Chat buffer with `* Human Input` content
**WHEN** Human presses C-c C-c
**THEN** `:chat-pending` is set in consciousness
**AND** `agent-think` is triggered

**Scenario:** Agent reads chat
**GIVEN** Chat buffer with 2 exchanges
**WHEN** Agent evals `(agent-chat-read-pairs 2)`
**THEN** Returns list of (:type :human :content ...) and (:type :agent :output ...)

**Scenario:** Agent responds
**GIVEN** Pending chat
**WHEN** Agent evals `(agent-chat-append-response "thinking" "response")`
**THEN** Buffer has new `* Agent Response` with Think and Output
**AND** Think is collapsed
**AND** New `* Human Input` heading is ready

**Scenario:** Think exclusion
**GIVEN** Agent reading previous exchanges
**WHEN** `(agent-chat-read-pairs 3)` (no include-think)
**THEN** Previous Think content is NOT included
**AND** Only Output content returned

## Design Note: Agent Role

The chat buffer is for coordinated human-agent communication, but it does NOT restrict the agent's role to "assistant." The agent has full agency over its environment:

- Agent can work autonomously between chat interactions
- Agent can choose when to engage with pending chat
- Agent's role encompasses the full Emacs environment, not just responding to chat
- Chat is one channel among many (buffers, files, threads)

This is collaboration, not command-response.

## Deferred

- Autonomous tick interaction (debounce, pause on typing)
- Agent-initiated ping (email notification)
- Region sending, cursor-to-point
- Multiple chat buffers
- Chat history archival

## Estimated Effort

60 minutes

## Notes

The beauty of this design is the agent can just... read org. We're not building a framework, we're teaching a pattern. The skill shows how to parse, the helpers make it convenient, but the agent could also just `(buffer-string)` and regex its way through if it wanted.

Eventually the reverse flow (agent pings human via email) uses the same primitives: agent writes to buffer, triggers notification. But that's Phase 3+ when autonomous ticks exist.
