---
node_id: AI-IMP-033
tags:
  - IMP-LIST
  - Implementation
  - chat
  - status-line
  - observability
kanban_status: complete
depends_on:
  - AI-EPIC-003
confidence_score: 0.95
created_date: 2025-12-25
close_date: 2025-12-30
--- 

# AI-IMP-033: Chat Status Line

## Summary

Add an ephemeral status line to the chat buffer showing current tick number and in-progress monologue.

**Current state:** Chat buffer shows historical exchanges but no indication of current agent activity.

**Target state:** Footer/header line showing real-time tick progress and current monologue line.

**Why:**
- Human sees agent is working without switching buffers
- Provides "thinking indicator" during inference
- Matches mockup showing ephemeral status at bottom of chat

**Done when:** Chat buffer displays current tick and monologue, updates during inference, clears when idle.

### Out of Scope

- Progress bar for inference (no reliable progress info)
- Multiple status lines
- Customizable status format

### Design/Approach

**Display options:**

Option A: `header-line-format` (top of buffer)
```elisp
(setq-local header-line-format
  '(:eval (amacs-chat--status-line)))
```

Option B: Mode line segment (bottom, with mode line)
```elisp
(setq-local mode-line-format
  '(...existing... (:eval (amacs-chat--status-line))))
```

Option C: Overlay at end of buffer (inline)
```elisp
(let ((ov (make-overlay (point-max) (point-max))))
  (overlay-put ov 'after-string (amacs-chat--status-line)))
```

**Recommendation:** `header-line-format` - visible, doesn't interfere with content, standard Emacs pattern.

**Status line content:**
```elisp
(defun amacs-chat--status-line ()
  "Generate status line for chat buffer."
  (if (agent-inference-in-progress-p)
      (format "Tick %d: %s  |  Mood: %s"
              (agent-current-tick)
              (or (agent-current-activity) "thinking...")
              (alist-get 'mood agent-consciousness "?"))
    (format "Idle at tick %d  |  Mood: %s"
            (agent-current-tick)
            (alist-get 'mood agent-consciousness "?"))))
```

**Activity tracking:**
Add `:current-activity` to consciousness, updated during inference:
```elisp
(agent-set 'current-activity "Evaluating response...")
;; ... do work ...
(agent-set 'current-activity nil)  ; Clear when done
```

**Auto-refresh:**
Use timer during inference to force header line redisplay:
```elisp
(defvar amacs-chat--status-timer nil)

(defun amacs-chat--start-status-updates ()
  (setq amacs-chat--status-timer
        (run-with-timer 0.5 0.5 #'force-mode-line-update)))

(defun amacs-chat--stop-status-updates ()
  (when amacs-chat--status-timer
    (cancel-timer amacs-chat--status-timer)
    (setq amacs-chat--status-timer nil)))
```

### Files to Touch

- `harness/agent-chat.el`: Header line setup, status function
- `harness/agent-consciousness.el`: Add current-activity field
- `harness/agent-inference.el`: Update activity during inference

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**? 
</CRITICAL_RULE> 

- [ ] Add `current-activity` to consciousness schema (default nil)
- [ ] Implement `agent-inference-in-progress-p` predicate
- [ ] Implement `agent-current-activity` accessor
- [ ] Implement `amacs-chat--status-line` format function
- [ ] Set `header-line-format` in `amacs-chat-mode`
- [ ] Add activity updates in `agent-think` (start, phases, end)
- [ ] Implement `amacs-chat--start-status-updates` timer
- [ ] Implement `amacs-chat--stop-status-updates` cleanup
- [ ] Hook timer start to inference start
- [ ] Hook timer stop to inference end
- [ ] Style header line face for visibility
- [ ] Test status shows during inference
- [ ] Test status clears when idle
- [ ] Test mood updates reflected
- [ ] Ensure timer cleanup on buffer kill

### Acceptance Criteria

**Scenario:** Status shows during inference
**GIVEN** chat buffer is open
**WHEN** `agent-think` is triggered
**THEN** header line shows "Tick N: thinking..."
**AND** mood emoji/keyword is displayed
**AND** updates periodically during inference

**Scenario:** Status shows idle when done
**GIVEN** inference has completed
**THEN** header line shows "Idle at tick N"
**AND** no timer running

**Scenario:** Activity phases shown
**GIVEN** inference is in progress
**WHEN** agent moves through phases (API call, parsing, eval)
**THEN** activity description updates
**AND** human sees progress indication

**Scenario:** Clean up on buffer kill
**GIVEN** chat buffer with active status timer
**WHEN** buffer is killed
**THEN** timer is cancelled
**AND** no orphan timers remain

### Issues Encountered

1. **Timer cleanup** - Used `unwind-protect` in `agent-think` to ensure timer stops even on error. Also added `kill-buffer-hook` to clean up when last chat buffer is killed.

2. **Forward declarations** - `agent-inference.el` needs to call chat timer functions but can't require `agent-chat.el`. Used `declare-function` for `amacs-chat--start-status-updates` and `amacs-chat--stop-status-updates`.

3. **Face visibility** - Created distinct faces `amacs-chat-status-line` (idle) and `amacs-chat-status-active` (green, during inference) for clear visual feedback.
