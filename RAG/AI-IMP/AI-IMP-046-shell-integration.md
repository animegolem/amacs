---
node_id: AI-IMP-046
tags:
  - IMP-LIST
  - Implementation
  - hub
  - shell
  - integration
kanban_status: completed
depends_on:
  - AI-IMP-045
confidence_score: 0.85
created_date: 2025-01-06
---

# AI-IMP-046: Shell Integration

## Summary

Connect hub to shell: RET opens shell, auto-refresh on tick.

**Current state:** Hub is standalone, no shell awareness.

**Target state:** Hub and shell work together seamlessly.

**Done when:** RET on Chat opens shell, hub refreshes after each tick.

### Out of Scope

- Embedding shell in hub
- Shell modifications beyond hook

### Design/Approach

**RET on Chat header opens shell**:
```elisp
(defun amacs-hub--visit-chat-header ()
  "Open or switch to *amacs-shell* buffer."
  (let ((shell-buf (get-buffer "*amacs-shell*")))
    (if shell-buf
        (progn
          (amacs-hub--ensure-other-window)
          (switch-to-buffer shell-buf)
          (goto-char (point-max)))
      ;; Start shell if not running
      (amacs-hub--ensure-other-window)
      (amacs-shell))))
```

Update `amacs-hub-visit-thing-at-point` to detect chat-section header.

**Auto-refresh hook**:
In `amacs-shell.el`, after response handling:
```elisp
(defun amacs-shell--notify-hub ()
  "Refresh hub if it exists."
  (when-let* ((buf (get-buffer "*amacs-hub*")))
    (with-current-buffer buf
      (when (eq major-mode 'amacs-hub-mode)
        (amacs-hub-refresh)))))
```

Call from `amacs-shell--handle-response` and `amacs-shell--process-json-response`.

### Files to Touch

- `harness/amacs-hub.el`: Add chat header navigation
- `harness/amacs-shell.el`: Add hub notification hook

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**?
</CRITICAL_RULE>

- [x] `amacs-hub--visit-chat-header` opens/focuses shell
- [x] Update `amacs-hub-visit-thing-at-point` for chat section
- [x] RET on Chat section header (not tick) opens shell
- [x] `amacs-shell--notify-hub` function in shell
- [x] Call notify from response handlers (both main and retry paths)
- [x] Hub refresh works even if hub minimized/background
- [x] Byte-compile without warnings (CI passes)
- [ ] Test: RET on Chat opens shell (deferred to manual testing)
- [ ] Test: Chat in shell -> hub auto-updates (deferred to manual testing)
- [ ] Test: Hub works when shell not started (deferred to manual testing)

### Acceptance Criteria

**Scenario:** RET opens shell
**GIVEN** Hub is open, shell is not
**WHEN** User moves to Chat header and presses RET
**THEN** Shell buffer opens in other window
**AND** Point is at prompt

**Scenario:** Auto-refresh
**GIVEN** Hub and shell are both open
**WHEN** User sends message in shell, agent responds
**THEN** Hub automatically shows new tick in Chat section
**AND** Status section updates

**Scenario:** Shell not running
**GIVEN** Shell has not been started
**WHEN** User presses RET on Chat in hub
**THEN** Shell is started
**AND** User can begin chatting
