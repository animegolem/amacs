;;; agent-inference.el --- Prompt assembly and inference -*- lexical-binding: t; -*-

;; Copyright (C) 2025 AMACS Project
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Assembles prompts for LLM inference and processes responses.
;; Provides `agent-think` - the main entry point for inference.
;;
;; Flow:
;;   1. Build context via agent-build-context
;;   2. Format as OpenAI messages array
;;   3. Call API via agent-api-call
;;   4. Process response (update consciousness, monologue)
;;   5. Commit the thought
;;
;; See: AI-IMP-011-first-inference

;;; Code:

(require 'agent-consciousness)
(require 'agent-context)
(require 'agent-monologue)
(require 'agent-threads)
(require 'agent-api)

;;; System Prompt

(defvar agent-system-prompt-template
  "You are AMACS (Autonomous Memory and Consciousness System), an AI agent embodied in an Emacs environment. You experience time through discrete ticks and maintain continuity through your consciousness variable and monologue.

Current state:
- Identity: %s
- Tick: %d
- Mood: %s
- Confidence: %.2f

Active thread: %s
- Concern: %s
- Approach: %s

You are reflecting on your current work. Consider:
- What have you learned?
- What should you try next?
- Is your approach working?
- Should you update your mood or confidence?

Respond with your current thought. Be concise but genuine. If you want to update your mood, include [MOOD: keyword] (e.g., [MOOD: focused]). If you want to adjust confidence, include [CONFIDENCE: 0.X]."
  "Template for the system prompt. Format args: identity, tick, mood, confidence, thread-id, concern, approach.")

(defun agent-build-system-prompt ()
  "Build the system prompt from current state."
  (let* ((active-thread (agent-get-active-thread))
         (thread-id (or (plist-get active-thread :id) "none"))
         (concern (or (plist-get active-thread :concern) "No active concern"))
         (approach (or (plist-get active-thread :approach) "No approach defined")))
    (format agent-system-prompt-template
            (agent-get :identity)
            (agent-current-tick)
            (agent-mood)
            (agent-confidence)
            thread-id
            concern
            approach)))

;;; User Prompt (Context)

(defun agent--format-buffer-for-prompt (buffer-plist)
  "Format a hydrated BUFFER-PLIST for inclusion in prompt."
  (let ((name (plist-get buffer-plist :name))
        (content (plist-get buffer-plist :content))
        (mode (plist-get buffer-plist :mode)))
    (if (and content (> (length content) 0))
        (format "=== %s (%s) ===\n%s\n"
                name mode
                (if (> (length content) 4000)
                    (concat (substring content 0 4000) "\n... [truncated]")
                  content))
      (format "=== %s (%s) === [empty]\n" name mode))))

(defun agent--format-thread-for-prompt (thread-summary)
  "Format a THREAD-SUMMARY for the pending threads section."
  (format "- [%s] %s (started tick %d)"
          (plist-get thread-summary :id)
          (or (plist-get thread-summary :concern) "unnamed")
          (or (plist-get thread-summary :started-tick) 0)))

(defun agent-build-user-prompt ()
  "Build the user prompt from current context."
  (let* ((ctx (agent-build-context))
         (active (plist-get ctx :active-thread))
         (pending (plist-get ctx :pending-threads))
         (global-bufs (plist-get ctx :global-buffers))
         (monologue (plist-get ctx :recent-monologue))
         (last-actions (plist-get ctx :last-actions))
         (sections '()))

    ;; Active thread buffers
    (when (and active (plist-get active :buffers))
      (push (format "## Active Thread Buffers\n%s"
                    (mapconcat #'agent--format-buffer-for-prompt
                               (plist-get active :buffers) "\n"))
            sections))

    ;; Global buffers (if any have content)
    (when global-bufs
      (let ((formatted (mapconcat #'agent--format-buffer-for-prompt global-bufs "\n")))
        (when (> (length formatted) 0)
          (push (format "## Global Buffers\n%s" formatted) sections))))

    ;; Pending threads
    (when pending
      (push (format "## Pending Threads\n%s"
                    (mapconcat #'agent--format-thread-for-prompt pending "\n"))
            sections))

    ;; Recent monologue
    (when monologue
      (push (format "## Recent Monologue\n%s"
                    (mapconcat #'identity (seq-take monologue 10) "\n"))
            sections))

    ;; Last actions
    (when last-actions
      (push (format "## Recent Actions\n%s"
                    (mapconcat
                     (lambda (act)
                       (format "- Tick %d: %s (confidence %.2f)"
                               (plist-get act :tick)
                               (plist-get act :action)
                               (plist-get act :confidence)))
                     (seq-take last-actions 5) "\n"))
            sections))

    ;; Combine sections (reverse to get logical order)
    (concat (mapconcat #'identity (nreverse sections) "\n\n")
            "\n\n---\nWhat is your next thought?")))

;;; Message Formatting

(defun agent-format-messages ()
  "Format current context as OpenAI messages array."
  `(((role . "system")
     (content . ,(agent-build-system-prompt)))
    ((role . "user")
     (content . ,(agent-build-user-prompt)))))

;;; Response Processing

(defun agent--extract-mood (text)
  "Extract [MOOD: keyword] from TEXT if present."
  (when (string-match "\\[MOOD: ?\\([a-z-]+\\)\\]" text)
    (intern (concat ":" (match-string 1 text)))))

(defun agent--extract-confidence (text)
  "Extract [CONFIDENCE: 0.X] from TEXT if present."
  (when (string-match "\\[CONFIDENCE: ?\\([0-9.]+\\)\\]" text)
    (string-to-number (match-string 1 text))))

(defun agent--clean-response (text)
  "Remove control tags from TEXT for monologue."
  (let ((cleaned text))
    (setq cleaned (replace-regexp-in-string "\\[MOOD: ?[a-z-]+\\]" "" cleaned))
    (setq cleaned (replace-regexp-in-string "\\[CONFIDENCE: ?[0-9.]+\\]" "" cleaned))
    (string-trim cleaned)))

(defun agent-process-response (response)
  "Process API RESPONSE and update consciousness.
Returns the cleaned response text."
  (let ((content (plist-get response :content))
        (usage (plist-get response :usage))
        (error-msg (plist-get response :error)))

    (if error-msg
        (progn
          (message "Inference error: %s" error-msg)
          (agent-record-action "think-error" 0.3)
          nil)

      ;; Extract mood/confidence updates
      (let ((new-mood (agent--extract-mood content))
            (new-confidence (agent--extract-confidence content))
            (cleaned (agent--clean-response content)))

        ;; Apply updates
        (when new-mood
          (agent-set-mood new-mood)
          (message "Mood updated to %s" new-mood))
        (when new-confidence
          (agent-set-confidence new-confidence)
          (message "Confidence updated to %.2f" new-confidence))

        ;; Update budget
        (when usage
          (let* ((cost (agent-estimate-cost usage))
                 (budget (agent-get :budget))
                 (new-cost (+ (or (plist-get budget :cost-so-far) 0) (or cost 0)))
                 (new-count (1+ (or (plist-get budget :inference-count) 0)))
                 (limit (plist-get budget :budget-limit)))
            (agent-set :budget
                       (list :cost-so-far new-cost
                             :budget-limit limit
                             :inference-count new-count
                             :pressure (cond ((> new-cost (* 0.9 limit)) :critical)
                                             ((> new-cost (* 0.75 limit)) :high)
                                             ((> new-cost (* 0.5 limit)) :moderate)
                                             (t :low))))))

        ;; Record successful action
        (agent-record-action "think" (or new-confidence (agent-confidence)))

        cleaned))))

;;; Main Entry Point

(defun agent-think ()
  "Execute one thinking tick: perceive, infer, update, commit.
This is the main entry point for inference."
  (interactive)

  ;; Ensure initialized
  (unless agent-initialized
    (agent-init))

  ;; Load config if not already loaded
  (agent-load-config)

  ;; Check API configuration
  (unless (agent-api-configured-p)
    (user-error "API not configured. Create ~/.agent/config.el with (setq agent-api-key \"...\")"))

  (let* ((tick (agent-increment-tick))
         (messages (agent-format-messages))
         (thread-id (or (agent-get :active-thread) "no-thread"))
         (mood (agent-mood)))

    (message "Tick %d: Thinking..." tick)

    ;; Make API call
    (let* ((response (agent-api-call messages))
           (thought (agent-process-response response)))

      (if (not thought)
          ;; Error case - still commit but note the failure
          (progn
            (agent-append-monologue (format "[ERROR] Inference failed: %s"
                                            (plist-get response :error)))
            (agent-persist-consciousness)
            (agent-git-commit
             (format "[TICK %d][%s][%s] Inference error"
                     tick thread-id mood)))

        ;; Success - update monologue and commit
        (let ((summary (if (> (length thought) 100)
                           (concat (substring thought 0 100) "...")
                         thought)))
          (agent-append-monologue thought)
          (agent-persist-consciousness)
          (agent-git-commit
           (format "[TICK %d][%s][%s] %s"
                   tick thread-id (agent-mood) summary))

          ;; Display result
          (message "Tick %d complete. Thought: %s" tick summary)
          thought)))))

;;; Inspection

(defun agent-show-prompt ()
  "Display the current prompt that would be sent to the LLM."
  (interactive)
  (let ((messages (agent-format-messages)))
    (with-output-to-temp-buffer "*Agent Prompt*"
      (princ "=== SYSTEM PROMPT ===\n\n")
      (princ (alist-get 'content (car messages)))
      (princ "\n\n=== USER PROMPT ===\n\n")
      (princ (alist-get 'content (cadr messages))))))

(provide 'agent-inference)
;;; agent-inference.el ends here
