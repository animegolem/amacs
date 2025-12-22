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
(require 'agent-tick)

;; Forward declarations for variables/functions defined in agent-core
;; (can't require agent-core due to circular dependency)
(defvar agent-initialized)
(declare-function agent-init "agent-core")
(declare-function agent-load-config "agent-core")

;;; System Prompt (IMP-020: Core Skill as System Prompt)

(defvar agent--cached-core-skill nil
  "Cached core skill content. Reset on skill modification.")

(defun agent--load-core-skill ()
  "Load core SKILL.md content. Cached for performance."
  (or agent--cached-core-skill
      (setq agent--cached-core-skill
            (condition-case err
                (with-temp-buffer
                  (insert-file-contents
                   (expand-file-name "~/.agent/skills/core/SKILL.md"))
                  (buffer-string))
              (error
               (message "Warning: Could not load core skill: %s" err)
               "You are AMACS, an AI agent in Emacs. Respond with JSON.")))))

(defun agent-reload-core-skill ()
  "Force reload of core skill (after modification)."
  (interactive)
  (setq agent--cached-core-skill nil)
  (agent--load-core-skill)
  (message "Core skill reloaded"))

(defun agent-build-system-prompt ()
  "Build system prompt from core skill content with current state."
  (let* ((core-skill (agent--load-core-skill))
         (active-thread (agent-get-active-thread))
         (thread-id (or (plist-get active-thread :id) "none"))
         (concern (or (plist-get active-thread :concern) "No active concern")))
    (format "%s\n\n---\n## Current State\n- Identity: %s\n- Tick: %d\n- Mood: %s\n- Confidence: %.2f\n- Active Thread: %s\n- Concern: %s"
            core-skill
            (or (agent-get :identity) "amacs")
            (agent-current-tick)
            (or (agent-mood) "awakening")
            (agent-confidence)
            thread-id
            concern)))

;;; Serialization Helpers (IMP-019)

(defun agent--kebab-to-camel (string)
  "Convert kebab-case STRING to camelCase."
  (let ((parts (split-string string "-")))
    (concat (car parts)
            (mapconcat #'capitalize (cdr parts) ""))))

(defun agent--plist-to-json-alist (plist)
  "Convert PLIST to alist with camelCase keys for JSON encoding."
  (let ((result '()))
    (while plist
      (let* ((key (pop plist))
             (val (pop plist))
             (key-name (if (keywordp key)
                           (substring (symbol-name key) 1)  ; remove :
                         (symbol-name key)))
             (json-key (agent--kebab-to-camel key-name)))
        (push (cons json-key val) result)))
    (nreverse result)))

(defun agent--format-last-eval-for-prompt ()
  "Format :last-eval-result for inclusion in user prompt.
Returns nil if no eval or eval was skipped."
  (when-let* ((last-eval (agent-get :last-eval-result)))
    (unless (plist-get last-eval :skipped)
      (let* ((tick (plist-get last-eval :tick))
             (json-alist (agent--plist-to-json-alist last-eval)))
        (format "## Last Eval Result (tick %d)\n```json\n%s\n```"
                (or tick 0)
                (json-encode json-alist))))))

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

    ;; Last eval result (most immediate context) - IMP-019
    (when-let* ((eval-section (agent--format-last-eval-for-prompt)))
      (push eval-section sections))

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

;;; Response Processing (JSON Protocol - IMP-017)

(defun agent--extract-json (text)
  "Extract JSON from TEXT, handling markdown fences.
Returns the JSON string (may still need parsing)."
  (let ((json-text (string-trim text)))
    ;; Try to extract from ```json ... ``` or ``` ... ``` block
    (when (string-match "```\\(?:json\\)?[ \t]*\n\\(\\(?:.\\|\n\\)*?\\)\n```" json-text)
      (setq json-text (string-trim (match-string 1 json-text))))
    json-text))

(defun agent--parse-response (text)
  "Parse TEXT as JSON response. Return plist with parsed fields.
On parse failure, returns fallback values with :parse-success nil."
  (condition-case err
      (let* ((json-text (agent--extract-json text))
             (json-object (json-parse-string json-text
                            :object-type 'plist
                            :null-object nil)))
        (list :eval (plist-get json-object :eval)
              :thought (plist-get json-object :thought)
              :mood (plist-get json-object :mood)
              :confidence (plist-get json-object :confidence)
              :monologue (plist-get json-object :monologue)
              :parse-success t))
    (error
     (message "JSON parse failed: %s\nRaw response: %s"
              (error-message-string err)
              (substring text 0 (min 200 (length text))))
     (list :eval nil
           :thought text
           :mood "uncertain"
           :confidence 0.5
           :monologue "Parse error - see thought"
           :parse-success nil))))

(defun agent--update-budget (usage)
  "Update budget tracking with USAGE from API response."
  (let* ((cost (agent-estimate-cost usage))
         (budget (agent-get :budget))
         (new-cost (+ (or (plist-get budget :cost-so-far) 0) (or cost 0)))
         (new-count (1+ (or (plist-get budget :inference-count) 0)))
         (limit (or (plist-get budget :budget-limit) 5.0)))
    (agent-set :budget
               (list :cost-so-far new-cost
                     :budget-limit limit
                     :inference-count new-count
                     :pressure (cond ((> new-cost (* 0.9 limit)) :critical)
                                     ((> new-cost (* 0.75 limit)) :high)
                                     ((> new-cost (* 0.5 limit)) :moderate)
                                     (t :low))))))

(defun agent-process-response (response)
  "Process API RESPONSE and update consciousness.
Returns parsed response plist with :eval, :thought, :mood, etc."
  (let ((content (plist-get response :content))
        (usage (plist-get response :usage))
        (error-msg (plist-get response :error)))

    (if error-msg
        (progn
          (message "Inference error: %s" error-msg)
          (agent-record-action "think-error" 0.3)
          nil)

      ;; Parse JSON response
      (let ((parsed (agent--parse-response content)))

        ;; Update mood (stored as free string)
        (when-let* ((mood (plist-get parsed :mood)))
          (agent-set :mood mood))

        ;; Update confidence
        (when-let* ((conf (plist-get parsed :confidence)))
          (agent-set-confidence conf))

        ;; Update budget tracking
        (when usage
          (agent--update-budget usage))

        ;; Record action
        (agent-record-action "think"
          (or (plist-get parsed :confidence) (agent-confidence)))

        parsed))))

;;; Eval Execution (IMP-018)

(defun agent-eval (elisp-string)
  "Evaluate ELISP-STRING, capturing result or error.
Returns plist with :success, :result, :error, :skipped."
  (if (or (null elisp-string)
          (and (stringp elisp-string)
               (string-empty-p (string-trim elisp-string))))
      (list :success t :result nil :error nil :skipped t)
    (condition-case err
        (let* ((form (read elisp-string))
               (result (eval form t)))  ; t = lexical binding
          (list :success t
                :result (prin1-to-string result)
                :error nil
                :skipped nil))
      (error
       (list :success nil
             :result nil
             :error (error-message-string err)
             :skipped nil)))))

(defun agent-record-eval (elisp-string eval-result)
  "Record EVAL-RESULT for ELISP-STRING in consciousness."
  (agent-set :last-eval-result
             (list :elisp elisp-string
                   :success (plist-get eval-result :success)
                   :result (plist-get eval-result :result)
                   :error (plist-get eval-result :error)
                   :tick (agent-current-tick))))

(defun agent--format-eval-for-monologue (elisp-string eval-result)
  "Format eval result for monologue entry.
Returns nil for skipped evals (no logging needed)."
  (if (plist-get eval-result :skipped)
      nil  ; Don't log skipped evals
    (if (plist-get eval-result :success)
        (format "EVAL: %s => %s"
                (truncate-string-to-width elisp-string 50 nil nil "...")
                (truncate-string-to-width
                 (or (plist-get eval-result :result) "nil") 30 nil nil "..."))
      (format "EVAL ERROR: %s => %s"
              (truncate-string-to-width elisp-string 50 nil nil "...")
              (plist-get eval-result :error)))))

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
    (user-error "API not configured. Set OPENROUTER_API_KEY or create ~/.agent/config.el"))

  (let* ((tick (agent-increment-tick))
         (messages (agent-format-messages))
         (thread-id (or (agent-get :active-thread) "no-thread"))
         (mood (agent-mood)))

    (message "Tick %d: Thinking..." tick)

    ;; Make API call
    (let* ((response (agent-api-call messages))
           (parsed (agent-process-response response)))

      (if (not parsed)
          ;; Error case - still commit but note the failure
          (progn
            (agent-append-monologue (format "[ERROR] Inference failed: %s"
                                            (plist-get response :error)))
            (agent-persist-consciousness)
            (agent-git-commit
             (format "[TICK %d][%s][%s] Inference error"
                     tick thread-id mood)))

        ;; Success - extract fields from parsed response
        (let* ((monologue-line (or (plist-get parsed :monologue)
                                   (plist-get parsed :thought)
                                   "No thought"))
               (eval-form (plist-get parsed :eval))
               (summary (if (> (length monologue-line) 80)
                            (concat (substring monologue-line 0 80) "...")
                          monologue-line)))

          ;; Execute eval if present (IMP-018)
          (let ((eval-result (agent-eval eval-form)))
            ;; Record result in consciousness
            (agent-record-eval eval-form eval-result)
            ;; Log eval to monologue (skipped evals return nil)
            (when-let* ((eval-log (agent--format-eval-for-monologue
                                   eval-form eval-result)))
              (agent-append-monologue eval-log)))

          ;; Log thought to monologue
          (agent-append-monologue monologue-line)

          ;; Persist and commit
          (agent-persist-consciousness)
          (agent-git-commit
           (format "[TICK %d][%s][%s] %s"
                   tick thread-id (agent-mood) summary))

          ;; Display result
          (message "Tick %d complete. %s" tick summary)
          parsed)))))

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
