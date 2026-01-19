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

;; Forward declarations to avoid circular requires
(declare-function agent--load-thread-skills "agent-skills")
(declare-function amacs-chat--start-status-updates "agent-chat")
(declare-function amacs-chat--stop-status-updates "agent-chat")
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
         (thread-id (or (alist-get 'id active-thread) "none"))
         (concern (or (alist-get 'concern active-thread) "No active concern")))
    (format "%s\n\n---\n## Current State\n- Identity: %s\n- Tick: %d\n- Mood: %s\n- Confidence: %.2f\n- Active Thread: %s\n- Concern: %s"
            core-skill
            (or (agent-get 'identity) "amacs")
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

(defun agent--alist-to-json-alist (alist)
  "Convert ALIST with symbol keys to alist with camelCase string keys.
Suitable for JSON encoding with clean JavaScript-style keys."
  (mapcar (lambda (pair)
            (let* ((key (car pair))
                   (key-name (symbol-name key))
                   (json-key (agent--kebab-to-camel key-name)))
              (cons json-key (cdr pair))))
          alist))

(defun agent--format-last-eval-for-prompt ()
  "Format last-eval-result for inclusion in user prompt.
Returns nil if no eval or eval was skipped."
  (when-let* ((last-eval (agent-get 'last-eval-result)))
    (unless (alist-get 'skipped last-eval)
      (let* ((tick (alist-get 'tick last-eval))
             (json-alist (agent--alist-to-json-alist last-eval)))
        (format "## Last Eval Result (tick %d)\n```json\n%s\n```"
                (or tick 0)
                (json-encode json-alist))))))

;;; User Prompt (Context)

(defun agent--format-buffer-for-prompt (buffer-alist)
  "Format a hydrated BUFFER-ALIST for inclusion in prompt.
Truncates content exceeding `buffer-content-limit' from consciousness."
  (let* ((name (alist-get 'name buffer-alist))
         (content (alist-get 'content buffer-alist))
         (mode (alist-get 'mode buffer-alist))
         (limit (or (agent-get 'buffer-content-limit) 10000))
         (truncated-content
          (when (and content (> (length content) 0))
            (if (> (length content) limit)
                (concat (substring content 0 limit) "\n[...truncated...]")
              content))))
    (if truncated-content
        (format "=== %s (%s) ===\n%s\n" name mode truncated-content)
      (format "=== %s (%s) === [empty]\n" name mode))))

(defun agent--format-thread-for-prompt (thread-summary)
  "Format a THREAD-SUMMARY for the pending threads section."
  (format "- [%s] %s (started tick %d)"
          (alist-get 'id thread-summary)
          (or (alist-get 'concern thread-summary) "unnamed")
          (or (alist-get 'started-tick thread-summary) 0)))

(defun agent-build-user-prompt ()
  "Build the user prompt from current context."
  (let* ((ctx (agent-build-context))
         (active (alist-get 'active-thread ctx))
         (pending (alist-get 'pending-threads ctx))
         (global-bufs (alist-get 'global-buffers ctx))
         (monologue (alist-get 'recent-monologue ctx))
         (last-actions (alist-get 'last-actions ctx))
         (sections '()))

    ;; Last eval result (most immediate context) - IMP-019
    (when-let* ((eval-section (agent--format-last-eval-for-prompt)))
      (push eval-section sections))

    ;; Thread-bound skills - IMP-023
    (when-let* ((skills-section (agent--load-thread-skills)))
      (push skills-section sections))

    ;; Active thread buffers
    (when (and active (alist-get 'buffers active))
      (push (format "## Active Thread Buffers\n%s"
                    (mapconcat #'agent--format-buffer-for-prompt
                               (alist-get 'buffers active) "\n"))
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

    ;; Recent monologue (depth controlled by monologue-context-depth in consciousness)
    (when monologue
      (push (format "## Recent Monologue\n%s"
                    (mapconcat #'identity monologue "\n"))
            sections))

    ;; Last actions
    (when last-actions
      (push (format "## Recent Actions\n%s"
                    (mapconcat
                     (lambda (act)
                       (format "- Tick %d: %s (confidence %.2f)"
                               (alist-get 'tick act)
                               (alist-get 'action act)
                               (alist-get 'confidence act)))
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
         (budget (agent-get 'budget))
         (new-cost (+ (or (alist-get 'cost-so-far budget) 0) (or cost 0)))
         (new-count (1+ (or (alist-get 'inference-count budget) 0)))
         (limit (or (alist-get 'budget-limit budget) 5.0)))
    (agent-set 'budget
               `((cost-so-far . ,new-cost)
                 (budget-limit . ,limit)
                 (inference-count . ,new-count)
                 (pressure . ,(cond ((> new-cost (* 0.9 limit)) 'critical)
                                    ((> new-cost (* 0.75 limit)) 'high)
                                    ((> new-cost (* 0.5 limit)) 'moderate)
                                    (t 'low)))))))

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
Returns alist with success, result, error, skipped."
  (if (or (null elisp-string)
          (and (stringp elisp-string)
               (string-empty-p (string-trim elisp-string))))
      '((success . t) (result . nil) (error . nil) (skipped . t))
    (condition-case err
        (let* ((form (read elisp-string))
               (result (eval form t)))  ; t = lexical binding
          `((success . t)
            (result . ,(prin1-to-string result))
            (error . nil)
            (skipped . nil)))
      (error
       `((success . nil)
         (result . nil)
         (error . ,(error-message-string err))
         (skipped . nil))))))

(defun agent-record-eval (elisp-string eval-result)
  "Record EVAL-RESULT for ELISP-STRING in consciousness."
  (agent-set 'last-eval-result
             `((elisp . ,elisp-string)
               (success . ,(alist-get 'success eval-result))
               (result . ,(alist-get 'result eval-result))
               (error . ,(alist-get 'error eval-result))
               (skipped . ,(alist-get 'skipped eval-result))
               (tick . ,(agent-current-tick)))))

(defun agent--format-eval-for-monologue (elisp-string eval-result)
  "Format eval result for monologue entry.
Returns nil for skipped evals (no logging needed)."
  (if (alist-get 'skipped eval-result)
      nil  ; Don't log skipped evals
    (if (alist-get 'success eval-result)
        (format "EVAL: %s => %s"
                (truncate-string-to-width elisp-string 50 nil nil "...")
                (truncate-string-to-width
                 (or (alist-get 'result eval-result) "nil") 30 nil nil "..."))
      (format "EVAL ERROR: %s => %s"
              (truncate-string-to-width elisp-string 50 nil nil "...")
              (alist-get 'error eval-result)))))

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

  ;; Start inference tracking (IMP-033)
  (agent-start-inference)
  (amacs-chat--start-status-updates)

  (let* ((tick (agent-increment-tick))
         (messages (agent-format-messages))
         (thread-id (or (agent-get :active-thread) "no-thread"))
         (mood (agent-mood)))

    (message "Tick %d: Thinking..." tick)
    (agent-set-activity "Calling API...")

    ;; Make API call
    (let* ((response (agent-api-call messages))
           (_ (agent-set-activity "Processing response..."))
           (parsed (agent-process-response response)))

      (unwind-protect
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

              (agent-set-activity "Evaluating...")

              ;; Execute eval if present (IMP-018)
              (let ((eval-result (agent-eval eval-form)))
                ;; Record result in consciousness
                (agent-record-eval eval-form eval-result)
                ;; Log eval to monologue (skipped evals return nil)
                (when-let* ((eval-log (agent--format-eval-for-monologue
                                       eval-form eval-result)))
                  (agent-append-monologue eval-log)))

              (agent-set-activity "Committing...")

              ;; Log thought to monologue
              (agent-append-monologue monologue-line)

              ;; Persist and commit
              (agent-persist-consciousness)
              (agent-git-commit
               (format "[TICK %d][%s][%s] %s"
                       tick thread-id (agent-mood) summary))

              ;; Display result
              (message "Tick %d complete. %s" tick summary)
              parsed))
        ;; Cleanup - always stop inference tracking
        (agent-end-inference)
        (amacs-chat--stop-status-updates)))))

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

;;; Shell Integration (IMP-049)
;; Entry point for shell to trigger inference with human input

(declare-function agent-persistence-init "agent-persistence")
(declare-function agent-chat-load-history "agent-persistence")
(declare-function agent-scratchpad-load "agent-persistence")
(declare-function agent-scratchpad-filter-by-thread "agent-persistence")

(defun agent-infer (human-input &optional context-builder)
  "Execute inference with HUMAN-INPUT from shell.
CONTEXT-BUILDER is an optional function that returns the context string.
If not provided, uses a minimal context.

Returns plist with:
  :reply      - text for human (may be nil for silent ticks)
  :mood       - agent mood string
  :confidence - confidence 0.0-1.0
  :monologue  - one-line summary for git commit
  :eval       - elisp string to execute (may be nil)
  :scratchpad - scratchpad update plist (may be nil)
  :parse-success - t if JSON parsed correctly"
  (let* ((system-prompt (agent--build-shell-system-prompt))
         (context (if context-builder
                      (funcall context-builder human-input)
                    (agent--build-minimal-context human-input)))
         (messages `(((role . "system")
                      (content . ,system-prompt))
                     ((role . "user")
                      (content . ,context))))
         (response (agent-api-call messages))
         (content (plist-get response :content))
         (api-error (plist-get response :error)))

    (cond
     ;; API error
     (api-error
      (list :reply nil
            :mood "error"
            :confidence 0.0
            :monologue (format "API error: %s" api-error)
            :eval nil
            :scratchpad nil
            :parse-success nil
            :error api-error))

     ;; Parse response
     (content
      (agent--parse-shell-response content))

     ;; No content
     (t
      (list :reply nil
            :mood "confused"
            :confidence 0.0
            :monologue "No response from API"
            :eval nil
            :scratchpad nil
            :parse-success nil)))))

(defun agent--build-shell-system-prompt ()
  "Build system prompt for shell interaction.
Loads from core skill if available, otherwise uses fallback."
  (let ((core-skill (agent--load-core-skill)))
    ;; Core skill should contain full instructions
    ;; If not available, use minimal fallback
    (if (and core-skill (> (length core-skill) 100))
        core-skill
      "You are AMACS (Agentic Macros), an AI agent embodied in Emacs.

CRITICAL: Your response MUST be valid JSON with these fields:
- \"mood\": string (required) - your current mood (e.g., \"focused\", \"curious\")
- \"confidence\": number 0.0-1.0 (required) - confidence in your response
- \"monologue\": string (required) - one line for memory log / git commit
- \"reply\": string (optional) - your response to the human
- \"eval\": string or null (optional) - elisp to execute
- \"scratchpad\": object or null (optional) - notes to save

Example response:
{
  \"reply\": \"Hello! I'm AMACS, ready to help.\",
  \"mood\": \"curious\",
  \"confidence\": 0.85,
  \"monologue\": \"Greeted the human, exploring the system\"
}")))

(defun agent--build-minimal-context (human-input)
  "Build minimal context with HUMAN-INPUT for inference."
  (format "<agent-consciousness>
tick: %d
mood: %s
confidence: %.2f
active-thread: %s
</agent-consciousness>
Human: %s"
          (agent-current-tick)
          (or (agent-get 'mood) "awakening")
          (or (agent-get 'confidence) 0.5)
          (or (agent-get 'active-thread) "none")
          human-input))

(defun agent--parse-shell-response (text)
  "Parse TEXT as JSON response for shell.
Returns plist with :reply :mood :confidence :monologue :eval :scratchpad."
  (condition-case err
      (let* ((json-text (agent--extract-json text))
             (json-object (json-parse-string json-text
                                             :object-type 'plist
                                             :null-object nil)))
        ;; Validate required fields
        (if (and (plist-get json-object :mood)
                 (plist-get json-object :confidence)
                 (plist-get json-object :monologue))
            (list :reply (plist-get json-object :reply)
                  :mood (plist-get json-object :mood)
                  :confidence (plist-get json-object :confidence)
                  :monologue (plist-get json-object :monologue)
                  :eval (plist-get json-object :eval)
                  :scratchpad (plist-get json-object :scratchpad)
                  :parse-success t)
          ;; Missing required fields
          (list :reply nil
                :mood "confused"
                :confidence 0.5
                :monologue "Response missing required fields"
                :eval nil
                :scratchpad nil
                :parse-success nil)))
    (error
     (message "JSON parse failed: %s" (error-message-string err))
     (list :reply nil
           :mood "uncertain"
           :confidence 0.5
           :monologue "Parse error"
           :eval nil
           :scratchpad nil
           :parse-success nil))))

(provide 'agent-inference)
;;; agent-inference.el ends here

