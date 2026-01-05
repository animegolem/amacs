;;; amacs-shell.el --- Comint-based human-agent I/O -*- lexical-binding: t; -*-

;; Copyright (C) 2025 AMACS Project
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Comint-based shell for human-agent interaction.
;; Replaces org-mode prompt blocks with a simple shell interface.
;;
;; The human types at a prompt, presses enter, and the agent's
;; response appears inline. No subprocess is used - we intercept
;; input via `comint-input-sender' and insert responses directly.
;;
;; See: AI-IMP-036-comint-shell
;;      RAG/RFC/amacs-rfc-v4-transition.md

;;; Code:

(require 'comint)
(require 'json)

;; Forward declarations to avoid circular requires
(declare-function agent-api-call "agent-api")
(declare-function agent-api-configured-p "agent-api")
(declare-function agent-load-config "agent-api")

;;; Variables

(defvar amacs-shell-prompt "amacs> "
  "Prompt string for the AMACS shell.")

(defvar amacs-shell-buffer-name "*amacs-shell*"
  "Name of the AMACS shell buffer.")

(defvar amacs-shell--pending-input nil
  "Most recent human input, waiting to be processed.")

(defvar amacs-shell--processing nil
  "Non-nil when inference is in progress.")

(defvar amacs-shell--last-response nil
  "Last parsed response from the agent.
Plist with :reply :mood :confidence :monologue :eval :scratchpad.")

(defvar amacs-shell--max-retries 2
  "Maximum number of retries for JSON parse errors.")

;;; Mode Definition

(defvar amacs-shell-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Inherit from comint-mode-map
    (set-keymap-parent map comint-mode-map)
    map)
  "Keymap for `amacs-shell-mode'.")

(define-derived-mode amacs-shell-mode comint-mode "AMACS"
  "Major mode for AMACS human-agent interaction.

This is a comint-based shell with no real subprocess.
Human input is captured and processed by the AMACS harness.

\\{amacs-shell-mode-map}"
  ;; No real process - we fake it
  (setq-local comint-input-sender #'amacs-shell--input-sender)
  (setq-local comint-process-echoes nil)
  ;; Prompt handling
  (setq-local comint-prompt-regexp (concat "^" (regexp-quote amacs-shell-prompt)))
  (setq-local comint-prompt-read-only t)
  ;; Don't try to send to a process
  (setq-local comint-input-autoexpand nil))

;;; Fake Process Setup

(defun amacs-shell--setup-fake-process (buffer)
  "Set up a fake process for BUFFER so comint functions work."
  (let ((fake-proc (start-process "amacs-fake" buffer "cat")))
    ;; Immediately stop cat from doing anything
    (set-process-query-on-exit-flag fake-proc nil)
    (set-process-filter fake-proc #'amacs-shell--process-filter)
    fake-proc))

(defun amacs-shell--process-filter (_proc _output)
  "Filter for fake process. Ignores all output from cat."
  ;; We don't want cat to echo anything
  nil)

;;; System Prompt

(defvar amacs-shell--system-prompt
  "You are AMACS, an AI agent in Emacs. Respond with valid JSON.

CRITICAL: Your response MUST be valid JSON with these fields:
- \"mood\": string (required) - your current mood
- \"confidence\": number 0.0-1.0 (required) - confidence in your response
- \"monologue\": string (required) - one line for memory log
- \"reply\": string (optional) - your response to the human
- \"eval\": string or null (optional) - elisp to execute

Example response:
```json
{
  \"reply\": \"Hello! I'm AMACS, ready to help.\",
  \"mood\": \"curious\",
  \"confidence\": 0.85,
  \"monologue\": \"Greeted the human, exploring the system\"
}
```"
  "Minimal system prompt for basic inference.
Full context assembly comes in IMP-038.")

;;; Input Handling

(defun amacs-shell--input-sender (_proc input)
  "Handle INPUT from comint. PROC is ignored (fake process).
Stores input for harness to process and triggers inference."
  (setq amacs-shell--pending-input (string-trim input))
  ;; Don't send to process (there isn't really one)
  ;; Instead, trigger our inference pipeline
  (amacs-shell--trigger-inference))

(defun amacs-shell--trigger-inference ()
  "Trigger inference for pending input."
  (when (and amacs-shell--pending-input
             (not (string-empty-p amacs-shell--pending-input))
             (not amacs-shell--processing))
    (setq amacs-shell--processing t)
    ;; Show thinking indicator
    (amacs-shell--insert-output "\n[Thinking...]\n")
    ;; Run inference (async-ish via run-at-time to let UI update)
    (run-at-time 0.01 nil #'amacs-shell--do-inference 0)))

(defun amacs-shell--do-inference (retry-count)
  "Execute inference with RETRY-COUNT for error recovery."
  (require 'agent-api)
  ;; Ensure API is configured
  (agent-load-config)
  (if (not (agent-api-configured-p))
      (progn
        (amacs-shell--insert-response
         "[Error: API not configured. Set OPENROUTER_API_KEY or create ~/.agent/config.el]")
        (setq amacs-shell--processing nil)
        (setq amacs-shell--pending-input nil))
    ;; Build messages and call API
    (let* ((messages (amacs-shell--build-messages amacs-shell--pending-input))
           (response (agent-api-call messages))
           (content (plist-get response :content))
           (api-error (plist-get response :error)))
      (cond
       ;; API error
       (api-error
        (amacs-shell--insert-response
         (format "[API Error: %s]" api-error))
        (setq amacs-shell--processing nil)
        (setq amacs-shell--pending-input nil))
       ;; Parse the response
       (content
        (let ((parsed (amacs-shell--parse-response content)))
          (if parsed
              ;; Success - show reply
              (progn
                (setq amacs-shell--last-response parsed)
                (amacs-shell--insert-response
                 (or (plist-get parsed :reply)
                     (format "[No reply. Mood: %s, Confidence: %.2f]"
                             (plist-get parsed :mood)
                             (plist-get parsed :confidence))))
                (setq amacs-shell--processing nil)
                (setq amacs-shell--pending-input nil))
            ;; Parse error - retry if possible
            (if (< retry-count amacs-shell--max-retries)
                (progn
                  (amacs-shell--update-status
                   (format "[Parse error, retrying %d/%d...]"
                           (1+ retry-count) amacs-shell--max-retries))
                  (run-at-time 0.1 nil #'amacs-shell--do-inference-retry
                               (1+ retry-count) content))
              ;; Max retries exceeded
              (amacs-shell--insert-response
               (format "[Parse error after %d retries. Raw response:\n%s]"
                       amacs-shell--max-retries
                       (substring content 0 (min 500 (length content)))))
              (setq amacs-shell--processing nil)
              (setq amacs-shell--pending-input nil)))))
       ;; No content
       (t
        (amacs-shell--insert-response "[No response from API]")
        (setq amacs-shell--processing nil)
        (setq amacs-shell--pending-input nil))))))

(defun amacs-shell--do-inference-retry (retry-count failed-response)
  "Retry inference with RETRY-COUNT, informing agent of FAILED-RESPONSE."
  (require 'agent-api)
  (let* ((retry-prompt (format "%s\n\n[Your previous response was not valid JSON. \
Please respond with ONLY valid JSON. Error in: %s]"
                               amacs-shell--pending-input
                               (substring failed-response 0 (min 200 (length failed-response)))))
         (messages (amacs-shell--build-messages retry-prompt))
         (response (agent-api-call messages))
         (content (plist-get response :content))
         (api-error (plist-get response :error)))
    (cond
     (api-error
      (amacs-shell--insert-response (format "[API Error on retry: %s]" api-error))
      (setq amacs-shell--processing nil)
      (setq amacs-shell--pending-input nil))
     (content
      (let ((parsed (amacs-shell--parse-response content)))
        (if parsed
            (progn
              (setq amacs-shell--last-response parsed)
              (amacs-shell--insert-response
               (or (plist-get parsed :reply)
                   (format "[No reply. Mood: %s]" (plist-get parsed :mood))))
              (setq amacs-shell--processing nil)
              (setq amacs-shell--pending-input nil))
          ;; Still failing - check retry count
          (if (< retry-count amacs-shell--max-retries)
              (run-at-time 0.1 nil #'amacs-shell--do-inference-retry
                           (1+ retry-count) content)
            (amacs-shell--insert-response
             (format "[Parse error after %d retries]" amacs-shell--max-retries))
            (setq amacs-shell--processing nil)
            (setq amacs-shell--pending-input nil)))))
     (t
      (amacs-shell--insert-response "[No response on retry]")
      (setq amacs-shell--processing nil)
      (setq amacs-shell--pending-input nil)))))

;;; Message Building

(defun amacs-shell--build-messages (user-input)
  "Build messages array with USER-INPUT.
Uses minimal system prompt for now (full context in IMP-038)."
  `(((role . "system")
     (content . ,amacs-shell--system-prompt))
    ((role . "user")
     (content . ,user-input))))

;;; Response Parsing

(defun amacs-shell--extract-json (text)
  "Extract JSON from TEXT, handling markdown code fences."
  (let ((trimmed (string-trim text)))
    ;; Try to extract from ```json ... ``` block
    (if (string-match "```\\(?:json\\)?[ \t]*\n?\\(\\(?:.\\|\n\\)*?\\)\n?```" trimmed)
        (string-trim (match-string 1 trimmed))
      trimmed)))

(defun amacs-shell--parse-response (text)
  "Parse TEXT as JSON response.
Returns plist with :reply :mood :confidence :monologue :eval :scratchpad,
or nil if parsing fails."
  (condition-case err
      (let* ((json-text (amacs-shell--extract-json text))
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
                  :scratchpad (plist-get json-object :scratchpad))
          ;; Missing required fields
          (message "JSON missing required fields (mood/confidence/monologue)")
          nil))
    (error
     (message "JSON parse error: %s" (error-message-string err))
     nil)))

;;; Output Handling

(defun amacs-shell--insert-output (text)
  "Insert TEXT into the shell buffer at process mark."
  (let ((buf (get-buffer amacs-shell-buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (proc (get-buffer-process buf)))
          (goto-char (process-mark proc))
          (insert text)
          (set-marker (process-mark proc) (point)))))))

(defun amacs-shell--update-status (text)
  "Update the status line (replace current line with TEXT)."
  (let ((buf (get-buffer amacs-shell-buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (proc (get-buffer-process buf)))
          (goto-char (process-mark proc))
          (delete-region (line-beginning-position) (point))
          (insert text)
          (set-marker (process-mark proc) (point)))))))

(defun amacs-shell--insert-response (response)
  "Insert agent RESPONSE and new prompt."
  (let ((buf (get-buffer amacs-shell-buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (proc (get-buffer-process buf)))
          ;; Clear the [Processing...] line
          (goto-char (process-mark proc))
          (delete-region (line-beginning-position) (point))
          ;; Insert response
          (insert response)
          (insert "\n\n")
          ;; Insert new prompt
          (let ((prompt-start (point)))
            (insert amacs-shell-prompt)
            (add-text-properties prompt-start (point)
                                 '(read-only t rear-nonsticky t front-sticky (read-only)))
            (set-marker (process-mark proc) (point))))))))

;;; Public Interface

;;;###autoload
(defun amacs-shell-start ()
  "Start or switch to the AMACS shell buffer."
  (interactive)
  (let ((buf (get-buffer-create amacs-shell-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'amacs-shell-mode)
        (amacs-shell-mode)
        ;; Set up fake process
        (amacs-shell--setup-fake-process buf)
        ;; Insert initial prompt
        (let ((inhibit-read-only t)
              (proc (get-buffer-process buf)))
          (erase-buffer)
          (insert "Welcome to AMACS shell.\n")
          (insert "Type your message and press RET to send.\n\n")
          (let ((prompt-start (point)))
            (insert amacs-shell-prompt)
            (add-text-properties prompt-start (point)
                                 '(read-only t rear-nonsticky t front-sticky (read-only)))
            (set-marker (process-mark proc) (point))))))
    (pop-to-buffer buf)
    buf))

(defun amacs-shell-get-pending-input ()
  "Return the pending human input, if any."
  amacs-shell--pending-input)

(defun amacs-shell-clear-pending-input ()
  "Clear the pending input after processing."
  (setq amacs-shell--pending-input nil))

(provide 'amacs-shell)
;;; amacs-shell.el ends here
