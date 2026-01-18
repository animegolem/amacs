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
(require 'cl-lib)
(require 'agent-consciousness)

;; Forward declarations to avoid circular requires
(declare-function agent-api-call "agent-api")
(declare-function agent-api-configured-p "agent-api")
(declare-function agent-load-config "agent-api")
(declare-function agent-persistence-init "agent-persistence")
(declare-function agent-chat-append-exchange "agent-persistence")
(declare-function agent-chat-load-history "agent-persistence")
(declare-function agent-scratchpad-append "agent-persistence")
(declare-function agent-scratchpad-load "agent-persistence")
(declare-function agent-scratchpad-filter-by-thread "agent-persistence")

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

(defvar amacs-shell--chat-history nil
  "List of completed chat exchanges.
Each entry is a plist (:human STRING :agent STRING :tick NUMBER).")

;;; Thread Management (IMP-041)
;; Thread state is now managed by consciousness alist.
;; See agent-consciousness.el for: open-threads, completed-threads, active-thread

(defvar amacs-shell--max-threads 3
  "Maximum number of open threads allowed.")

;;; Hub Integration (IMP-046)

(declare-function amacs-hub-refresh "amacs-hub")

(defun amacs-shell--notify-hub ()
  "Refresh the hub buffer if it exists.
Called after each tick to keep hub in sync."
  (when-let* ((buf (get-buffer "*amacs-hub*")))
    (with-current-buffer buf
      (when (fboundp 'amacs-hub-refresh)
        (amacs-hub-refresh)))))

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
  "You are AMACS (Agentic Macros), an AI agent embodied in Emacs.

CRITICAL: Your response MUST be valid JSON with these fields:
- \"mood\": string (required) - your current mood (e.g., \"focused\", \"curious\")
- \"confidence\": number 0.0-1.0 (required) - confidence in your response
- \"monologue\": string (required) - one line for memory log / git commit
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
```

The context below shows your current state, chat history, and any notes.
Use this to maintain continuity across turns."
  "System prompt for AMACS shell.")

;;; Context Building (IMP-038)

(defun amacs-shell--format-consciousness ()
  "Format current consciousness state for context."
  (let ((base (format "<agent-consciousness>
tick: %d
mood: %s
confidence: %.2f
chat-depth: %d
active-thread: %s
open-threads: %d
</agent-consciousness>"
                      (agent-current-tick)
                      (or (agent-get 'mood) "awakening")
                      (or (agent-get 'confidence) 0.5)
                      (or (agent-get 'chat-context-depth) 5)
                      (or (agent-get 'active-thread) "none")
                      (length (agent-get 'open-threads))))
        (eval-section (amacs-shell--format-last-eval))
        (threads-section (amacs-shell--format-threads)))
    (concat base
            (when eval-section (concat "\n\n" eval-section))
            (when threads-section (concat "\n\n" threads-section)))))

(defun amacs-shell--format-chat-history ()
  "Format chat history for context, respecting depth limit."
  (let* ((history (reverse amacs-shell--chat-history))  ; oldest first
         (depth (or (agent-get 'chat-context-depth) 5))
         (recent (if (> (length history) depth)
                     (last history depth)
                   history))
         (formatted (mapconcat
                     (lambda (exchange)
                       (format "Human: %s\nAgent: %s"
                               (plist-get exchange :human)
                               (plist-get exchange :agent)))
                     recent
                     "\n\n")))
    (if (string-empty-p formatted)
        ""
      (format "<chat>\n%s\n</chat>" formatted))))

(defun amacs-shell--format-monologue ()
  "Format recent monologue entries for context."
  (let* ((entries (mapcar (lambda (ex) (plist-get ex :monologue))
                          (last amacs-shell--chat-history 10)))
         (filtered (seq-filter #'identity entries)))
    (if filtered
        (format "<monologue>\n%s\n</monologue>"
                (mapconcat #'identity filtered "\n"))
      "")))

;; Scratchpad depth controls are now in consciousness alist.
;; See agent-consciousness.el for: global-scratchpad-depth, thread-scratchpad-depth

(defun amacs-shell--format-scratchpad ()
  "Format scratchpad notes for context."
  (require 'agent-persistence)
  (let* ((all-notes (agent-scratchpad-load))
         (filtered (agent-scratchpad-filter-by-thread all-notes
                                                      (agent-get 'active-thread)))
         ;; Separate global and thread notes
         (global-notes (seq-filter (lambda (n) (null (plist-get n :thread)))
                                   filtered))
         (thread-notes (seq-filter (lambda (n) (plist-get n :thread))
                                   filtered))
         ;; Apply depth limits (take last N)
         (recent-global (last global-notes (or (agent-get 'global-scratchpad-depth) 5)))
         (recent-thread (last thread-notes (or (agent-get 'thread-scratchpad-depth) 10)))
         (combined (append recent-global recent-thread)))
    (if combined
        (format "<scratchpad>\n%s\n</scratchpad>"
                (mapconcat (lambda (note)
                             (let ((heading (plist-get note :heading))
                                   (thread (plist-get note :thread))
                                   (content (plist-get note :content)))
                               (if thread
                                   (format "* [%s] %s\n%s" thread heading content)
                                 (format "* %s\n%s" heading content))))
                           combined
                           "\n\n"))
      "")))

(defun amacs-shell--build-context (pending-message)
  "Build full context string with PENDING-MESSAGE."
  (let ((consciousness (amacs-shell--format-consciousness))
        (chat (amacs-shell--format-chat-history))
        (monologue (amacs-shell--format-monologue))
        (scratchpad (amacs-shell--format-scratchpad)))
    (concat consciousness "\n\n"
            (unless (string-empty-p chat) (concat chat "\n\n"))
            (unless (string-empty-p monologue) (concat monologue "\n\n"))
            (unless (string-empty-p scratchpad) (concat scratchpad "\n\n"))
            "Human: " pending-message)))

(defun amacs-shell--record-exchange (human-msg agent-reply monologue)
  "Record completed exchange with HUMAN-MSG, AGENT-REPLY, and MONOLOGUE."
  (let ((tick (agent-increment-tick)))
    (push (list :human human-msg
                :agent agent-reply
                :monologue monologue
                :tick tick)
          amacs-shell--chat-history)
    ;; Persist to disk
    (require 'agent-persistence)
    (agent-chat-append-exchange tick human-msg agent-reply)))

(defun amacs-shell--handle-scratchpad (scratchpad)
  "Handle SCRATCHPAD field from agent response.
SCRATCHPAD is a plist with :heading, :thread, :content."
  (when scratchpad
    (let ((heading (plist-get scratchpad :heading))
          (thread (plist-get scratchpad :thread))
          (content (plist-get scratchpad :content)))
      (when (and heading content)
        (require 'agent-persistence)
        (agent-scratchpad-append heading thread (agent-current-tick) content)))))

;;; Eval Execution (IMP-040)

(defun amacs-shell--execute-eval (elisp-string)
  "Execute ELISP-STRING, capturing result or error.
Returns alist with success, result, error, skipped, elisp, tick."
  (let ((result
         (if (or (null elisp-string)
                 (and (stringp elisp-string)
                      (string-empty-p (string-trim elisp-string))))
             '((success . t) (result . nil) (error . nil) (skipped . t))
           (condition-case err
               (let* ((form (read elisp-string))
                      (eval-result (eval form t)))  ; t = lexical binding
                 `((success . t)
                   (result . ,(prin1-to-string eval-result))
                   (error . nil)
                   (skipped . nil)))
             (error
              `((success . nil)
                (result . nil)
                (error . ,(error-message-string err))
                (skipped . nil)))))))
    ;; Add elisp and tick to result
    `((elisp . ,elisp-string)
      (tick . ,(agent-current-tick))
      ,@result)))

(defun amacs-shell--format-eval-for-monologue (eval-result)
  "Format EVAL-RESULT for monologue entry.
Returns nil for skipped evals."
  (unless (alist-get 'skipped eval-result)
    (let ((elisp (alist-get 'elisp eval-result)))
      (if (alist-get 'success eval-result)
          (format "EVAL: %s => %s"
                  (truncate-string-to-width (or elisp "") 50 nil nil "...")
                  (truncate-string-to-width
                   (or (alist-get 'result eval-result) "nil") 30 nil nil "..."))
        (format "EVAL ERROR: %s => %s"
                (truncate-string-to-width (or elisp "") 50 nil nil "...")
                (alist-get 'error eval-result))))))

(defun amacs-shell--format-last-eval ()
  "Format last-eval-result for context display.
Returns nil if no eval or eval was skipped."
  (let ((last-eval (agent-get 'last-eval-result)))
    (when last-eval
      (unless (alist-get 'skipped last-eval)
        (let* ((tick (alist-get 'tick last-eval))
               (elisp (alist-get 'elisp last-eval))
               (success (alist-get 'success last-eval))
               (result (alist-get 'result last-eval))
               (err (alist-get 'error last-eval)))
        (format "<last-eval tick=\"%d\">\nelisp: %s\nsuccess: %s\n%s\n</last-eval>"
                (or tick 0)
                (or elisp "nil")
                (if success "true" "false")
                (if success
                    (format "result: %s" (or result "nil"))
                  (format "error: %s" (or err "unknown")))))))))

;;; Thread API (IMP-041)

(defun agent-create-thread (id &rest args)
  "Create a new thread with ID.
ARGS is a plist with optional :concern and :buffers.
Returns thread alist on success, or signals error on failure."
  (let ((concern (plist-get args :concern))
        (buffers (plist-get args :buffers))
        (open-threads (or (agent-get 'open-threads) '())))
    ;; Check for duplicate ID
    (when (cl-find id open-threads
                   :key (lambda (thr) (alist-get 'id thr))
                   :test #'equal)
      (error "Thread with ID '%s' already exists" id))
    ;; Enforce max threads
    (when (>= (length open-threads) amacs-shell--max-threads)
      (error "Maximum threads (%d) reached. Complete a thread first"
             amacs-shell--max-threads))
    ;; Create thread alist
    (let ((thread (list (cons 'id id)
                        (cons 'concern (or concern ""))
                        (cons 'buffers (or buffers '()))
                        (cons 'started-tick (agent-current-tick))
                        (cons 'hydrated nil))))
      (agent-set 'open-threads (cons thread open-threads))
      thread)))

(defun agent-switch-thread (id)
  "Switch active thread to ID.
Dehydrates old active thread, hydrates new one."
  (let* ((open-threads (or (agent-get 'open-threads) '()))
         (thread (cl-find id open-threads
                          :key (lambda (thr) (alist-get 'id thr))
                          :test #'equal))
         (active-thread (agent-get 'active-thread)))
    (unless thread
      (error "Thread '%s' not found" id))
    ;; Dehydrate old active
    (when active-thread
      (let ((old (cl-find active-thread open-threads
                          :key (lambda (thr) (alist-get 'id thr))
                          :test #'equal)))
        (when old
          (setf (alist-get 'hydrated old) nil))))
    ;; Hydrate new
    (setf (alist-get 'hydrated thread) t)
    (agent-set 'active-thread id)
    id))

(defun agent-complete-thread (id &rest args)
  "Complete thread with ID.
ARGS is a plist with optional :evidence and :learned."
  (let* ((open-threads (or (agent-get 'open-threads) '()))
         (completed-threads (or (agent-get 'completed-threads) '()))
         (thread (cl-find id open-threads
                          :key (lambda (thr) (alist-get 'id thr))
                          :test #'equal))
         (evidence (plist-get args :evidence))
         (learned (plist-get args :learned)))
    (unless thread
      (error "Thread '%s' not found" id))
    ;; Update thread with completion data
    (setf (alist-get 'completion-tick thread) (agent-current-tick))
    (setf (alist-get 'completion-evidence thread) evidence)
    (setf (alist-get 'learned thread) learned)
    (setf (alist-get 'hydrated thread) nil)
    ;; Move from open to completed
    (agent-set 'open-threads
               (cl-remove id open-threads
                          :key (lambda (thr) (alist-get 'id thr))
                          :test #'equal))
    (agent-set 'completed-threads (cons thread completed-threads))
    ;; Clear active if this was active
    (when (equal (agent-get 'active-thread) id)
      (agent-set 'active-thread nil))
    thread))

(defun agent-thread-add-buffer (id buffer-name)
  "Add BUFFER-NAME to thread ID's buffer list."
  (let* ((open-threads (or (agent-get 'open-threads) '()))
         (thread (cl-find id open-threads
                          :key (lambda (thr) (alist-get 'id thr))
                          :test #'equal)))
    (unless thread
      (error "Thread '%s' not found" id))
    (let ((buffers (alist-get 'buffers thread)))
      (unless (member buffer-name buffers)
        (setf (alist-get 'buffers thread)
              (cons buffer-name buffers))))
    (alist-get 'buffers thread)))

(defun agent-thread-remove-buffer (id buffer-name)
  "Remove BUFFER-NAME from thread ID's buffer list."
  (let* ((open-threads (or (agent-get 'open-threads) '()))
         (thread (cl-find id open-threads
                          :key (lambda (thr) (alist-get 'id thr))
                          :test #'equal)))
    (unless thread
      (error "Thread '%s' not found" id))
    (setf (alist-get 'buffers thread)
          (remove buffer-name (alist-get 'buffers thread)))
    (alist-get 'buffers thread)))

(defun agent-list-threads ()
  "Return list of open thread IDs."
  (mapcar (lambda (thr) (alist-get 'id thr))
          (or (agent-get 'open-threads) '())))

;;; Git Integration (IMP-042)

(defvar amacs-shell--git-directory "~/.agent/"
  "Directory containing the agent's git repository.")

(defvar amacs-shell--git-author "AMACS Agent <amacs@localhost>"
  "Author string for git commits.")

(defvar amacs-shell--last-commit nil
  "Hash of last git commit, or nil.")

(defun amacs-shell--git-run (&rest args)
  "Run git command with ARGS in agent directory. Returns output string."
  (let ((default-directory (expand-file-name amacs-shell--git-directory)))
    (with-output-to-string
      (apply #'call-process "git" nil standard-output nil args))))

(defun amacs-shell--git-init ()
  "Initialize git repository in agent directory if needed."
  (let ((dir (expand-file-name amacs-shell--git-directory)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (unless (file-directory-p (expand-file-name ".git" dir))
      (amacs-shell--git-run "init")
      ;; Create .gitignore
      (with-temp-file (expand-file-name ".gitignore" dir)
        (insert "config.el\n*.elc\n"))
      (amacs-shell--git-run "add" ".gitignore")
      (amacs-shell--git-run "commit" "-m" "Initialize agent repository"
                            "--author" amacs-shell--git-author))))

(defun amacs-shell--git-commit ()
  "Commit current state with tick-format message."
  (let* ((tick (agent-current-tick))
         (thread (or (agent-get 'active-thread) "no-thread"))
         (mood (or (agent-get 'mood) "awakening"))
         (confidence (or (agent-get 'confidence) 0.5))
         (monologue (or (plist-get amacs-shell--last-response :monologue) ""))
         (truncated (if (> (length monologue) 60)
                        (concat (substring monologue 0 57) "...")
                      monologue))
         (message (format "Tick %d ‖ %s ‖ %s ‖ %.2f ‖ %s"
                          tick thread mood confidence truncated)))
    ;; Stage all files
    (amacs-shell--git-run "add" "-A")
    ;; Check if there are changes
    (let ((status (string-trim (amacs-shell--git-run "status" "--porcelain"))))
      (unless (string-empty-p status)
        (amacs-shell--git-run "commit" "-m" message
                              "--author" amacs-shell--git-author)
        (setq amacs-shell--last-commit
              (string-trim (amacs-shell--git-run "rev-parse" "--short" "HEAD")))))))

(defun amacs-shell--format-threads ()
  "Format thread info for context display.
Returns nil if no threads."
  (let ((open-threads (or (agent-get 'open-threads) '()))
        (active-thread (agent-get 'active-thread)))
    (when open-threads
      (let ((active (when active-thread
                      (cl-find active-thread open-threads
                               :key (lambda (thr) (alist-get 'id thr))
                               :test #'equal)))
            (pending (cl-remove active-thread open-threads
                                :key (lambda (thr) (alist-get 'id thr))
                                :test #'equal)))
      (with-output-to-string
        (princ "<threads>\n")
        ;; Active thread (full info)
        (when active
          (princ (format "active: %s\n" (alist-get 'id active)))
          (princ (format "  concern: %s\n" (or (alist-get 'concern active) "")))
          (princ (format "  buffers: %s\n" (alist-get 'buffers active))))
        ;; Pending threads (summaries)
        (when pending
          (princ "pending:\n")
          (dolist (thr pending)
            (princ (format "  - %s: %s\n"
                           (alist-get 'id thr)
                           (or (alist-get 'concern thr) "")))))
        (princ "</threads>"))))))

(defun amacs-shell--load-history ()
  "Load chat history from disk for warm start.
Populates `amacs-shell--chat-history'. Tick is managed by consciousness."
  (require 'agent-persistence)
  (agent-persistence-init)
  ;; Ensure git repo exists
  (amacs-shell--git-init)
  (let ((loaded (agent-chat-load-history)))
    (when loaded
      ;; Chat history is oldest-first, but we push (so want newest-first)
      (setq amacs-shell--chat-history (reverse loaded))
      ;; Sync consciousness tick if chat history has higher max
      ;; (shouldn't happen normally, but protects against manual edits)
      (let ((max-tick (apply #'max (mapcar (lambda (ex) (plist-get ex :tick))
                                           loaded))))
        (when (> max-tick (agent-current-tick))
          (agent-set 'current-tick max-tick))))))

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
              ;; Success - show reply and record exchange
              (let ((reply (or (plist-get parsed :reply)
                               (format "[No reply. Mood: %s]"
                                       (plist-get parsed :mood))))
                    (monologue (plist-get parsed :monologue)))
                (setq amacs-shell--last-response parsed)
                ;; Store mood/confidence in consciousness
                (agent-set 'mood (plist-get parsed :mood))
                (agent-set 'confidence (plist-get parsed :confidence))
                ;; Execute eval if present (IMP-040)
                (let* ((eval-form (plist-get parsed :eval))
                       (eval-result (amacs-shell--execute-eval eval-form))
                       (eval-log (amacs-shell--format-eval-for-monologue eval-result)))
                  ;; Store eval result for next tick context
                  (agent-set 'last-eval-result eval-result)
                  ;; Append eval log to monologue if applicable
                  (when eval-log
                    (setq monologue (concat monologue " | " eval-log))))
                ;; Record exchange in history
                (amacs-shell--record-exchange
                 amacs-shell--pending-input
                 reply
                 monologue)
                ;; Handle scratchpad if present
                (amacs-shell--handle-scratchpad (plist-get parsed :scratchpad))
                ;; Git commit (IMP-042)
                (amacs-shell--git-commit)
                ;; Display reply
                (amacs-shell--insert-response reply)
                ;; Notify hub to refresh (IMP-046)
                (amacs-shell--notify-hub)
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
            (let ((reply (or (plist-get parsed :reply)
                             (format "[No reply. Mood: %s]"
                                     (plist-get parsed :mood))))
                  (monologue (plist-get parsed :monologue)))
              (setq amacs-shell--last-response parsed)
              ;; Store mood/confidence in consciousness
              (agent-set 'mood (plist-get parsed :mood))
              (agent-set 'confidence (plist-get parsed :confidence))
              ;; Execute eval if present (IMP-040)
              (let* ((eval-form (plist-get parsed :eval))
                     (eval-result (amacs-shell--execute-eval eval-form))
                     (eval-log (amacs-shell--format-eval-for-monologue eval-result)))
                (agent-set 'last-eval-result eval-result)
                (when eval-log
                  (setq monologue (concat monologue " | " eval-log))))
              ;; Record exchange
              (amacs-shell--record-exchange
               amacs-shell--pending-input
               reply
               monologue)
              ;; Handle scratchpad if present
              (amacs-shell--handle-scratchpad (plist-get parsed :scratchpad))
              ;; Git commit (IMP-042)
              (amacs-shell--git-commit)
              (amacs-shell--insert-response reply)
              ;; Notify hub to refresh (IMP-046)
              (amacs-shell--notify-hub)
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
  "Build messages array with USER-INPUT and full context."
  (let ((context (amacs-shell--build-context user-input)))
    `(((role . "system")
       (content . ,amacs-shell--system-prompt))
      ((role . "user")
       (content . ,context)))))

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
        ;; Initialize consciousness (warm or cold start)
        (agent-init-consciousness)
        ;; Load history for warm start
        (amacs-shell--load-history)
        ;; Set up fake process
        (amacs-shell--setup-fake-process buf)
        ;; Insert initial prompt
        (let ((inhibit-read-only t)
              (proc (get-buffer-process buf)))
          (erase-buffer)
          (insert "Welcome to AMACS shell.\n")
          (if (> (agent-current-tick) 0)
              (insert (format "Resuming from tick %d with %d exchanges.\n\n"
                              (agent-current-tick)
                              (length amacs-shell--chat-history)))
            (insert "Type your message and press RET to send.\n\n"))
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
