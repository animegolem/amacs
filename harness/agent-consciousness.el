;;; agent-consciousness.el --- Consciousness variable management -*- lexical-binding: t; -*-

;; Copyright (C) 2025 AMACS Project
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Defines and manages the agent-consciousness variable - the agent's working
;; memory that persists across ticks. This is the core state that makes the
;; agent "itself" between inference calls.
;;
;; Uses alist format with symbol keys for clean access patterns:
;;   (alist-get 'mood agent-consciousness)
;;   (setf (alist-get 'mood agent-consciousness) "focused")
;;
;; See: amacs-rfc-v3.md Part 6 (Consciousness Variable)
;;      skills/amacs-bootstrap-skill/core/references/consciousness-schema.md
;;      STYLE.md (alist conventions)

;;; Code:

(require 'cl-lib)

;;; Variables

(defvar agent-consciousness nil
  "The agent's working memory. Persists across ticks.
This alist contains all state the agent needs to maintain identity
and continuity. Uses symbol keys for clean alist-get access.
See `agent-init-consciousness' for full schema.")

(defvar agent-consciousness-file "~/.agent/consciousness.el"
  "File where consciousness is persisted between sessions.")

(defvar agent-identity "amacs-instance-1"
  "Default identity for new consciousness instances.")

;;; Schema Definition

(defun agent--default-consciousness ()
  "Return a fresh consciousness alist with default values.
Uses symbol keys for clean alist-get access patterns."
  `((identity . ,agent-identity)

    ;; Temporal
    (current-tick . 0)
    (current-time . ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))
    (last-inference-time . nil)
    (long-gap-detected . nil)

    ;; Affective
    (mood . "awakening")
    (confidence . 0.5)

    ;; Threads
    (active-thread . nil)
    (thread-budget . 3)
    (open-threads . ())
    (completed-threads . ())

    ;; Action history
    (last-actions . ())

    ;; Context (thread-centric: buffers owned by threads, not global)
    (global-buffers . ("*agent-chat*"))
    (focus . nil)

    ;; Memory pointers
    (last-commit . nil)
    (recent-monologue . ("Consciousness initialized."))

    ;; Skills
    (active-skills . ())

    ;; Human interaction
    (human-review-requested . nil)
    (chat-pending . nil)

    ;; Inference tracking (for status line)
    (inference-in-progress . nil)
    (current-activity . nil)

    ;; Eval tracking
    (last-eval-result . nil)

    ;; Budget (nested alist)
    (budget . ((cost-so-far . 0.0)
               (budget-limit . 5.0)
               (inference-count . 0)
               (pressure . low)))

    ;; Context depth controls (agent can adjust these)
    (chat-context-depth . 5)
    (monologue-context-depth . 20)
    (global-scratchpad-depth . 5)   ; last N global headings
    (thread-scratchpad-depth . 10)  ; last N thread-specific headings
    (buffer-content-limit . 10000)  ; max chars per buffer in context

    ;; Autonomous tick control (IMP-056)
    (autonomous-tick-limit . 10)    ; max ticks before requiring human input
    (autonomous-tick-counter . 0)   ; current count (resets on human input)

    ;; API settings (agent can modify for self-tuning)
    (api-settings . ((temperature . 1.0)
                     (top-p . 1.0)
                     (top-k . nil)
                     (thinking . nil)
                     (max-tokens . 8192)
                     (model . nil)))))  ; nil = use agent-model default

;;; Persistence

(defun agent-persist-consciousness ()
  "Write consciousness to disk for crash recovery.
Called automatically at end of each tick."
  (let ((dir (file-name-directory (expand-file-name agent-consciousness-file))))
    (unless (file-directory-p dir)
      (make-directory dir t)))
  (with-temp-file (expand-file-name agent-consciousness-file)
    (insert ";;; -*- lexical-binding: t; -*-\n")
    (insert ";;; Agent consciousness - auto-generated, do not edit\n")
    (insert (format ";;; Saved at: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
    (insert "(setq agent-consciousness\n      '")
    (pp agent-consciousness (current-buffer))
    (insert ")\n"))
  (message "Consciousness persisted at tick %s" (agent-get :current-tick)))

(defun agent-load-consciousness ()
  "Load consciousness from disk. Returns t if loaded, nil if no file."
  (let ((file (expand-file-name agent-consciousness-file)))
    (if (file-exists-p file)
        (progn
          (load file t t)
          (message "Loaded consciousness from %s (tick %s)" 
                   file (agent-get :current-tick))
          t)
      nil)))

;;; Accessors

(defun agent--normalize-key (key)
  "Convert KEY to symbol for alist access.
Accepts keywords (:foo) or symbols (foo), returns symbol."
  (if (keywordp key)
      (intern (substring (symbol-name key) 1))
    key))

(defun agent-get (key &optional default)
  "Get KEY from consciousness, with optional DEFAULT.
KEY can be a keyword (:foo) or symbol (foo)."
  (alist-get (agent--normalize-key key) agent-consciousness default))

(defun agent-set (key value)
  "Set KEY to VALUE in consciousness. Returns VALUE.
KEY can be a keyword (:foo) or symbol (foo)."
  (setf (alist-get (agent--normalize-key key) agent-consciousness) value)
  value)

;;; Initialization

(defun agent-init-consciousness (&optional force)
  "Initialize consciousness. Load from disk or create fresh.
With FORCE non-nil, always create fresh (cold start)."
  (if (and (not force) (agent-load-consciousness))
      ;; Warm start - check for long gap
      (agent-check-gap)
    ;; Cold start
    (setq agent-consciousness (agent--default-consciousness))
    (message "Cold start: initialized fresh consciousness"))
  agent-consciousness)

;;; Temporal

(defun agent-current-tick ()
  "Return the current tick number."
  (or (agent-get :current-tick) 0))

(defun agent-increment-tick ()
  "Increment tick counter and update timestamps.
Returns the new tick number."
  (let ((new-tick (1+ (agent-current-tick)))
        (now (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)))
    ;; Save previous time before updating
    (agent-set :last-inference-time (agent-get :current-time))
    (agent-set :current-time now)
    (agent-set :current-tick new-tick)
    new-tick))

(defun agent--parse-iso-time (time-string)
  "Parse ISO 8601 TIME-STRING into Emacs time value.
Falls back to date-to-time if parse-iso8601-time-string unavailable."
  (condition-case nil
      (if (fboundp 'parse-iso8601-time-string)
          (parse-iso8601-time-string time-string)
        ;; Fallback for older Emacs
        (date-to-time time-string))
    (error nil)))

(defun agent-check-gap ()
  "Check if there's been a long gap since last inference.
Sets :long-gap-detected if gap > 1 hour. Returns gap in seconds or nil."
  (let ((last (agent-get :last-inference-time))
        (now (agent-get :current-time)))
    (when (and last now)
      (let ((last-time (agent--parse-iso-time last))
            (now-time (agent--parse-iso-time now)))
        (if (and last-time now-time)
            (let ((diff (float-time (time-subtract now-time last-time))))
              (agent-set :long-gap-detected (> diff 3600))
              (when (> diff 3600)
                (message "Long gap detected: %.1f hours since last tick" (/ diff 3600.0)))
              diff)
          (message "Could not parse timestamps for gap check")
          nil)))))

;;; Confidence

(defun agent-confidence ()
  "Return current confidence level (0.0-1.0)."
  (or (agent-get :confidence) 0.5))

(defun agent-set-confidence (value)
  "Set confidence to VALUE (clamped to 0.0-1.0)."
  (agent-set :confidence (max 0.0 (min 1.0 value))))

(defun agent-record-action (action-name confidence)
  "Record an action with its confidence score.
Maintains a rolling window of last 20 actions."
  (let* ((current-tick (agent-current-tick))
         (action-record `((tick . ,current-tick)
                          (action . ,action-name)
                          (confidence . ,confidence)))
         (last-actions (agent-get 'last-actions))
         (updated (cons action-record (seq-take last-actions 19))))
    (agent-set 'last-actions updated)
    (agent-set-confidence confidence)
    action-record))

;;; Mood

(defun agent-mood ()
  "Return current mood keyword."
  (or (agent-get :mood) :neutral))

(defun agent-set-mood (mood)
  "Set mood to MOOD (keyword like :focused, :curious, :stressed)."
  (agent-set :mood mood))

;;; Threads (basic accessors - full management in later IMP)

(defun agent-active-thread ()
  "Return the ID of the currently active thread, or nil."
  (agent-get :active-thread))

(defun agent-open-threads ()
  "Return list of open threads."
  (or (agent-get :open-threads) '()))

;;; Skills (basic accessor - full management in IMP-003)

(defun agent-active-skills ()
  "Return list of currently active skills."
  (or (agent-get :active-skills) '()))

;;; Human Review

(defun agent-request-human-review (reason)
  "Request human review with REASON."
  (agent-set 'human-review-requested
             `((requested . t)
               (reason . ,reason)
               (requested-at-tick . ,(agent-current-tick))))
  (message "HUMAN REVIEW REQUESTED: %s" reason))

(defun agent-clear-human-review ()
  "Clear the human review request."
  (agent-set 'human-review-requested nil))

(defun agent-human-review-pending-p ()
  "Return t if a human review is pending."
  (alist-get 'requested (agent-get 'human-review-requested)))

;;; Inference Tracking (IMP-033)

(defun agent-inference-in-progress-p ()
  "Return t if inference is currently in progress."
  (agent-get 'inference-in-progress))

(defun agent-current-activity ()
  "Return current activity description, or nil if idle."
  (agent-get 'current-activity))

(defun agent-set-activity (activity)
  "Set current ACTIVITY description. Pass nil to clear."
  (agent-set 'current-activity activity))

(defun agent-start-inference ()
  "Mark inference as in progress."
  (agent-set 'inference-in-progress t)
  (agent-set 'current-activity "Starting..."))

(defun agent-end-inference ()
  "Mark inference as complete."
  (agent-set 'inference-in-progress nil)
  (agent-set 'current-activity nil))

;;; API Settings (IMP-032)

(defvar agent-api-param-bounds
  '((temperature . (0.0 . 2.0))
    (top-p . (0.0 . 1.0))
    (top-k . (1 . 100))
    (max-tokens . (1 . 32768)))
  "Min/max bounds for numeric API parameters.")

(defun agent-get-api-param (param)
  "Get API PARAM from consciousness api-settings.
PARAM should be a symbol like temperature, top-p, thinking, model."
  (let ((settings (agent-get 'api-settings)))
    (alist-get param settings)))

(defun agent-set-api-param (param value)
  "Set API PARAM to VALUE in consciousness, with bounds checking.
Numeric params are clamped to bounds defined in `agent-api-param-bounds'.
Returns the (possibly clamped) value that was set."
  (let* ((settings (or (agent-get 'api-settings) '()))
         (bounds (alist-get param agent-api-param-bounds))
         (final-value value))
    ;; Apply bounds for numeric params
    (when (and bounds (numberp value))
      (let ((min-val (car bounds))
            (max-val (cdr bounds)))
        (when (< value min-val)
          (setq final-value min-val)
          (message "Clamped %s to minimum: %s" param min-val))
        (when (> value max-val)
          (setq final-value max-val)
          (message "Clamped %s to maximum: %s" param max-val))))
    ;; Update settings
    (setf (alist-get param settings) final-value)
    (agent-set 'api-settings settings)
    final-value))

(defun agent-api-settings ()
  "Return current API settings as alist.
Returns merged settings from consciousness, with defaults."
  (or (agent-get 'api-settings)
      '((temperature . 1.0)
        (top-p . 1.0)
        (max-tokens . 8192))))

(provide 'agent-consciousness)
;;; agent-consciousness.el ends here
