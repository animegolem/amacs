;;; agent-monologue.el --- Episodic memory via append-only log -*- lexical-binding: t; -*-

;; Copyright (C) 2025 AMACS Project
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; 
;; The monologue is the agent's stream of consciousness - an append-only
;; org file that captures thoughts, decisions, and observations.
;;
;; Two storage locations:
;; 1. Permanent: ~/.agent/monologue.org (append-only, timestamped, grepable)
;; 2. Working: :recent-monologue in consciousness (rolling window, most recent first)
;;
;; See: amacs-rfc-v3.md Part 7 (Memory Architecture)

;;; Code:

(require 'agent-consciousness)

;;; Variables

(defvar agent-monologue-file "~/.agent/monologue.org"
  "File where monologue is appended. Append-only, never truncated.")

(defvar agent-monologue-window-size 100
  "Number of recent monologue entries to keep in consciousness.
Older entries remain in the file but not in working memory.")

;;; Formatting

(defun agent-format-monologue-line (content)
  "Format CONTENT as a monologue line with timestamp and tick.
Returns string like: [2025-11-27 14:32][TICK 42] content"
  (format "[%s][TICK %d] %s"
          (format-time-string "%Y-%m-%d %H:%M")
          (agent-current-tick)
          content))

;;; Core Operations

(defun agent-append-monologue (content)
  "Append CONTENT to monologue file and update rolling window.
CONTENT is the raw thought - formatting is applied automatically.
Returns the formatted line."
  (let* ((formatted (agent-format-monologue-line content))
         (file (expand-file-name agent-monologue-file)))
    ;; Ensure directory exists
    (let ((dir (file-name-directory file)))
      (unless (file-directory-p dir)
        (make-directory dir t)))
    ;; Append to file
    (with-temp-buffer
      (insert formatted "\n")
      (append-to-file (point-min) (point-max) file))
    ;; Update rolling window in consciousness
    (agent--update-monologue-window content)
    formatted))

(defun agent--update-monologue-window (content)
  "Add CONTENT to front of :recent-monologue, maintaining window size.
Note: We store raw content in window, not formatted line."
  (let* ((current (or (agent-get :recent-monologue) '()))
         (updated (cons content current))
         (trimmed (seq-take updated agent-monologue-window-size)))
    (agent-set :recent-monologue trimmed)))

;;; Retrieval

(defun agent-recent-monologue (&optional n)
  "Return the last N monologue entries (default: 10).
Returns raw content strings, most recent first."
  (seq-take (or (agent-get :recent-monologue) '()) (or n 10)))

(defun agent-monologue-head ()
  "Return the most recent monologue entry, or nil."
  (car (agent-get :recent-monologue)))

(defun agent-monologue-count ()
  "Return number of entries in rolling window."
  (length (or (agent-get :recent-monologue) '())))

;;; File Operations (for manual inspection/search)

(defun agent-monologue-search (pattern)
  "Search monologue file for PATTERN using grep.
Returns matching lines as a list of strings."
  (let ((file (expand-file-name agent-monologue-file)))
    (if (file-exists-p file)
        (split-string
         (shell-command-to-string
          (format "grep -i %s %s" 
                  (shell-quote-argument pattern)
                  (shell-quote-argument file)))
         "\n" t)
      '())))

(defun agent-monologue-tail (&optional n)
  "Return last N lines from monologue file (default: 20).
Unlike `agent-recent-monologue', this reads from file, not memory."
  (let ((file (expand-file-name agent-monologue-file))
        (count (or n 20)))
    (if (file-exists-p file)
        (split-string
         (shell-command-to-string
          (format "tail -n %d %s" count (shell-quote-argument file)))
         "\n" t)
      '())))

(defun agent-view-monologue ()
  "Open monologue file in a buffer for viewing."
  (interactive)
  (find-file-read-only (expand-file-name agent-monologue-file)))

;;; Integration Hook

(defun agent-monologue-tick-hook ()
  "Hook to run during tick - appends placeholder monologue.
In future, LLM will provide actual content."
  (let ((content (format "Tick %d completed" (agent-current-tick))))
    (agent-append-monologue content)))

(provide 'agent-monologue)
;;; agent-monologue.el ends here
