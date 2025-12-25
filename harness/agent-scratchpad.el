;;; agent-scratchpad.el --- Scratchpad mode and buffer discovery -*- lexical-binding: t; -*-

;; Copyright (C) 2025 AMACS Project
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Provides mode-based buffer discovery and scratchpad for agent working notes.
;; Buffers with amacs-chat-mode or agent-scratchpad-mode are automatically
;; included in context.
;;
;; See: AI-IMP-026-mode-based-discovery

;;; Code:

(require 'org)

;;; Scratchpad Mode

;;;###autoload
(define-minor-mode agent-scratchpad-mode
  "Minor mode for AMACS scratchpad buffers.
These buffers are automatically included in agent context."
  :lighter " Scratch"
  :keymap nil)

;;; Buffer Discovery

(defun agent-find-buffers-by-mode (mode)
  "Find all buffers with minor MODE enabled.
MODE should be a symbol like amacs-chat-mode or agent-scratchpad-mode."
  (seq-filter (lambda (buf)
                (buffer-local-value mode buf))
              (buffer-list)))

(defun agent-get-mode-buffers ()
  "Return list of buffer names with agent-relevant modes enabled.
Includes buffers with amacs-chat-mode or agent-scratchpad-mode."
  (let ((chat-bufs (agent-find-buffers-by-mode 'amacs-chat-mode))
        (scratch-bufs (agent-find-buffers-by-mode 'agent-scratchpad-mode)))
    (mapcar #'buffer-name
            (delete-dups (append chat-bufs scratch-bufs)))))

;;; Scratchpad Creation

(defvar agent-scratchpad-file "~/.agent/scratchpad.org"
  "Default location for agent scratchpad file.")

(defun agent-create-scratchpad (&optional file-path)
  "Create a scratchpad buffer at FILE-PATH.
If FILE-PATH is nil, uses `agent-scratchpad-file'.
Returns the buffer."
  (let* ((path (expand-file-name (or file-path agent-scratchpad-file)))
         (buf (find-file-noselect path)))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-mode)
        (org-mode))
      (agent-scratchpad-mode 1)
      (when (= (buffer-size) 0)
        (insert "#+TITLE: Agent Scratchpad\n")
        (insert "#+STARTUP: showall\n\n")
        (insert "* Working Notes\n\n")
        (insert "Use this buffer for persistent notes across ticks.\n")
        (save-buffer)))
    buf))

(defun agent-ensure-scratchpad ()
  "Ensure default scratchpad exists and is loaded.
Creates the scratchpad if it doesn't exist."
  (let ((path (expand-file-name agent-scratchpad-file)))
    (if (get-file-buffer path)
        (get-file-buffer path)
      (agent-create-scratchpad path))))

(provide 'agent-scratchpad)
;;; agent-scratchpad.el ends here
