;;; agent-game-audit.el --- Action logging and audit trail -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: amacs
;; Keywords: games, agents

;;; Commentary:

;; Every game action is logged in JSONL format for debugging and analysis.
;; This is critical for understanding arbiter decisions and agent behavior.

;;; Code:

(require 'json)
(require 'cl-lib)

(defvar agent-game-audit-file nil
  "Path to current audit log file.")

(defvar agent-game-audit-buffer "*agent-game-audit*"
  "Buffer name for displaying audit logs.")

(defun agent-game-audit-init (game-type)
  "Initialize audit log for GAME-TYPE."
  (let* ((timestamp (format-time-string "%Y-%m-%d"))
         (filename (format "%s-%s.jsonl" timestamp game-type))
         (filepath (expand-file-name filename
                                     (expand-file-name "audit"
                                                       (file-name-directory
                                                        (or load-file-name
                                                            buffer-file-name
                                                            default-directory))))))
    ;; Ensure audit directory exists
    (make-directory (file-name-directory filepath) t)
    (setq agent-game-audit-file filepath)
    (agent-game-audit-log 'game-start `((game-type . ,game-type)))))

(defun agent-game-audit-log (action data)
  "Log ACTION with DATA to audit trail.
ACTION is a symbol like 'roll, 'move, 'turn-end.
DATA is an alist of additional information."
  (when agent-game-audit-file
    (let* ((timestamp (float-time))
           (entry (append `((ts . ,timestamp)
                           (action . ,action))
                         data))
           (json-str (json-encode entry)))
      (with-temp-buffer
        (insert json-str)
        (insert "\n")
        (append-to-file (point-min) (point-max) agent-game-audit-file))
      ;; Also update audit buffer if it exists
      (when (get-buffer agent-game-audit-buffer)
        (with-current-buffer agent-game-audit-buffer
          (goto-char (point-max))
          (insert (format "[%s] %s: %s\n"
                         (format-time-string "%H:%M:%S" (seconds-to-time timestamp))
                         action
                         (json-encode data)))
          (goto-char (point-max)))))))

(defun agent-game-audit-view ()
  "Open audit log buffer for viewing."
  (interactive)
  (let ((buf (get-buffer-create agent-game-audit-buffer)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (when (and agent-game-audit-file
                 (file-exists-p agent-game-audit-file))
        (insert-file-contents agent-game-audit-file))
      (goto-char (point-max))
      (setq buffer-read-only t))
    (display-buffer buf)))

(defun agent-game-audit-query (predicate)
  "Query audit log for entries matching PREDICATE.
PREDICATE is a function that takes an entry alist and returns t if it matches."
  (when (and agent-game-audit-file
             (file-exists-p agent-game-audit-file))
    (let ((entries '()))
      (with-temp-buffer
        (insert-file-contents agent-game-audit-file)
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((line (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position)))
                 (entry (ignore-errors (json-read-from-string line))))
            (when (and entry (funcall predicate entry))
              (push entry entries)))
          (forward-line 1)))
      (nreverse entries))))

(provide 'agent-game-audit)
;;; agent-game-audit.el ends here
