;;; agent-core.el --- AMACS agent core initialization -*- lexical-binding: t; -*-

;; Copyright (C) 2025 AMACS Project
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; 
;; Main entry point for the AMACS agent harness.
;; Handles initialization (cold start vs warm start), directory setup,
;; and provides the top-level interface.
;;
;; Usage:
;;   (require 'agent-core)
;;   (agent-init)        ; Initialize (warm start if possible)
;;   (agent-tick)        ; Execute one tick
;;   (agent-status)      ; Show current state
;;
;; See: amacs-rfc-v3.md

;;; Code:

(require 'agent-consciousness)
(require 'agent-monologue)
(require 'agent-tick)

;;; Variables

(defvar agent-base-directory "~/.agent/"
  "Base directory for all agent state.")

(defvar agent-initialized nil
  "Non-nil if agent has been initialized this session.")

;;; Helper Functions

(defun agent--has-commits-p ()
  "Return t if the git repo has at least one commit."
  (condition-case nil
      (let ((result (agent--git-run "rev-parse" "HEAD")))
        (not (string-empty-p result)))
    (error nil)))

;;; Directory Setup

(defun agent--ensure-directories ()
  "Create required directories if they don't exist."
  (let ((dirs (list agent-base-directory
                    (expand-file-name "skills/" agent-base-directory))))
    (dolist (dir dirs)
      (unless (file-directory-p dir)
        (make-directory dir t)
        (message "Created directory: %s" dir)))))

(defun agent--ensure-gitignore ()
  "Create .gitignore with sensible defaults if it doesn't exist."
  (let ((gitignore (expand-file-name ".gitignore" agent-base-directory)))
    (unless (file-exists-p gitignore)
      (with-temp-file gitignore
        (insert "# AMACS Agent .gitignore\n\n")
        (insert "# Emacs backup files\n")
        (insert "*~\n")
        (insert "\\#*\\#\n")
        (insert ".\\#*\n\n")
        (insert "# Scratch files\n")
        (insert "scratch.org\n")
        (insert "*.scratch\n\n")
        (insert "# OS files\n")
        (insert ".DS_Store\n")
        (insert "Thumbs.db\n"))
      (message "Created .gitignore"))))

;;; Initialization

(defun agent-init (&optional force-cold)
  "Initialize the agent. Warm start if state exists, cold start otherwise.
With FORCE-COLD non-nil, always do cold start.

Returns the consciousness plist."
  (interactive "P")
  
  ;; Ensure directories exist
  (agent--ensure-directories)
  (agent--ensure-gitignore)
  
  ;; Initialize git repo if needed
  (agent-git-init)
  
  ;; Initialize consciousness (warm or cold)
  (agent-init-consciousness force-cold)
  
  ;; Mark as initialized
  (setq agent-initialized t)
  
  ;; Report status
  (if (agent-get :last-inference-time)
      (message "Warm start: resumed at tick %d" (agent-current-tick))
    (message "Cold start: initialized at tick 0"))
  
  ;; Initial commit if cold start and nothing committed yet
  (when (and (= (agent-current-tick) 0)
             (not (agent--has-commits-p)))
    (agent-persist-consciousness)
    (agent-git-commit "[TICK 0][genesis][:awakening] First awareness"))
  
  agent-consciousness)

(defun agent-reset ()
  "Reset agent to fresh state. Destructive - use with caution.
Preserves git history but reinitializes consciousness."
  (interactive)
  (when (yes-or-no-p "Reset agent consciousness? This cannot be undone. ")
    (agent-init t)
    (message "Agent reset to tick 0")))

;;; Convenience

(defun agent-info ()
  "Display detailed agent information."
  (interactive)
  (if (not agent-initialized)
      (message "Agent not initialized. Run (agent-init) first.")
    (with-output-to-temp-buffer "*Agent Info*"
      (princ "AMACS Agent Status\n")
      (princ "==================\n\n")
      (princ (format "Identity: %s\n" (agent-get :identity)))
      (princ (format "Current Tick: %d\n" (agent-current-tick)))
      (princ (format "Current Time: %s\n" (agent-get :current-time)))
      (princ (format "Last Inference: %s\n" (or (agent-get :last-inference-time) "never")))
      (princ (format "Long Gap Detected: %s\n" (agent-get :long-gap-detected)))
      (princ "\n")
      (princ (format "Mood: %s\n" (agent-mood)))
      (princ (format "Confidence: %.2f\n" (agent-confidence)))
      (princ (format "Active Thread: %s\n" (or (agent-active-thread) "none")))
      (princ (format "Open Threads: %d\n" (length (agent-open-threads))))
      (princ "\n")
      (princ (format "Last Commit: %s\n" (or (agent-get :last-commit) "none")))
      (princ (format "Monologue Window: %d entries\n" (agent-monologue-count)))
      (princ (format "Last Monologue: %s\n" (or (agent-monologue-head) "none")))
      (princ (format "Active Skills: %d\n" (length (agent-active-skills))))
      (princ "\n")
      (let ((budget (agent-get :budget)))
        (princ (format "Budget: $%.2f / $%.2f (%s)\n"
                       (plist-get budget :cost-so-far)
                       (plist-get budget :budget-limit)
                       (plist-get budget :pressure))))
      (princ (format "Human Review Pending: %s\n" (agent-human-review-pending-p))))))

;;; Autoload hints

;;;###autoload
(defun amacs-start ()
  "Start the AMACS agent."
  (interactive)
  (agent-init)
  (agent-info))

(provide 'agent-core)
;;; agent-core.el ends here
