;;; agent-tick.el --- Tick cycle implementation -*- lexical-binding: t; -*-

;; Copyright (C) 2025 AMACS Project
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; 
;; Implements the tick cycle - the fundamental heartbeat of the agent.
;; Each tick: increment counter -> update timestamps -> persist -> commit
;;
;; See: amacs-rfc-v3.md Part 5 (Tick System)
;;      skills/amacs-bootstrap-skill/core/references/tick-system.md

;;; Code:

(require 'agent-consciousness)
(require 'agent-monologue)

;;; Variables

(defvar agent-git-directory "~/.agent/"
  "Directory containing the agent's git repository.")

(defvar agent-commit-author "Amacs <ghost@machine>"
  "Author string for git commits.")

(defvar agent-commit-delimiter " ‖ "
  "Delimiter for structured commit messages.
Uses U+2016 (double vertical line) which is rare in natural text.")

;;; Git Operations

(defun agent--git-repo-initialized-p ()
  "Return t if git repo exists in agent directory."
  (file-directory-p (expand-file-name ".git" agent-git-directory)))

(defun agent--git-run (&rest args)
  "Run git command with ARGS in agent directory. Returns output string."
  (let ((default-directory (expand-file-name agent-git-directory)))
    (with-temp-buffer
      (apply #'call-process "git" nil t nil args)
      (string-trim (buffer-string)))))

(defun agent-git-init ()
  "Initialize git repository in agent directory."
  (let ((dir (expand-file-name agent-git-directory)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (unless (agent--git-repo-initialized-p)
      (agent--git-run "init")
      (message "Initialized git repository in %s" dir))))

(defun agent-git-commit (message)
  "Stage all changes and commit with MESSAGE.
Returns the commit hash or nil on failure."
  (let ((default-directory (expand-file-name agent-git-directory)))
    ;; Stage all changes
    (agent--git-run "add" "-A")
    ;; Check if there's anything to commit
    (let ((status (agent--git-run "status" "--porcelain")))
      (if (string-empty-p status)
          (progn
            (message "Nothing to commit at tick %s" (agent-current-tick))
            nil)
        ;; Commit
        (agent--git-run "commit" 
                        "-m" message
                        (format "--author=%s" agent-commit-author))
        (let ((hash (agent--git-run "rev-parse" "--short" "HEAD")))
          (agent-set :last-commit hash)
          (message "Committed: %s" hash)
          hash)))))

(defun agent-get-last-commit-hash ()
  "Get the short hash of the most recent commit."
  (agent--git-run "rev-parse" "--short" "HEAD"))

;;; Commit Message Formatting

(defun agent--format-commit-message ()
  "Format the commit message for current tick.
Format: Tick N ‖ thread ‖ mood ‖ confidence ‖ monologue
Monologue is last field for greedy capture (handles pipes in text)."
  (let* ((tick (agent-current-tick))
         (thread-id (agent-active-thread))
         (thread-name (or thread-id "no-thread"))
         (mood (agent-mood))
         (confidence (agent-confidence))
         (monologue (car (agent-get :recent-monologue))))
    (format "Tick %d%s%s%s%s%s%.2f%s%s"
            tick agent-commit-delimiter
            thread-name agent-commit-delimiter
            mood agent-commit-delimiter
            confidence agent-commit-delimiter
            (or monologue "tick completed"))))

;;; Commit Parsing and Retrieval

(defun agent--parse-commit-message (msg)
  "Parse tick commit message MSG into alist.
Format: Tick N ‖ thread ‖ mood ‖ confidence ‖ monologue...
Returns alist with tick, thread, mood, confidence, monologue keys."
  (let ((parts (split-string msg agent-commit-delimiter)))
    (when (>= (length parts) 5)
      (let ((tick-str (nth 0 parts)))
        ;; Extract tick number from "Tick N"
        (when (string-match "^Tick \\([0-9]+\\)" tick-str)
          `((tick . ,(string-to-number (match-string 1 tick-str)))
            (thread . ,(nth 1 parts))
            (mood . ,(nth 2 parts))
            (confidence . ,(string-to-number (nth 3 parts)))
            ;; Monologue is everything from field 5 onward (rejoin if split)
            (monologue . ,(mapconcat #'identity (nthcdr 4 parts)
                                     agent-commit-delimiter))))))))

(defun agent-get-tick-commit (tick-number)
  "Return commit hash for TICK-NUMBER, or nil if not found."
  (let ((default-directory (expand-file-name agent-git-directory)))
    (let ((result (string-trim
                   (shell-command-to-string
                    (format "git log --oneline --grep='^Tick %d ‖' --format='%%H' -1"
                            tick-number)))))
      (unless (string-empty-p result) result))))

(defun agent-get-tick-diff (tick-number)
  "Return diff string for TICK-NUMBER commit, or nil if not found."
  (let ((hash (agent-get-tick-commit tick-number)))
    (when hash
      (let ((default-directory (expand-file-name agent-git-directory)))
        (shell-command-to-string
         (format "git show --stat --patch %s" hash))))))

(defun agent-get-tick-commit-info (tick-number)
  "Return alist with commit info for TICK-NUMBER.
Includes hash, parsed message fields, and full commit message."
  (let ((hash (agent-get-tick-commit tick-number)))
    (when hash
      (let* ((default-directory (expand-file-name agent-git-directory))
             (msg (string-trim
                   (shell-command-to-string
                    (format "git log -1 --format='%%s' %s" hash))))
             (parsed (agent--parse-commit-message msg)))
        (when parsed
          (cons `(hash . ,hash)
                (cons `(full-message . ,msg) parsed)))))))

;;; The Tick Function

(defun agent-tick ()
  "Execute one complete tick cycle.
This is the heartbeat - call manually during Phase 1.

Cycle:
1. Increment tick counter and timestamps
2. Check for long gaps
3. Append to monologue
4. Persist consciousness to disk
5. Commit to git

Returns the new tick number."
  (interactive)
  (condition-case err
      (let ((new-tick (agent-increment-tick)))
        ;; Check for gaps (updates :long-gap-detected)
        (agent-check-gap)
        
        ;; Record this as an action
        (agent-record-action "tick" 0.8)
        
        ;; Append to monologue (placeholder content for now)
        (agent-monologue-tick-hook)
        
        ;; Persist consciousness
        (agent-persist-consciousness)
        
        ;; Commit to git
        (let ((msg (agent--format-commit-message)))
          (agent-git-commit msg))
        
        (message "Tick %d complete" new-tick)
        new-tick)
    (error
     (message "Tick failed: %s" (error-message-string err))
     (agent-record-action "tick-error" 0.2)
     nil)))

;;; Inspection

(defun agent-status ()
  "Display current agent status."
  (interactive)
  (let ((tick (agent-current-tick))
        (mood (agent-mood))
        (conf (agent-confidence))
        (thread (agent-active-thread))
        (gap (agent-get :long-gap-detected))
        (review (agent-human-review-pending-p)))
    (message "Tick: %d | Mood: %s | Confidence: %.2f | Thread: %s | Gap: %s | Review: %s"
             tick mood conf (or thread "none") gap review)))

(provide 'agent-tick)
;;; agent-tick.el ends here
