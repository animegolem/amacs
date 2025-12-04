;;; test-harness.el --- Manual tests for AMACS harness -*- lexical-binding: t; -*-

;; Copyright (C) 2025 AMACS Project
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Manual test script for IMP-001 (Heartbeat).
;; Run with: emacs -Q -l test-harness.el
;;
;; Tests:
;; 1. Cold start creates consciousness, tick 0 commits
;; 2. 10 ticks produce 10 commits with incrementing tick numbers
;; 3. Warm start resumes from correct tick
;; 4. Long gap detection works

;;; Code:

;; Add harness to load path
(add-to-list 'load-path (file-name-directory load-file-name))

(require 'agent-core)
(require 'agent-monologue)

;;; Test Helpers

(defvar test-results '()
  "Accumulator for test results.")

(defun test-log (name result &optional message)
  "Log test NAME with RESULT (t or nil) and optional MESSAGE."
  (push (list name result message) test-results)
  (message "%s: %s%s" 
           (if result "PASS" "FAIL")
           name
           (if message (format " - %s" message) "")))

(defun test-summary ()
  "Print test summary."
  (let ((passed (length (seq-filter #'cadr test-results)))
        (total (length test-results)))
    (message "\n==================")
    (message "Tests: %d/%d passed" passed total)
    (message "==================\n")
    (when (< passed total)
      (message "Failed tests:")
      (dolist (test test-results)
        (unless (cadr test)
          (message "  - %s: %s" (car test) (or (caddr test) "no details")))))))

;;; Clean Slate

(defun test-clean-slate ()
  "Remove ~/.agent/ for clean test. DESTRUCTIVE."
  (let ((dir (expand-file-name "~/.agent/")))
    (when (file-directory-p dir)
      (delete-directory dir t))
    (message "Cleaned ~/.agent/")))

;;; Tests

(defun test-cold-start ()
  "Test: Cold start creates consciousness and initial commit."
  (message "\n--- Test: Cold Start ---")
  
  ;; Clean slate
  (test-clean-slate)
  
  ;; Initialize
  (agent-init)
  
  ;; Check consciousness exists
  (test-log "consciousness-exists" 
            (not (null agent-consciousness))
            (format "agent-consciousness is %s" (if agent-consciousness "set" "nil")))
  
  ;; Check tick is 0
  (test-log "tick-is-zero"
            (= (agent-current-tick) 0)
            (format "tick = %d" (agent-current-tick)))
  
  ;; Check directories created
  (test-log "agent-dir-exists"
            (file-directory-p "~/.agent/")
            "~/.agent/ directory")
  
  ;; Check git initialized
  (test-log "git-initialized"
            (file-directory-p "~/.agent/.git/")
            ".git directory")
  
  ;; Check consciousness file exists
  (test-log "consciousness-file-exists"
            (file-exists-p "~/.agent/consciousness.el")
            "consciousness.el file")
  
  ;; Check initial commit
  (let* ((default-directory (expand-file-name "~/.agent/"))
         (log (shell-command-to-string "git log --oneline -1")))
    (test-log "initial-commit-exists"
              (string-match-p "TICK 0" log)
              (format "git log: %s" (string-trim log)))))

(defun test-tick-cycle ()
  "Test: 10 ticks produce 10 commits with incrementing numbers."
  (message "\n--- Test: Tick Cycle ---")
  
  ;; Run 10 ticks
  (dotimes (i 10)
    (agent-tick))
  
  ;; Check tick counter
  (test-log "tick-counter"
            (= (agent-current-tick) 10)
            (format "tick = %d (expected 10)" (agent-current-tick)))
  
  ;; Check git log has 11 commits (initial + 10 ticks)
  (let* ((default-directory (expand-file-name "~/.agent/"))
         (count (string-to-number 
                 (shell-command-to-string "git rev-list --count HEAD"))))
    (test-log "commit-count"
              (= count 11)
              (format "%d commits (expected 11)" count)))
  
  ;; Check commits have incrementing tick numbers
  (let* ((default-directory (expand-file-name "~/.agent/"))
         (log (shell-command-to-string "git log --oneline")))
    (test-log "commit-messages"
              (and (string-match-p "TICK 10" log)
                   (string-match-p "TICK 5" log)
                   (string-match-p "TICK 1" log))
              "commits contain TICK markers")))

(defun test-warm-start ()
  "Test: Restart resumes from correct tick."
  (message "\n--- Test: Warm Start ---")
  
  ;; Clear loaded consciousness
  (setq agent-consciousness nil)
  (setq agent-initialized nil)
  
  ;; Re-init (should warm start)
  (agent-init)
  
  ;; Check tick resumed
  (test-log "warm-start-tick"
            (= (agent-current-tick) 10)
            (format "tick = %d (expected 10)" (agent-current-tick)))
  
  ;; Run one more tick
  (agent-tick)
  
  ;; Check it's now 11
  (test-log "tick-after-warm"
            (= (agent-current-tick) 11)
            (format "tick = %d (expected 11)" (agent-current-tick))))

(defun test-long-gap ()
  "Test: Long gap detection triggers."
  (message "\n--- Test: Long Gap Detection ---")
  
  ;; Manually set old timestamp to simulate gap
  (agent-set :last-inference-time "2020-01-01T00:00:00Z")
  (agent-set :current-time (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))
  
  ;; Check gap
  (agent-check-gap)
  
  (test-log "long-gap-detected"
            (agent-get :long-gap-detected)
            (format ":long-gap-detected = %s" (agent-get :long-gap-detected))))

(defun test-monologue-append ()
  "Test: Monologue append creates file and updates window."
  (message "\n--- Test: Monologue Append ---")
  
  ;; Check file was created
  (test-log "monologue-file-exists"
            (file-exists-p "~/.agent/monologue.org")
            "monologue.org file")
  
  ;; Check content in file
  (let* ((content (with-temp-buffer
                    (insert-file-contents "~/.agent/monologue.org")
                    (buffer-string))))
    (test-log "monologue-has-entries"
              (string-match-p "TICK" content)
              "file contains TICK entries"))
  
  ;; Check rolling window
  (let ((window (agent-get :recent-monologue)))
    (test-log "monologue-window-populated"
              (> (length window) 0)
              (format "window has %d entries" (length window)))))

(defun test-monologue-window-size ()
  "Test: Rolling window doesn't exceed configured size."
  (message "\n--- Test: Monologue Window Size ---")
  
  ;; Current window size after previous tests
  (let ((window-size (length (agent-get :recent-monologue))))
    (test-log "window-size-bounded"
              (<= window-size agent-monologue-window-size)
              (format "window %d <= max %d" window-size agent-monologue-window-size))))

(defun test-commit-includes-monologue ()
  "Test: Git commits include monologue content."
  (message "\n--- Test: Commit Includes Monologue ---")
  
  (let* ((default-directory (expand-file-name "~/.agent/"))
         (log (shell-command-to-string "git log --oneline -1")))
    (test-log "commit-has-tick-content"
              (string-match-p "Tick.*completed" log)
              (format "last commit: %s" (string-trim log)))))

;;; Run All Tests

(defun test-run-all ()
  "Run all tests and print summary."
  (interactive)
  (setq test-results '())
  
  (message "\n========================================")
  (message "AMACS Harness Tests (IMP-001 + IMP-002)")
  (message "========================================\n")
  
  (condition-case err
      (progn
        (test-cold-start)
        (test-tick-cycle)
        (test-monologue-append)
        (test-monologue-window-size)
        (test-commit-includes-monologue)
        (test-warm-start)
        (test-long-gap)
        (test-summary))
    (error
     (message "TEST ERROR: %s" (error-message-string err))
     (test-summary))))

;; Run tests when loaded
(test-run-all)

;;; test-harness.el ends here
