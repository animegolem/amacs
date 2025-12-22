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
(require 'agent-skills)
(require 'agent-threads)
(require 'agent-context)

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
  "Print test summary. Returns t if all passed, nil otherwise."
  (let ((passed (length (seq-filter #'cadr test-results)))
        (total (length test-results)))
    (message "\n==================")
    (message "Tests: %d/%d passed" passed total)
    (message "==================\n")
    (when (< passed total)
      (message "Failed tests:")
      (dolist (test test-results)
        (unless (cadr test)
          (message "  - %s: %s" (car test) (or (caddr test) "no details")))))
    (= passed total)))

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

(defun test-skill-system-init ()
  "Test: Skill system initializes with core skill."
  (message "\n--- Test: Skill System Init ---")
  
  ;; Check skills directory exists
  (test-log "skills-dir-exists"
            (file-directory-p "~/.agent/skills/")
            "skills directory")
  
  ;; Check core skill was installed
  (test-log "core-skill-exists"
            (file-exists-p "~/.agent/skills/core/SKILL.md")
            "core/SKILL.md")
  
  ;; Check core skill references exist
  (test-log "core-skill-references"
            (file-exists-p "~/.agent/skills/core/references/consciousness-schema.md")
            "core/references/consciousness-schema.md"))

(defun test-skill-loading ()
  "Test: Core skill can be loaded."
  (message "\n--- Test: Skill Loading ---")
  
  ;; Load core skill
  (let ((content (agent-load-skill "core")))
    (test-log "skill-loads"
              (and content (> (length content) 0))
              (format "loaded %d chars" (length (or content ""))))
    
    ;; Check content looks right
    (test-log "skill-content-valid"
              (and content (string-match-p "AMACS" content))
              "content contains AMACS")))

(defun test-skill-tracking ()
  "Test: Skills are tracked in :active-skills."
  (message "\n--- Test: Skill Tracking ---")
  
  (let ((active (agent-get :active-skills)))
    (test-log "active-skills-present"
              (> (length active) 0)
              (format "%d skills tracked" (length active)))
    
    ;; Check core skill is tracked
    (let ((core-entry (assoc "core" active)))
      (test-log "core-skill-tracked"
                core-entry
                (if core-entry 
                    (format "use-count: %d" 
                            (plist-get (cdr core-entry) :use-count))
                  "not found")))))

(defun test-relevant-skills ()
  "Test: Relevant skills include core."
  (message "\n--- Test: Relevant Skills ---")
  
  (let ((relevant (agent-get-relevant-skills)))
    (test-log "relevant-includes-core"
              (member "core" relevant)
              (format "relevant: %s" relevant))))

;;; IMP-004 Tests: Thread-Centric Context

(defun test-default-thread-created ()
  "Test: Cold start creates a default thread."
  (message "\n--- Test: Default Thread Created ---")
  
  ;; Check active thread exists
  (test-log "has-active-thread"
            (agent-get :active-thread)
            (format "active: %s" (agent-get :active-thread)))
  
  ;; Check open threads has at least one
  (test-log "has-open-threads"
            (> (length (agent-get :open-threads)) 0)
            (format "%d open threads" (length (agent-get :open-threads)))))

(defun test-thread-creation ()
  "Test: Creating a thread captures buffer and mode."
  (message "\n--- Test: Thread Creation ---")
  
  ;; Create a test buffer
  (with-temp-buffer
    (setq-local major-mode 'emacs-lisp-mode)
    (rename-buffer "test-file.el" t)
    
    ;; Create thread from this buffer
    (let ((thread (agent-create-thread "Test concern" '("test-file.el"))))
      (test-log "thread-has-id"
                (plist-get thread :id)
                (format "id: %s" (plist-get thread :id)))
      
      (test-log "thread-has-buffers"
                (member "test-file.el" (plist-get thread :buffers))
                (format "buffers: %s" (plist-get thread :buffers)))
      
      (test-log "thread-has-mode"
                (plist-get thread :primary-mode)
                (format "mode: %s" (plist-get thread :primary-mode)))
      
      (test-log "thread-not-hydrated"
                (not (plist-get thread :hydrated))
                "new threads start dehydrated"))))

(defun test-thread-switching ()
  "Test: Switching threads changes hydration state."
  (message "\n--- Test: Thread Switching ---")
  
  ;; Create and add a new thread
  (let* ((thread (agent-create-thread "Switch test" '("*scratch*")))
         (thread-id (plist-get thread :id)))
    (agent-add-thread thread)
    
    ;; Remember old active
    (let ((old-active (agent-get :active-thread)))
      ;; Switch to new thread
      (agent-switch-thread thread-id)
      
      (test-log "active-thread-changed"
                (equal (agent-get :active-thread) thread-id)
                (format "active is now: %s" (agent-get :active-thread)))
      
      ;; Check new thread is hydrated
      (let ((switched-thread (agent-get-thread thread-id)))
        (test-log "new-thread-hydrated"
                  (plist-get switched-thread :hydrated)
                  "switched thread is hydrated"))
      
      ;; Switch back
      (when old-active
        (agent-switch-thread old-active)))))

(defun test-global-buffers ()
  "Test: Global buffers are in watched list."
  (message "\n--- Test: Global Buffers ---")
  
  ;; Check *agent-chat* is in global buffers
  (let ((global (agent-get :global-buffers)))
    (test-log "agent-chat-global"
              (member "*agent-chat*" global)
              (format "global-buffers: %s" global)))
  
  ;; Check watched buffers includes global
  (let ((watched (agent-get-watched-buffer-names)))
    (test-log "watched-includes-global"
              (member "*agent-chat*" watched)
              (format "watched: %s" watched))))

(defun test-context-assembly ()
  "Test: Context assembly produces expected structure."
  (message "\n--- Test: Context Assembly ---")
  
  (let ((ctx (agent-build-context)))
    (test-log "context-has-consciousness"
              (plist-get ctx :consciousness)
              "has :consciousness")
    
    (test-log "context-has-active-thread"
              (plist-get ctx :active-thread)
              "has :active-thread")
    
    (test-log "context-has-pending-threads"
              (listp (plist-get ctx :pending-threads))
              "has :pending-threads list")
    
    (test-log "context-has-global-buffers"
              (listp (plist-get ctx :global-buffers))
              "has :global-buffers list")))

;;; IMP-017 Tests: JSON Response Protocol

(defun test-json-parsing ()
  "Test: JSON response parsing works correctly."
  (message "\n--- Test: JSON Parsing ---")
  (require 'agent-inference)

  ;; Test valid JSON
  (let ((parsed (agent--parse-response
                 "{\"eval\": \"(+ 1 1)\", \"thought\": \"testing\", \"mood\": \"focused\", \"confidence\": 0.9, \"monologue\": \"test line\"}")))
    (test-log "json-parse-eval"
              (equal (plist-get parsed :eval) "(+ 1 1)")
              (format "eval: %s" (plist-get parsed :eval)))
    (test-log "json-parse-mood"
              (equal (plist-get parsed :mood) "focused")
              (format "mood: %s" (plist-get parsed :mood)))
    (test-log "json-parse-success"
              (plist-get parsed :parse-success)
              "parse-success is t"))

  ;; Test JSON in markdown fence
  (let ((parsed (agent--parse-response
                 "Here's my response:\n```json\n{\"eval\": null, \"thought\": \"thinking\", \"mood\": \"curious\", \"confidence\": 0.7, \"monologue\": \"hmm\"}\n```")))
    (test-log "json-markdown-fence"
              (equal (plist-get parsed :mood) "curious")
              (format "extracted from fence: %s" (plist-get parsed :mood))))

  ;; Test emoji mood preserved
  (let ((parsed (agent--parse-response
                 "{\"eval\": null, \"thought\": \"emoji test\", \"mood\": \"🤔\", \"confidence\": 0.8, \"monologue\": \"test\"}")))
    (test-log "json-emoji-mood"
              (equal (plist-get parsed :mood) "🤔")
              (format "emoji: %s" (plist-get parsed :mood))))

  ;; Test malformed JSON fallback
  (let ((parsed (agent--parse-response "This is not valid JSON {broken")))
    (test-log "json-fallback-mood"
              (equal (plist-get parsed :mood) "uncertain")
              "fallback mood is 'uncertain'")
    (test-log "json-fallback-thought"
              (stringp (plist-get parsed :thought))
              "fallback thought is string")
    (test-log "json-fallback-success"
              (not (plist-get parsed :parse-success))
              "parse-success is nil")))

(defun test-eval-execution ()
  "Test: Eval execution works correctly (IMP-018)."
  (message "\n--- Test: Eval Execution ---")
  (require 'agent-inference)

  ;; Test simple expression
  (let ((result (agent-eval "(+ 2 2)")))
    (test-log "eval-simple-success"
              (plist-get result :success)
              "simple eval succeeds")
    (test-log "eval-simple-result"
              (equal (plist-get result :result) "4")
              (format "result: %s" (plist-get result :result))))

  ;; Test error capture (undefined function)
  (let ((result (agent-eval "(undefined-function-xyz)")))
    (test-log "eval-error-captured"
              (not (plist-get result :success))
              "error eval has :success nil")
    (test-log "eval-error-message"
              (stringp (plist-get result :error))
              (format "error: %s" (plist-get result :error))))

  ;; Test progn (multi-statement)
  (let ((result (agent-eval "(progn (setq test-var-xyz 1) (+ test-var-xyz 2))")))
    (test-log "eval-progn-success"
              (plist-get result :success)
              "progn eval succeeds")
    (test-log "eval-progn-result"
              (equal (plist-get result :result) "3")
              (format "result: %s" (plist-get result :result))))

  ;; Test null eval skips
  (let ((result (agent-eval nil)))
    (test-log "eval-null-skipped"
              (plist-get result :skipped)
              "null eval is skipped")
    (test-log "eval-null-success"
              (plist-get result :success)
              "null eval has :success t"))

  ;; Test empty string skips
  (let ((result (agent-eval "  ")))
    (test-log "eval-empty-skipped"
              (plist-get result :skipped)
              "empty string eval is skipped"))

  ;; Test monologue formatting
  (let* ((result (agent-eval "(+ 10 20)"))
         (log-line (agent--format-eval-for-monologue "(+ 10 20)" result)))
    (test-log "eval-monologue-format"
              (and log-line (string-match "EVAL:" log-line))
              (format "log: %s" log-line)))

  ;; Test monologue nil for skipped
  (let* ((result (agent-eval nil))
         (log-line (agent--format-eval-for-monologue nil result)))
    (test-log "eval-monologue-skipped-nil"
              (null log-line)
              "skipped eval returns nil for monologue")))

(defun test-context-integration ()
  "Test: Context integration for eval results (IMP-019)."
  (message "\n--- Test: Context Integration ---")
  (require 'agent-inference)

  ;; Test kebab-to-camel
  (test-log "kebab-to-camel-simple"
            (equal (agent--kebab-to-camel "last-eval-result") "lastEvalResult")
            (format "result: %s" (agent--kebab-to-camel "last-eval-result")))
  (test-log "kebab-to-camel-single"
            (equal (agent--kebab-to-camel "eval") "eval")
            "single word unchanged")

  ;; Test plist-to-json-alist
  (let ((alist (agent--plist-to-json-alist '(:last-eval "test" :my-key 42))))
    (test-log "plist-to-alist-keys"
              (and (assoc "lastEval" alist) (assoc "myKey" alist))
              (format "keys: %s" (mapcar #'car alist)))
    (test-log "plist-to-alist-values"
              (equal (cdr (assoc "myKey" alist)) 42)
              "values preserved"))

  ;; Test eval result in prompt (success case)
  (agent-set :last-eval-result
             '(:elisp "(+ 2 2)" :success t :result "4" :error nil :tick 5))
  (let ((section (agent--format-last-eval-for-prompt)))
    (test-log "eval-context-exists"
              (and section (string-match "Last Eval Result" section))
              "section header present")
    (test-log "eval-context-camel-keys"
              (and section (string-match "\"success\"" section))
              "camelCase keys in JSON")
    (test-log "eval-context-tick"
              (and section (string-match "tick 5" section))
              "tick number in header"))

  ;; Test eval error in prompt
  (agent-set :last-eval-result
             '(:elisp "(bad)" :success nil :result nil :error "void function" :tick 6))
  (let ((section (agent--format-last-eval-for-prompt)))
    (test-log "eval-context-error"
              (and section
                   (or (string-match "\"success\":false" section)
                       (string-match "\"success\":null" section)))
              "error shows success:false or null"))

  ;; Test skipped eval returns nil
  (agent-set :last-eval-result '(:skipped t :success t))
  (let ((section (agent--format-last-eval-for-prompt)))
    (test-log "eval-context-skipped-nil"
              (null section)
              "skipped eval returns nil"))

  ;; Test no eval result returns nil
  (agent-set :last-eval-result nil)
  (let ((section (agent--format-last-eval-for-prompt)))
    (test-log "eval-context-none-nil"
              (null section)
              "no eval returns nil")))

(defun test-skill-binding ()
  "Test: Skill binding to threads (IMP-023)."
  (message "\n--- Test: Skill Binding ---")
  (require 'agent-skills)

  ;; Test core excluded from available skills
  (test-log "skill-list-excludes-core"
            (not (member "core" (agent-list-available-skills)))
            "core not in available list")

  ;; Test thread has bound-skills field
  (let ((thread (agent-get-active-thread)))
    (test-log "thread-has-bound-skills"
              (plist-member thread :bound-skills)
              "thread has :bound-skills field"))

  ;; Test binding nonexistent skill errors
  (let ((error-caught nil))
    (condition-case _err
        (agent-bind-skill-to-thread "nonexistent-skill-xyz")
      (error (setq error-caught t)))
    (test-log "bind-nonexistent-errors"
              error-caught
              "binding nonexistent skill errors"))

  ;; Test load-thread-skills returns nil with no skills bound
  (let ((section (agent--load-thread-skills)))
    (test-log "no-skills-no-section"
              (null section)
              "no bound skills returns nil")))

(defun test-chat-interface ()
  "Test: Chat interface functions (IMP-022)."
  (message "\n--- Test: Chat Interface ---")
  (require 'agent-chat)

  ;; Test consciousness has chat-pending field
  (test-log "chat-pending-field"
            (plist-member agent-consciousness :chat-pending)
            "consciousness has :chat-pending")

  ;; Test chat-pending starts nil
  (test-log "chat-pending-nil"
            (null (agent-get :chat-pending))
            "chat-pending starts nil")

  ;; Test setting chat-pending
  (agent-set :chat-pending (list :buffer "*test-chat*" :queued-at (current-time)))
  (test-log "chat-pending-set"
            (agent-chat-pending-p)
            "agent-chat-pending-p returns t after setting")

  ;; Test chat buffer name retrieval
  (test-log "chat-buffer-name"
            (equal (agent-chat-buffer-name) "*test-chat*")
            (format "buffer name: %s" (agent-chat-buffer-name)))

  ;; Test creating chat buffer
  (let ((buf (agent-create-chat-buffer "*test-amacs-chat*")))
    (test-log "chat-buffer-created"
              (bufferp buf)
              "chat buffer created")
    (test-log "chat-buffer-has-mode"
              (with-current-buffer buf
                (and (derived-mode-p 'org-mode)
                     amacs-chat-mode))
              "buffer has org-mode and amacs-chat-mode")
    (test-log "chat-buffer-has-header"
              (with-current-buffer buf
                (string-match "AMACS Chat" (buffer-string)))
              "buffer has title")
    ;; Clean up
    (kill-buffer buf))

  ;; Test clear pending
  (agent-chat-clear-pending)
  (test-log "chat-cleared"
            (not (agent-chat-pending-p))
            "chat-pending cleared"))

;;; Run All Tests

(defun test-run-all ()
  "Run all tests and print summary."
  (interactive)
  (setq test-results '())

  (message "\n================================================")
  (message "AMACS Harness Tests")
  (message "================================================\n")

  (condition-case err
      (progn
        (test-cold-start)
        (test-skill-system-init)
        (test-skill-loading)
        (test-default-thread-created)
        (test-thread-creation)
        (test-tick-cycle)
        (test-monologue-append)
        (test-monologue-window-size)
        (test-commit-includes-monologue)
        (test-skill-tracking)
        (test-relevant-skills)
        (test-thread-switching)
        (test-global-buffers)
        (test-context-assembly)
        (test-json-parsing)
        (test-eval-execution)
        (test-context-integration)
        (test-skill-binding)
        (test-chat-interface)
        (test-warm-start)
        (test-long-gap)
        (test-summary))
    (error
     (message "TEST ERROR: %s" (error-message-string err))
     (test-summary))))

(defun test-run-all-batch ()
  "Run all tests in batch mode, exit with appropriate code.
Exit 0 if all tests pass, exit 1 if any fail."
  (let ((all-passed nil))
    (condition-case err
        (progn
          (setq test-results '())
          (message "\n================================================")
          (message "AMACS Harness Tests (Batch Mode)")
          (message "================================================\n")
          (test-cold-start)
          (test-skill-system-init)
          (test-skill-loading)
          (test-default-thread-created)
          (test-thread-creation)
          (test-tick-cycle)
          (test-monologue-append)
          (test-monologue-window-size)
          (test-commit-includes-monologue)
          (test-skill-tracking)
          (test-relevant-skills)
          (test-thread-switching)
          (test-global-buffers)
          (test-context-assembly)
          (test-json-parsing)
          (test-eval-execution)
          (test-context-integration)
          (test-skill-binding)
          (test-chat-interface)
          (test-warm-start)
          (test-long-gap)
          (setq all-passed (test-summary)))
      (error
       (message "TEST ERROR: %s" (error-message-string err))
       (test-summary)
       (setq all-passed nil)))
    (kill-emacs (if all-passed 0 1))))

;; Run tests when loaded interactively (not in batch mode)
(unless noninteractive
  (test-run-all))

;;; test-harness.el ends here
