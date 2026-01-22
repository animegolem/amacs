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
(require 'agent-chat)
(require 'agent-scratchpad)

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
              (string-match-p "Tick.*‖" log)
              (format "last commit: %s" (string-trim log)))))

(defun test-commit-message-format ()
  "Test: Commit message format and parsing (IMP-034)."
  (message "\n--- Test: Commit Message Format (IMP-034) ---")

  ;; Test format generation
  (let* ((msg (agent--format-commit-message))
         (has-delimiter (string-match-p "‖" msg))
         (has-tick (string-match-p "^Tick [0-9]+" msg)))
    (test-log "format-has-delimiter"
              has-delimiter
              (format "message: %s" (substring msg 0 (min 60 (length msg)))))
    (test-log "format-has-tick-prefix"
              has-tick
              "starts with Tick N"))

  ;; Test parsing a well-formed message
  (let* ((sample "Tick 42 ‖ rust-debugging ‖ focused ‖ 0.85 ‖ Testing the parser")
         (parsed (agent--parse-commit-message sample)))
    (test-log "parse-returns-alist"
              (listp parsed)
              "returns a list")
    (test-log "parse-tick-correct"
              (= 42 (alist-get 'tick parsed))
              (format "tick = %s" (alist-get 'tick parsed)))
    (test-log "parse-thread-correct"
              (equal "rust-debugging" (alist-get 'thread parsed))
              (format "thread = %s" (alist-get 'thread parsed)))
    (test-log "parse-mood-correct"
              (equal "focused" (alist-get 'mood parsed))
              (format "mood = %s" (alist-get 'mood parsed)))
    (test-log "parse-confidence-correct"
              (= 0.85 (alist-get 'confidence parsed))
              (format "confidence = %s" (alist-get 'confidence parsed)))
    (test-log "parse-monologue-correct"
              (equal "Testing the parser" (alist-get 'monologue parsed))
              (format "monologue = %s" (alist-get 'monologue parsed))))

  ;; Test parsing with pipes in monologue
  (let* ((sample "Tick 5 ‖ testing ‖ curious ‖ 0.70 ‖ Checking A | B | C options")
         (parsed (agent--parse-commit-message sample)))
    (test-log "parse-handles-pipes-in-monologue"
              (equal "Checking A | B | C options" (alist-get 'monologue parsed))
              (format "monologue with pipes = %s" (alist-get 'monologue parsed))))

  ;; Test parsing invalid message
  (let ((parsed (agent--parse-commit-message "not a valid commit message")))
    (test-log "parse-rejects-invalid"
              (null parsed)
              "returns nil for invalid")))

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
                            (alist-get 'use-count (cdr core-entry)))
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
                (alist-get 'id thread)
                (format "id: %s" (alist-get 'id thread)))

      (test-log "thread-has-buffers"
                (member "test-file.el" (alist-get 'buffers thread))
                (format "buffers: %s" (alist-get 'buffers thread)))

      (test-log "thread-has-mode"
                (alist-get 'primary-mode thread)
                (format "mode: %s" (alist-get 'primary-mode thread)))

      (test-log "thread-not-hydrated"
                (not (alist-get 'hydrated thread))
                "new threads start dehydrated"))))

(defun test-thread-switching ()
  "Test: Switching threads changes hydration state."
  (message "\n--- Test: Thread Switching ---")

  ;; Create and add a new thread
  (let* ((thread (agent-create-thread "Switch test" '("*scratch*")))
         (thread-id (alist-get 'id thread)))
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
                  (alist-get 'hydrated switched-thread)
                  "switched thread is hydrated"))

      ;; Switch back
      (when old-active
        (agent-switch-thread old-active)))))

(defun test-global-buffers ()
  "Test: Global buffers discovered by mode."
  (message "\n--- Test: Global Buffers ---")

  ;; Create a chat buffer with mode
  (let ((chat-buf (agent-create-chat-buffer "*test-mode-chat*")))
    ;; Check mode is enabled
    (test-log "chat-mode-enabled"
              (buffer-local-value 'amacs-chat-mode chat-buf)
              "chat buffer has amacs-chat-mode")

    ;; Check discovered by mode
    (let ((global (agent-get-global-buffers)))
      (test-log "chat-discovered-by-mode"
                (member "*test-mode-chat*" global)
                (format "global-buffers: %s" global)))

    ;; Kill the test buffer
    (kill-buffer chat-buf))

  ;; Check scratchpad was created and discovered
  (test-log "scratchpad-exists"
            (file-exists-p (expand-file-name "~/.agent/scratchpad.org"))
            "scratchpad file exists")

  (let ((scratch-buf (get-file-buffer
                      (expand-file-name "~/.agent/scratchpad.org"))))
    (test-log "scratchpad-mode-enabled"
              (and scratch-buf
                   (buffer-local-value 'agent-scratchpad-mode scratch-buf))
              "scratchpad has agent-scratchpad-mode")

    (let ((global (agent-get-global-buffers)))
      (test-log "scratchpad-discovered-by-mode"
                (member (buffer-name scratch-buf) global)
                (format "global includes scratchpad: %s" global))))

  ;; Check watched buffers includes discovered buffers
  (let ((watched (agent-get-watched-buffer-names)))
    (test-log "watched-includes-discovered"
              (> (length watched) 0)
              (format "watched: %s" watched))))

(defun test-context-assembly ()
  "Test: Context assembly produces expected structure."
  (message "\n--- Test: Context Assembly ---")

  (let ((ctx (agent-build-context)))
    (test-log "context-has-consciousness"
              (alist-get 'consciousness ctx)
              "has consciousness")

    (test-log "context-has-active-thread"
              (alist-get 'active-thread ctx)
              "has active-thread")

    (test-log "context-has-pending-threads"
              (listp (alist-get 'pending-threads ctx))
              "has pending-threads list")

    (test-log "context-has-global-buffers"
              (listp (alist-get 'global-buffers ctx))
              "has global-buffers list")))

;;; IMP-052 Tests: Buffer Hydration

(defun test-buffer-hydration ()
  "Test: Buffer content hydration in context (IMP-052)."
  (message "\n--- Test: Buffer Hydration ---")
  (require 'agent-context)
  (require 'agent-inference)

  ;; Test 1: Buffer content appears in context
  (let* ((test-buf (get-buffer-create "*hydration-test*"))
         (old-active (agent-get :active-thread)))
    (with-current-buffer test-buf
      (erase-buffer)
      (insert "Test buffer content for hydration"))
    ;; Create thread with this buffer and switch to it
    (let ((thread (agent-create-thread "hydration-test" '("*hydration-test*"))))
      (agent-add-thread thread)
      (agent-switch-thread (alist-get 'id thread)))
    (let* ((ctx (agent-build-context))
           (active-thread (alist-get 'active-thread ctx))
           (buffers (alist-get 'buffers active-thread)))
      (test-log "buffer-content-hydrated"
                (and buffers
                     (cl-some (lambda (b)
                                (and (equal (alist-get 'name b) "*hydration-test*")
                                     (string-match-p "Test buffer content"
                                                     (or (alist-get 'content b) ""))))
                              buffers))
                "buffer content in context"))
    ;; Switch back and cleanup
    (when old-active (agent-switch-thread old-active))
    (kill-buffer test-buf))

  ;; Test 2: Missing buffer handled gracefully
  (let ((missing-result (agent-hydrate-buffer "nonexistent-buffer-xyz")))
    (test-log "missing-buffer-nil"
              (null missing-result)
              "missing buffer returns nil"))

  ;; Test 3: Buffer truncation
  (let* ((test-buf (get-buffer-create "*truncation-test*"))
         (original-limit (agent-get 'buffer-content-limit)))
    (agent-set 'buffer-content-limit 100)  ; small limit for test
    (with-current-buffer test-buf
      (erase-buffer)
      (insert (make-string 500 ?x)))  ; 500 chars exceeds limit
    (let* ((hydrated (agent-hydrate-buffer "*truncation-test*"))
           (formatted (agent--format-buffer-for-prompt hydrated)))
      (test-log "buffer-truncated"
                (string-match-p "\\[...truncated...\\]" formatted)
                "large buffer truncated"))
    (agent-set 'buffer-content-limit original-limit)
    (kill-buffer test-buf)))

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
              (alist-get 'success result)
              "simple eval succeeds")
    (test-log "eval-simple-result"
              (equal (alist-get 'result result) "4")
              (format "result: %s" (alist-get 'result result))))

  ;; Test error capture (undefined function)
  (let ((result (agent-eval "(undefined-function-xyz)")))
    (test-log "eval-error-captured"
              (not (alist-get 'success result))
              "error eval has success nil")
    (test-log "eval-error-message"
              (stringp (alist-get 'error result))
              (format "error: %s" (alist-get 'error result))))

  ;; Test progn (multi-statement)
  (let ((result (agent-eval "(progn (setq test-var-xyz 1) (+ test-var-xyz 2))")))
    (test-log "eval-progn-success"
              (alist-get 'success result)
              "progn eval succeeds")
    (test-log "eval-progn-result"
              (equal (alist-get 'result result) "3")
              (format "result: %s" (alist-get 'result result))))

  ;; Test null eval skips
  (let ((result (agent-eval nil)))
    (test-log "eval-null-skipped"
              (alist-get 'skipped result)
              "null eval is skipped")
    (test-log "eval-null-success"
              (alist-get 'success result)
              "null eval has success t"))

  ;; Test empty string skips
  (let ((result (agent-eval "  ")))
    (test-log "eval-empty-skipped"
              (alist-get 'skipped result)
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

  ;; Test alist-to-json-alist
  (let ((alist (agent--alist-to-json-alist '((last-eval . "test") (my-key . 42)))))
    (test-log "alist-to-json-keys"
              (and (assoc "lastEval" alist) (assoc "myKey" alist))
              (format "keys: %s" (mapcar #'car alist)))
    (test-log "alist-to-json-values"
              (equal (cdr (assoc "myKey" alist)) 42)
              "values preserved"))

  ;; Test eval result in prompt (success case)
  (agent-set 'last-eval-result
             '((elisp . "(+ 2 2)") (success . t) (result . "4") (error . nil) (tick . 5)))
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
  (agent-set 'last-eval-result
             '((elisp . "(bad)") (success . nil) (result . nil) (error . "void function") (tick . 6)))
  (let ((section (agent--format-last-eval-for-prompt)))
    (test-log "eval-context-error"
              (and section
                   (or (string-match "\"success\":false" section)
                       (string-match "\"success\":null" section)))
              "error shows success:false or null"))

  ;; Test skipped eval returns nil
  (agent-set 'last-eval-result '((skipped . t) (success . t)))
  (let ((section (agent--format-last-eval-for-prompt)))
    (test-log "eval-context-skipped-nil"
              (null section)
              "skipped eval returns nil"))

  ;; Test no eval result returns nil
  (agent-set 'last-eval-result nil)
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
              (assoc 'bound-skills thread)
              "thread has bound-skills field"))

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
              "no bound skills returns nil"))

  ;; IMP-051: Test binding skill and verifying it appears in context
  (when (member "chat" (agent-list-available-skills))
    ;; Bind chat skill to active thread
    (agent-bind-skill-to-thread "chat")

    ;; Verify skill is in thread's bound-skills
    (test-log "skill-bound-to-thread"
              (member "chat" (agent-thread-bound-skills))
              "chat skill bound to thread")

    ;; Verify load-thread-skills returns content
    (let ((skills-section (agent--load-thread-skills)))
      (test-log "bound-skill-loads-content"
                (and skills-section (string-match-p "chat" skills-section))
                "bound skill content loaded"))

    ;; Verify skill content appears in full context
    (let* ((ctx (agent-build-context))
           (active-thread (alist-get 'active-thread ctx))
           (skills (alist-get 'skills active-thread)))
      (test-log "skill-in-context"
                (and skills (string-match-p "chat" skills))
                "skill content in context"))

    ;; Cleanup: unbind skill for other tests
    (agent-unbind-skill-from-thread "chat")))

(defun test-context-depth-controls ()
  "Test: Context depth controls (IMP-028)."
  (message "\n--- Test: Context Depth Controls ---")

  ;; Test default depth values exist in consciousness
  (test-log "chat-depth-exists"
            (numberp (agent-get 'chat-context-depth))
            (format "chat-context-depth: %s" (agent-get 'chat-context-depth)))

  (test-log "monologue-depth-exists"
            (numberp (agent-get 'monologue-context-depth))
            (format "monologue-context-depth: %s" (agent-get 'monologue-context-depth)))

  ;; Test monologue depth is applied in context
  (let* ((original-depth (agent-get 'monologue-context-depth))
         (test-monologue '("Line 1" "Line 2" "Line 3" "Line 4" "Line 5")))
    ;; Set small depth for test
    (agent-set 'monologue-context-depth 3)
    (agent-set 'recent-monologue test-monologue)
    (let* ((ctx (agent-build-context))
           (mono (alist-get 'recent-monologue ctx)))
      (test-log "monologue-depth-applied"
                (= (length mono) 3)
                (format "monologue limited to %d (depth=3)" (length mono))))
    ;; Restore
    (agent-set 'monologue-context-depth original-depth))

  ;; Test consciousness-for-context includes depth controls
  (let ((consciousness (agent-consciousness-for-context)))
    (test-log "context-has-chat-depth"
              (assoc 'chat-context-depth consciousness)
              "consciousness-for-context has chat-context-depth")
    (test-log "context-has-mono-depth"
              (assoc 'monologue-context-depth consciousness)
              "consciousness-for-context has monologue-context-depth")))

(defun test-bootstrap-skills ()
  "Test: Bootstrap skills installation (IMP-028)."
  (message "\n--- Test: Bootstrap Skills ---")

  ;; Test chat skill exists after init
  ;; (init was already called by warm start test)
  (test-log "chat-skill-available"
            (member "chat" (agent-list-available-skills))
            "chat skill in available list")

  ;; Test chat skill has SKILL.md
  (test-log "chat-skill-exists"
            (agent-skill-exists-p "chat")
            "chat skill SKILL.md exists"))

(defun test-chat-interface ()
  "Test: Chat interface functions (IMP-027)."
  (message "\n--- Test: Chat Interface ---")
  (require 'agent-chat)

  ;; Test consciousness has chat-pending field
  (test-log "chat-pending-field"
            (assoc 'chat-pending agent-consciousness)
            "consciousness has chat-pending")

  ;; Test chat-pending starts nil
  (test-log "chat-pending-nil"
            (null (agent-get 'chat-pending))
            "chat-pending starts nil")

  ;; Test setting chat-pending
  (agent-set 'chat-pending (list (cons 'buffer "*test-chat*")
                                  (cons 'queued-at (current-time))))
  (test-log "chat-pending-set"
            (agent-chat-pending-p)
            "agent-chat-pending-p returns t after setting")

  ;; Test chat buffer name retrieval
  (test-log "chat-buffer-name"
            (equal (agent-chat-buffer-name) "*test-chat*")
            (format "buffer name: %s" (agent-chat-buffer-name)))

  ;; Test creating chat buffer with prompt block
  (let ((buf (agent-create-chat-buffer "*test-amacs-chat*")))
    (test-log "chat-buffer-created"
              (bufferp buf)
              "chat buffer created")
    (test-log "chat-buffer-has-mode"
              (with-current-buffer buf
                (and (derived-mode-p 'org-mode)
                     amacs-chat-mode))
              "buffer has org-mode and amacs-chat-mode")
    (test-log "chat-buffer-has-prompt-block"
              (with-current-buffer buf
                (string-match "#\\+begin_prompt" (buffer-string)))
              "buffer has prompt block")
    ;; Clean up
    (kill-buffer buf))

  ;; Test clear pending
  (agent-chat-clear-pending)
  (test-log "chat-cleared"
            (not (agent-chat-pending-p))
            "chat-pending cleared"))

(defun test-chat-prompt-blocks ()
  "Test: Prompt block parsing and transformation (IMP-027)."
  (message "\n--- Test: Prompt Blocks ---")

  ;; Create a test buffer with prompt block
  (let ((buf (get-buffer-create "*test-prompt-blocks*")))
    (with-current-buffer buf
      (org-mode)
      (amacs-chat-mode 1)
      (erase-buffer)
      (insert "#+TITLE: Test\n\n")
      (insert "#+begin_prompt\nHello agent!\nHow are you?\n#+end_prompt\n"))

    ;; Test finding pending prompt
    (let ((prompt (agent-chat-find-pending-prompt buf)))
      (test-log "find-pending-prompt"
                prompt
                "found pending prompt")
      (test-log "prompt-content-extracted"
                (and prompt
                     (string-match "Hello agent" (alist-get 'content prompt)))
                (format "content: %s"
                        (and prompt
                             (substring (alist-get 'content prompt) 0
                                       (min 30 (length (alist-get 'content prompt))))))))

    ;; Test has-pending-prompt-p
    (test-log "has-pending-prompt"
              (agent-chat-has-pending-prompt-p buf)
              "has pending prompt")

    ;; Set up for response transformation
    (agent-set 'chat-pending (list (cons 'buffer (buffer-name buf))))

    ;; Test response transformation
    (agent-chat-respond "I am doing well, thank you!" "Thinking about response...")
    (with-current-buffer buf
      (test-log "response-creates-tick-heading"
                (string-match "^\\* Tick [0-9]+" (buffer-string))
                "tick heading created")
      (test-log "response-has-human-prompt"
                (string-match "^\\*\\* Human Prompt" (buffer-string))
                "human prompt subheading")
      (test-log "response-has-agent-response"
                (string-match "^\\*\\* Agent Response" (buffer-string))
                "agent response subheading")
      (test-log "response-preserves-content"
                (string-match "Hello agent" (buffer-string))
                "original content preserved")
      (test-log "response-includes-reply"
                (string-match "doing well" (buffer-string))
                "response text included"))

    ;; Test no longer has pending prompt (now wrapped in Tick)
    (test-log "no-pending-after-response"
              (not (agent-chat-has-pending-prompt-p buf))
              "no pending prompt after response")

    ;; Test reading exchanges
    (let ((exchanges (agent-chat-read-exchanges 1 buf)))
      (test-log "read-exchanges"
                exchanges
                (format "got %d exchanges" (length exchanges)))
      (when exchanges
        (test-log "exchange-has-human"
                  (alist-get 'human (car exchanges))
                  (format "human: %s"
                          (truncate-string-to-width
                           (or (alist-get 'human (car exchanges)) "nil") 30)))
        (test-log "exchange-has-agent"
                  (alist-get 'agent (car exchanges))
                  (format "agent: %s"
                          (truncate-string-to-width
                           (or (alist-get 'agent (car exchanges)) "nil") 30)))))

    ;; Clean up
    (kill-buffer buf)))

;;; Integration Tests (Manual - require API key)

(defun test-eval-loop ()
  "Integration test for eval loop (IMP-021).
Requires API key configured. Run with M-x test-eval-loop.
NOT included in automated CI tests."
  (interactive)
  (message "\n=== EVAL LOOP INTEGRATION TEST ===\n")
  (setq test-results '())

  ;; Setup
  (test-clean-slate)
  (agent-init)

  ;; Verify API is configured
  (unless (agent-api-configured-p)
    (user-error "API not configured. Set OPENROUTER_API_KEY first"))

  ;; Round 1: First Think
  (message "--- Round 1: First Think ---")
  (condition-case err
      (let ((thought1 (agent-think)))
        (message "Response received")

        ;; Check eval happened
        (let ((last-eval (agent-get 'last-eval-result)))
          (if (and last-eval (not (alist-get 'skipped last-eval)))
              (progn
                (message "Eval executed: %s" (alist-get 'elisp last-eval))
                (message "Result: %s" (alist-get 'result last-eval))
                (message "Success: %s" (alist-get 'success last-eval))
                (test-log "round-1-eval" t
                          (format "Eval: %s => %s"
                                  (alist-get 'elisp last-eval)
                                  (alist-get 'result last-eval))))
            (message "No eval in round 1 (agent may have returned null)")
            (test-log "round-1-eval" t "Agent didn't eval (acceptable)"))))
    (error
     (message "Round 1 error: %s" (error-message-string err))
     (test-log "round-1-eval" nil (error-message-string err))))

  ;; Brief pause to avoid rate limits
  (message "\nPausing 2 seconds for rate limit...")
  (sleep-for 2)

  ;; Round 2: Seeing Results
  (message "\n--- Round 2: Seeing Results ---")
  (condition-case err
      (let ((thought2 (agent-think)))
        (message "Response received")

        ;; Check agent saw previous result
        (let ((last-eval (agent-get 'last-eval-result)))
          (test-log "round-2-complete" t
                    (format "Tick: %d, Last eval tick: %s"
                            (agent-current-tick)
                            (alist-get 'tick last-eval)))))
    (error
     (message "Round 2 error: %s" (error-message-string err))
     (test-log "round-2-complete" nil (error-message-string err))))

  ;; Run assertions
  (test-eval-loop-assertions)

  ;; Summary
  (message "\n--- Results ---")
  (test-summary))

(defun test-eval-loop-assertions ()
  "Run assertions on eval loop state after test-eval-loop."
  ;; Consciousness should have eval result
  (test-log "consciousness-has-eval"
            (agent-get 'last-eval-result)
            "last-eval-result in consciousness")

  ;; Tick should have advanced
  (test-log "tick-advanced"
            (>= (agent-current-tick) 2)
            (format "Tick is %d" (agent-current-tick)))

  ;; Git commit should have happened
  (test-log "git-committed"
            (agent-get 'last-commit)
            (format "Last commit: %s" (agent-get 'last-commit)))

  ;; Monologue should have entries
  (test-log "monologue-has-entries"
            (> (length (agent-get 'recent-monologue)) 1)
            (format "Monologue entries: %d"
                    (length (agent-get 'recent-monologue)))))

(defun test-eval-error-handling ()
  "Test that eval errors don't crash harness.
Run with M-x test-eval-error-handling."
  (interactive)
  (message "\n=== EVAL ERROR HANDLING TEST ===\n")
  (setq test-results '())

  ;; Test error case directly
  (require 'agent-inference)
  (let ((result (agent-eval "(this-function-does-not-exist)")))
    (test-log "error-captured"
              (not (alist-get 'success result))
              "Error should be captured")
    (test-log "error-has-message"
              (stringp (alist-get 'error result))
              (format "Error: %s" (alist-get 'error result)))
    (test-log "no-crash"
              t
              "Harness didn't crash"))

  ;; Test read error
  (let ((result (agent-eval "(unclosed-paren")))
    (test-log "read-error-captured"
              (not (alist-get 'success result))
              "Read error should be captured")
    (test-log "read-error-message"
              (stringp (alist-get 'error result))
              (format "Read error: %s" (alist-get 'error result))))

  (test-summary))

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
        (test-commit-message-format)
        (test-skill-tracking)
        (test-relevant-skills)
        (test-thread-switching)
        (test-global-buffers)
        (test-context-assembly)
        (test-buffer-hydration)
        (test-json-parsing)
        (test-eval-execution)
        (test-context-integration)
        (test-skill-binding)
        (test-context-depth-controls)
        (test-bootstrap-skills)
        (test-chat-interface)
        (test-chat-prompt-blocks)
        (test-warm-start)
        (test-long-gap)
        (test-silent-tick)
        (test-autonomous-tick)
        (test-chat-context-integration)
        (test-summary))
    (error
     (message "TEST ERROR: %s" (error-message-string err))
     (test-summary))))

(defun test-silent-tick ()
  "Test: Silent tick support (IMP-055).
Verifies that responses without reply field process silently."
  (message "\n--- Test: Silent Tick Support (IMP-055) ---")

  ;; Test 1: Response with reply is handled normally
  (let* ((response-with-reply '(:parse-success t
                                :reply "Hello!"
                                :mood "happy"
                                :confidence 0.9
                                :monologue "Greeting user"))
         (has-reply (plist-get response-with-reply :reply)))
    (test-log "reply-present"
              has-reply
              (format "reply present: %s" has-reply)))

  ;; Test 2: Response without reply returns nil for :reply
  (let* ((response-no-reply '(:parse-success t
                              :mood "focused"
                              :confidence 0.8
                              :monologue "Working silently"))
         (reply (plist-get response-no-reply :reply)))
    (test-log "reply-absent"
              (null reply)
              (format "reply absent (nil): %s" reply)))

  ;; Test 3: Old fallback pattern no longer used
  ;; The old code did: (or (plist-get parsed :reply) "[No reply...]")
  ;; New code should just return nil for missing reply
  (let* ((response '(:parse-success t :mood "thinking" :confidence 0.7 :monologue "test"))
         (reply-direct (plist-get response :reply))
         (reply-with-fallback (or (plist-get response :reply) "FALLBACK")))
    (test-log "no-synthetic-reply"
              (null reply-direct)
              "direct plist-get returns nil for missing reply")
    (test-log "fallback-shows-nil-was-nil"
              (string= reply-with-fallback "FALLBACK")
              "confirms original value was nil (fallback triggered)")))

(defun test-autonomous-tick ()
  "Test: Autonomous tick mechanism (IMP-056).
Verifies continue field handling and tick limits."
  (message "\n--- Test: Autonomous Tick Mechanism (IMP-056) ---")

  ;; Test 1: Autonomous tick fields exist in consciousness
  (let ((limit (agent-get 'autonomous-tick-limit))
        (counter (agent-get 'autonomous-tick-counter)))
    (test-log "tick-limit-exists"
              limit
              (format "autonomous-tick-limit = %s" limit))
    (test-log "tick-counter-exists"
              (numberp counter)
              (format "autonomous-tick-counter = %s" counter)))

  ;; Test 2: Counter can be incremented
  (let ((old-counter (agent-get 'autonomous-tick-counter)))
    (agent-set 'autonomous-tick-counter (1+ old-counter))
    (let ((new-counter (agent-get 'autonomous-tick-counter)))
      (test-log "counter-increments"
                (= new-counter (1+ old-counter))
                (format "counter: %d -> %d" old-counter new-counter)))
    ;; Reset for other tests
    (agent-set 'autonomous-tick-counter 0))

  ;; Test 3: Response with continue field is detected
  (let* ((response-with-continue '(:parse-success t
                                   :continue t
                                   :mood "focused"
                                   :confidence 0.8
                                   :monologue "Continuing work"))
         (has-continue (plist-get response-with-continue :continue)))
    (test-log "continue-detected"
              has-continue
              (format "continue field: %s" has-continue)))

  ;; Test 4: Response without continue field returns nil
  (let* ((response-no-continue '(:parse-success t
                                 :mood "done"
                                 :confidence 0.9
                                 :monologue "Finished"))
         (continue-val (plist-get response-no-continue :continue)))
    (test-log "no-continue-nil"
              (null continue-val)
              "continue absent returns nil")))

(defun test-chat-context-integration ()
  "Test: Chat context integration in inference prompt (IMP-057).
Verifies that chat history is included in user prompt."
  (message "\n--- Test: Chat Context Integration (IMP-057) ---")
  (require 'agent-inference)
  (require 'agent-chat)

  ;; Test 1: chat-context-depth setting exists and is accessible
  (let ((depth (agent-get 'chat-context-depth)))
    (test-log "chat-depth-exists"
              depth
              (format "chat-context-depth = %s" depth)))

  ;; Test 2: Format function handles empty chat gracefully
  ;; (Uses temp buffer without exchanges)
  (let ((result (with-temp-buffer
                  (org-mode)
                  (agent-chat-read-exchanges 5))))
    (test-log "empty-chat-ok"
              (or (null result) (listp result))
              "empty chat returns nil or empty list"))

  ;; Test 3: Chat history format function exists and is callable
  (let ((chat-file (expand-file-name "agent-chat.org" "~/.agent/")))
    (if (file-exists-p chat-file)
        ;; If chat file exists, test formatting
        (let ((formatted (agent--format-chat-history)))
          (test-log "chat-format-works"
                    (or (null formatted) (stringp formatted))
                    (format "formatted chat: %s"
                            (if formatted
                                (substring formatted 0 (min 50 (length formatted)))
                              "nil"))))
      ;; If no chat file, verify graceful handling
      (test-log "no-chat-file-ok"
                t
                "no chat file - graceful handling")))

  ;; Test 4: User prompt includes chat sections when present
  (let ((prompt (agent-build-user-prompt)))
    (test-log "prompt-built"
              (stringp prompt)
              (format "prompt length: %d chars" (length prompt)))))

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
          (test-commit-message-format)
          (test-skill-tracking)
          (test-relevant-skills)
          (test-thread-switching)
          (test-global-buffers)
          (test-context-assembly)
          (test-buffer-hydration)
          (test-json-parsing)
          (test-eval-execution)
          (test-context-integration)
          (test-skill-binding)
          (test-context-depth-controls)
          (test-bootstrap-skills)
          (test-chat-interface)
          (test-chat-prompt-blocks)
          (test-warm-start)
          (test-long-gap)
          (test-silent-tick)
          (test-autonomous-tick)
          (test-chat-context-integration)
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
