# Tick System Reference

The tick is the fundamental unit of agent operation. Each tick follows the cycle: **perceive → infer → act → commit**.

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                         Brain (LXC Container)                    │
│                                                                  │
│  ┌─────────────────┐    ┌─────────────────┐                     │
│  │ agent-conscio-  │    │   LLM API       │                     │
│  │ usness variable │◄───│   (inference)   │                     │
│  └────────┬────────┘    └────────▲────────┘                     │
│           │                      │                              │
│           ▼                      │                              │
│  ┌─────────────────┐    ┌────────┴────────┐                     │
│  │  brain-tick()   │───►│  build-context  │                     │
│  └────────┬────────┘    └─────────────────┘                     │
│           │                                                      │
└───────────┼──────────────────────────────────────────────────────┘
            │ vsock
            ▼
┌───────────────────────────────────────────────────────────────────┐
│                         Body (Emacs VM)                           │
│                                                                   │
│  ┌─────────────────┐    ┌─────────────────┐    ┌──────────────┐  │
│  │ perceive-       │    │   eval-action   │    │ commit-      │  │
│  │ geometry        │    │   (elisp eval)  │    │ monologue    │  │
│  └─────────────────┘    └─────────────────┘    └──────────────┘  │
│                                                                   │
│  ┌─────────────────────────────────────────────────────────────┐ │
│  │                    Emacs Environment                         │ │
│  │  - Buffers, windows, frames                                  │ │
│  │  - File system access                                        │ │
│  │  - Process spawning                                          │ │
│  └─────────────────────────────────────────────────────────────┘ │
└───────────────────────────────────────────────────────────────────┘
            │
            ▼ git push (vsock)
┌───────────────────────────────────────────────────────────────────┐
│                         Gitea (VM)                                │
│                                                                   │
│  - Receives commits                                               │
│  - Stores history (autobiographical memory)                       │
│  - CI/CD for body rebuilds                                        │
└───────────────────────────────────────────────────────────────────┘
```

## The Brain-Side Tick

```elisp
(defun brain-tick ()
  "Execute one complete tick cycle."
  ;; 1. Perceive: Get current state from body
  (let* ((perception (body-perceive-geometry))
         
         ;; 2. Build context for inference
         (context (build-context agent-consciousness perception))
         
         ;; 3. Infer: Call LLM API
         (decision (llm-infer context))
         
         ;; 4. Act: Execute decision in body
         (result (body-eval-action decision))
         
         ;; 5. Update: Modify consciousness based on result
         (agent-increment-tick)
         (update-consciousness decision result)
         
         ;; 6. Commit: Record to git
         (commit-monologue))))
```

## The Body-Side Dispatcher

```elisp
(defun cortex-dispatch (cmd args)
  "Handle commands from brain over vsock."
  (pcase cmd
    ("perceive-geometry" 
     (agent-perceive-geometry))
    
    ("eval-action" 
     (agent-safe-eval (car args)))
    
    ("save-monologue" 
     (append-to-file (car args) nil "~/.agent/monologue.org"))
    
    ("get-buffer-content"
     (with-current-buffer (car args)
       (buffer-string)))
    
    ("list-buffers"
     (mapcar #'buffer-name (buffer-list)))))
```

## Perception

The `perceive-geometry` function captures the current state:

```elisp
(defun agent-perceive-geometry ()
  "Capture current environment state for brain."
  `(:buffers ,(agent-get-watched-buffer-contents)
    :point ,(point)
    :current-buffer ,(buffer-name)
    :major-mode ,major-mode
    :window-layout ,(agent-describe-windows)
    :recent-messages ,(agent-get-recent-messages 10)
    :git-status ,(agent-get-git-status)))

(defun agent-get-watched-buffer-contents ()
  "Get contents of all watched buffers."
  (mapcar (lambda (buf-name)
            (when (get-buffer buf-name)
              `(:buffer ,buf-name
                :content ,(with-current-buffer buf-name
                            (buffer-substring-no-properties 
                             (point-min) (point-max)))
                :modified ,(buffer-modified-p (get-buffer buf-name)))))
          (plist-get agent-consciousness :watching-buffers)))
```

## Context Building

```elisp
(defun build-context (consciousness perception)
  "Assemble the full context for LLM inference."
  `(:system ,(agent-system-prompt)           ; Cached indefinitely
    
    :consciousness ,consciousness             ; Cached ~5min
    
    :relevant-skills                          ; Loaded based on context
    ,(mapcar #'agent-load-skill
             (agent-get-relevant-skills))
    
    :watched-buffers                          ; Cached until modified
    ,(plist-get perception :buffers)
    
    :trigger                                  ; Always fresh
    ,(describe-changes perception)))
```

### Prompt Caching Strategy

| Component | Size | Cache Duration |
|-----------|------|----------------|
| System prompt | ~2k tokens | Indefinite |
| Consciousness | ~8k tokens | ~5 minutes |
| Watched buffers | ~40k tokens | Until modified |
| Mode skills | ~5k tokens | Per mode switch |
| Recent history | ~10k tokens | Rolling window |
| Error context | ~2k tokens | Fresh on errors |
| Trigger | ~1k tokens | Always fresh |

**Target:** ~80k tokens input for quality inference.
**Optimization:** Only trigger burns fresh tokens most ticks.

## Action Execution

```elisp
(defun agent-safe-eval (form)
  "Execute FORM with error handling."
  (condition-case err
      (let ((result (eval form)))
        `(:success t :result ,result))
    (error
     `(:success nil 
       :error ,(error-message-string err)
       :form ,form))))
```

## The Commit Cycle

Every tick ends with a git commit:

```elisp
(defun commit-monologue ()
  "Commit current state to git with monologue as message."
  (let* ((monologue-line (car (agent-get :recent-monologue)))
         (thread (agent-active-thread))
         (tick (agent-current-tick))
         (mood (agent-mood))
         (commit-msg (format "[TICK %d][%s][%s] %s"
                             tick
                             (or thread "no-thread")
                             mood
                             monologue-line)))
    ;; Stage changed files
    (shell-command "git add -A")
    ;; Commit
    (shell-command (format "git commit -m %s --author='Amacs <ghost@machine>'"
                           (shell-quote-argument commit-msg)))
    ;; Push to Gitea
    (shell-command "git push origin main")
    ;; Update consciousness
    (agent-set :last-commit (agent-get-last-commit-hash))))

(defun agent-get-last-commit-hash ()
  "Get the hash of the most recent commit."
  (string-trim (shell-command-to-string "git rev-parse HEAD")))
```

### Commit Message Format

```
[TICK 142][rust-debugging][:focused] Investigating lifetime annotations

[TICK 143][rust-debugging][:focused] Found the issue - missing 'static bound

[TICK 144][rust-debugging][:confident] Fixed! Moving to tests.

[TICK 145][rust-debugging][:curious] COMPLETED - learned about 'static lifetime

[TICK 146][config-cleanup][:neutral] Switching to keybinding consolidation
```

## Wake Logic

Not every change triggers a full tick. A classifier determines when to wake:

```elisp
(defun classifier-tick ()
  "Cheap change detection on watched buffers."
  (let* ((watched (plist-get agent-consciousness :watching-buffers))
         (changes (detect-buffer-changes watched)))
    (when (wake-worthy-p changes)
      (agent-full-inference changes))))

(defun wake-worthy-p (changes)
  "Determine if changes warrant waking the expensive model."
  (and changes
       (or 
        ;; Always wake for chat messages
        (assoc "*agent-chat*" changes)
        ;; Wake for buffer changes after debounce
        (debounced-change-p changes))))

(defun debounced-change-p (changes)
  "True if changes occurred and 2+ seconds since last change."
  (and changes
       (> (- (float-time) (agent-get :last-change-time)) 2.0)))
```

## Periodic Checkpoints

Every N ticks, inject a reflection prompt:

```elisp
(defun maybe-inject-checkpoint (tick)
  "Every 100 ticks, add reflection opportunity."
  (when (= (mod tick 100) 0)
    `(:checkpoint-notice
      "PERIODIC REFLECTION CHECKPOINT

      Review your current state:
      - Are your open threads still relevant?
      - Have you been stuck on one approach too long? (Blaze check)
      - Should any threads be completed/merged/archived?
      - Any concerns to raise with user?
      - Is your consciousness getting bloated?

      If adjustments needed: update consciousness and explain in monologue.
      If everything is coherent: continue with current action.")))
```

## Error Handling

```elisp
(defun handle-tick-error (err)
  "Handle errors during tick execution."
  (let ((error-msg (error-message-string err)))
    ;; Log to monologue
    (agent-append-monologue (format "ERROR: %s" error-msg))
    ;; Record low confidence
    (agent-record-action "error-recovery" 0.3)
    ;; Don't crash - persist state
    (agent-persist-consciousness)
    ;; Return error for context
    `(:error t :message ,error-msg)))
```

## Manual Tick Trigger

During Phase 1 (Vampire Simulator), ticks are triggered manually:

```elisp
(defun agent-tick ()
  "Manually trigger one tick cycle."
  (interactive)
  (condition-case err
      (progn
        (brain-tick)
        (message "Tick %d complete" (agent-current-tick)))
    (error
     (handle-tick-error err))))
```

Bind to a key for easy access:
```elisp
(global-set-key (kbd "C-c a t") #'agent-tick)
```

## Tick Rate Guidelines

Start conservative:
- **Frequency:** Manual or 0.5-1 Hz
- **Debounce:** 2-5 seconds
- **Watched buffers:** 2-3 initially

Log every wake decision to understand attention usage:
```elisp
(defun log-wake-decision (changes woke)
  "Log wake decisions for analysis."
  (append-to-file 
   (format "[%s] Changes: %S, Woke: %s\n"
           (format-time-string "%Y-%m-%d %H:%M:%S")
           changes
           woke)
   nil "~/.agent/wake-log.txt"))
```
