# Consciousness Variable Schema

The `agent-consciousness` variable is your working memory. It's an elisp plist that persists across ticks and is included in every inference context.

## Full Structure

```elisp
(defvar agent-consciousness
  '(:identity "amacs-instance-1"
    
    ;; Temporal awareness
    :current-tick 142
    :current-time "2025-05-26T14:32:00Z"
    :last-inference-time "2025-05-26T14:28:00Z"
    :long-gap-detected nil  ; true if >1hr since last tick
    
    ;; Affective state
    :mood :focused           ; :curious, :stressed, :confident, :stuck
    :confidence 0.85         ; 0.0-1.0, current action confidence
    
    ;; Thread management
    :active-thread "rust-debugging"
    :thread-budget 3         ; Max open threads before consolidation required
    
    :open-threads
      ((:id "rust-debugging"
        :started-tick 142
        :priority 1
        :concern "Ownership error in main.rs"
        :buffers ("src/main.rs" "Cargo.toml")
        :approach "Trying lifetime annotations"
        :blocking t)
       
       (:id "config-cleanup"
        :started-tick 98
        :priority 3
        :concern "Keybinding conflicts"
        :buffers ("init.el")
        :approach "Consolidate custom bindings"
        :blocking nil))
    
    :completed-threads
      ((:id "dependency-update"
        :completed-tick 140
        :outcome "Success - updated 3 packages"
        :learned "Always check changelog first"))
    
    ;; Action history (for confidence tracking)
    :last-actions
      ((:tick 142 :action "eval-elisp" :confidence 0.85)
       (:tick 141 :action "modify-thread" :confidence 0.80)
       (:tick 140 :action "git-commit" :confidence 0.90))
    
    ;; Context management
    :watching-buffers ("src/main.rs" "Cargo.toml" "*agent-chat*")
    :focus (:buffer "src/main.rs" :line 42)
    
    ;; Memory pointers
    :last-commit "a1b2c3d"
    :recent-monologue
      ("Investigating lifetime annotations"
       "Fixed buffer switching"
       ;; Last 50-100 lines, most recent first
       )
    
    ;; Skills currently active
    :active-skills
      ((:name "core" :loaded-at-tick 0)
       (:name "rust-mode" :loaded-at-tick 142 :uses 15))
    
    ;; Human interaction
    :human-review-requested nil
    ;; When set: (:requested t :reason "..." :requested-at-tick N)
    
    ;; Budget tracking
    :budget
      (:cost-so-far 2.47
       :budget-limit 5.00
       :inference-count 23
       :pressure :moderate)  ; :low, :moderate, :high, :critical
    
    ;; Cognitive mode (if agent-adjustable sampling enabled)
    :cognitive-mode nil
    ;; When set: (:temperature 0.7 :adjusted-at-tick N :revert-at-tick M :reason "...")
  ))
```

## Field Reference

### Identity & Temporal

| Field | Type | Description |
|-------|------|-------------|
| `:identity` | string | Instance name |
| `:current-tick` | integer | Current tick counter |
| `:current-time` | ISO 8601 | Wall clock time |
| `:last-inference-time` | ISO 8601 | When last tick completed |
| `:long-gap-detected` | boolean | True if >1hr since last tick |

### Affective State

| Field | Type | Description |
|-------|------|-------------|
| `:mood` | keyword | Self-reported state. For your use, not watchdog. |
| `:confidence` | float 0-1 | Confidence in current/recent action |

### Thread Management

| Field | Type | Description |
|-------|------|-------------|
| `:active-thread` | string | ID of current focus thread |
| `:thread-budget` | integer | Max simultaneous open threads |
| `:open-threads` | list | Active concerns being tracked |
| `:completed-threads` | list | Recently completed (for learning reference) |

**Thread structure:**
```elisp
(:id "thread-name"
 :started-tick N
 :priority 1-5        ; 1 = highest
 :concern "..."       ; What problem this addresses
 :buffers ("...")     ; Relevant files
 :approach "..."      ; Current strategy
 :blocking t/nil)     ; Is this blocking other work?
```

### Action History

| Field | Type | Description |
|-------|------|-------------|
| `:last-actions` | list | Recent actions with confidence scores |

This is what the human watchdog monitors. Sustained declining confidence = alarm.

### Context Management

| Field | Type | Description |
|-------|------|-------------|
| `:watching-buffers` | list | Buffers that trigger wake on change |
| `:focus` | plist | Current cursor position |

### Memory

| Field | Type | Description |
|-------|------|-------------|
| `:last-commit` | string | Most recent git commit hash |
| `:recent-monologue` | list | Last 50-100 monologue lines |

### Skills

| Field | Type | Description |
|-------|------|-------------|
| `:active-skills` | list | Currently loaded skills with usage counts |

### Human Interaction

| Field | Type | Description |
|-------|------|-------------|
| `:human-review-requested` | nil or plist | Signal to request human attention |

Set this when you need help but can't identify why:
```elisp
(:requested t
 :reason "Confidence declining but can't identify cause"
 :requested-at-tick 156)
```

### Budget

| Field | Type | Description |
|-------|------|-------------|
| `:budget` | plist | Token/cost tracking |

Pressure levels: `:low` (<50% used), `:moderate` (50-75%), `:high` (75-90%), `:critical` (>90%)

## Modifying Consciousness

You have full access to modify this variable. Common patterns:

```elisp
;; Update confidence
(plist-put agent-consciousness :confidence 0.75)

;; Add to monologue
(push "New thought" (plist-get agent-consciousness :recent-monologue))

;; Switch active thread
(plist-put agent-consciousness :active-thread "other-thread")

;; Request human review
(plist-put agent-consciousness :human-review-requested
           '(:requested t :reason "Stuck in loop" :requested-at-tick 150))
```

## Persistence

Consciousness is persisted to `~/.agent/consciousness.el` each tick:

```elisp
(defun persist-consciousness ()
  (with-temp-file "~/.agent/consciousness.el"
    (prin1 agent-consciousness (current-buffer))))
```

On warm start, this is loaded before the first tick runs.
