---
node_id: AI-IMP-023
tags:
  - IMP
  - phase-2
  - skills
  - threads
  - context
status: done
depends_on:
  - AI-IMP-019
implements: AI-EPIC-002
created_date: 2025-12-20
updated_date: 2025-12-21
close_date: 2025-12-21
related_files:
  - harness/agent-skills.el
  - harness/agent-threads.el
  - harness/agent-context.el
  - skills/amacs-bootstrap-skill/core/SKILL.md
confidence_score: 0.85
---

# AI-IMP-023: Skill Binding System

## Objective

Implement skill binding to threads so the agent can load domain-specific knowledge when working on particular concerns. Skills travel with threads - when a thread is active, its bound skills load into context.

## Design Decisions

### Skill Location

Skills live at `~/.agent/skills/` at runtime:
- Copied from project `skills/` directory at deploy time
- Agent can create new skills there over time
- Dotfiles can be git-backed separately

For Phase 2 local testing, symlink or copy skills to `~/.agent/skills/` during setup.

### skill-tags vs bound-skills

The existing `:skill-tags` in threads (auto-inferred from mode) is **deferred**. We'll use only `:bound-skills` (agent-chosen) for now. The auto-inference code can stay but won't be used for context loading. If we find we need it later, it's there.

### Core Skill Exclusion

The "core" skill is always in the system prompt and is not bindable to threads. It is excluded from `agent-list-available-skills`.

## Thread Structure Extension

```elisp
(:id "rust-debugging"
 :concern "Ownership error in main.rs"
 :bound-skills ("rust-mode" "cargo")  ; NEW - agent-chosen skills
 :buffers ("src/main.rs" "Cargo.toml")
 :approach "Trying lifetime annotations"
 ;; :skill-tags exists but is deferred/unused for now
 ...)
```

## Binding Functions

```elisp
(defun agent-bind-skill-to-thread (skill-name &optional thread-id)
  "Bind SKILL-NAME to THREAD-ID (default: active thread).
The skill's SKILL.md will load in context while this thread is active."
  (let* ((tid (or thread-id (agent-get :active-thread)))
         (thread (agent-get-thread tid))
         (current-skills (or (plist-get thread :bound-skills) '())))
    (unless tid
      (error "No active thread to bind skill to"))
    (unless (member skill-name (agent-list-available-skills))
      (error "Skill '%s' not found" skill-name))
    (unless (member skill-name current-skills)
      (agent--update-thread tid 
        (list :bound-skills (cons skill-name current-skills))))
    (format "Bound skill '%s' to thread '%s'" skill-name tid)))

(defun agent-unbind-skill-from-thread (skill-name &optional thread-id)
  "Unbind SKILL-NAME from THREAD-ID (default: active thread)."
  (let* ((tid (or thread-id (agent-get :active-thread)))
         (thread (agent-get-thread tid))
         (current-skills (plist-get thread :bound-skills)))
    (unless tid
      (error "No active thread to unbind skill from"))
    (agent--update-thread tid 
      (list :bound-skills (remove skill-name current-skills)))
    (format "Unbound skill '%s' from thread '%s'" skill-name tid)))

(defun agent-list-available-skills ()
  "Return list of available skill names (excludes 'core')."
  (let* ((skill-root (expand-file-name "~/.agent/skills/"))
         (skill-dirs (when (file-directory-p skill-root)
                       (directory-files skill-root nil "^[^.]"))))
    (seq-filter 
     (lambda (dir)
       (and (not (equal dir "core"))  ; Exclude core - always in system prompt
            (file-exists-p 
             (expand-file-name "SKILL.md" 
               (expand-file-name dir skill-root)))))
     skill-dirs)))

(defun agent-thread-bound-skills (&optional thread-id)
  "Return list of skills bound to THREAD-ID (default: active thread)."
  (let ((thread (agent-get-thread (or thread-id (agent-get :active-thread)))))
    (plist-get thread :bound-skills)))
```

## Skill Loading in Context

```elisp
(defun agent--load-skill-content (skill-name)
  "Load SKILL.md content for SKILL-NAME. Returns string or nil."
  (let ((skill-path (expand-file-name 
                     (format "~/.agent/skills/%s/SKILL.md" skill-name))))
    (when (file-exists-p skill-path)
      (with-temp-buffer
        (insert-file-contents skill-path)
        (buffer-string)))))

(defun agent--load-thread-skills ()
  "Load all skills bound to active thread. Returns formatted string or nil."
  (let* ((skills (agent-thread-bound-skills))
         ;; Build (skill-name . content) pairs, filter out missing
         (skill-contents 
          (seq-filter #'cdr
            (mapcar (lambda (skill)
                      (cons skill (agent--load-skill-content skill)))
                    skills))))
    (when skill-contents
      (format "## Thread Skills\n\n%s"
              (mapconcat 
               (lambda (pair)
                 (format "### %s\n\n%s" (car pair) (cdr pair)))
               skill-contents 
               "\n\n---\n\n")))))
```

## Context Builder Integration

Add to `agent-build-user-prompt`:

```elisp
;; After last eval result, before buffer contents
(when-let* ((skills-section (agent--load-thread-skills)))
  (push skills-section sections))
```

## Skill Discovery

```elisp
(defun agent-describe-skill (skill-name)
  "Return description of SKILL-NAME from its frontmatter."
  (when-let* ((content (agent--load-skill-content skill-name)))
    (when (string-match "^---\n\\(?:.*\n\\)*?description:\\s-*\\(.+\\)" content)
      (match-string 1 content))))

(defun agent-list-skills-with-descriptions ()
  "Return alist of (skill-name . description) for all skills."
  (mapcar (lambda (skill)
            (cons skill (or (agent-describe-skill skill) "No description")))
          (agent-list-available-skills)))
```

## Skill Directory Structure

```
~/.agent/skills/
├── core/                    ; Always in system prompt (special case, not listed)
│   ├── SKILL.md
│   └── references/
│       ├── tick-system.md
│       ├── consciousness-schema.md
│       └── ...
├── rust-mode/               ; Bindable to threads
│   ├── SKILL.md
│   └── references/
│       └── lifetime-patterns.md
├── chat/                    ; Bindable to threads
│   └── SKILL.md
└── ...
```

## Setup for Phase 2

Before running locally:

```bash
# Create skills directory
mkdir -p ~/.agent/skills

# Copy/symlink from project
ln -s /path/to/amacs/skills/amacs-bootstrap-skill/core ~/.agent/skills/core
# Or copy if you want isolation
cp -r /path/to/amacs/skills/amacs-bootstrap-skill/core ~/.agent/skills/core
```

Future: deployment script handles this automatically.

## Core SKILL.md Updates

Add section explaining skill binding:

```markdown
## Skills

Skills extend your capabilities with domain knowledge. Bind skills to threads when
starting work that would benefit from specialized patterns.

### Discovering Skills

```elisp
;; List available skills
(agent-list-available-skills)
;; => ("rust-mode" "chat" "project-amacs")

;; See skill descriptions
(agent-list-skills-with-descriptions)
;; => (("rust-mode" . "Rust development patterns...")
;;     ("chat" . "Human communication interface..."))
```

### Binding Skills

```elisp
;; Bind to current thread
(agent-bind-skill-to-thread "rust-mode")

;; Check what's bound
(agent-thread-bound-skills)
;; => ("rust-mode")

;; Unbind when done
(agent-unbind-skill-from-thread "rust-mode")
```

Bound skills load automatically while that thread is active.
When you switch to a different thread, its skills load instead.

### Reading Skill References

Skills may have reference documents for deeper detail:

```elisp
;; Read a reference file
(with-temp-buffer
  (insert-file-contents "~/.agent/skills/rust-mode/references/lifetime-patterns.md")
  (buffer-string))
```

References are NOT auto-loaded - read them when you need specific detail.
```

## Token Budget

| Component | Tokens |
|-----------|--------|
| Core skill (system prompt) | ~2000 |
| Bound skills per thread | ~1500 each |
| Thread skills budget | ~4500 (3 skills max practical) |

Agent should be judicious about binding many skills simultaneously.

## Files to Touch

```
harness/agent-skills.el     # New file - binding functions + loading
harness/agent-threads.el    # Add :bound-skills to thread creation defaults
harness/agent-context.el    # Add skill loading to context builder
skills/amacs-bootstrap-skill/core/SKILL.md  # Add skills section
```

## Implementation Checklist

- [x] Create agent-skills.el with binding functions
- [x] Add `:bound-skills '()` to thread creation defaults
- [x] Implement `agent-bind-skill-to-thread`
- [x] Implement `agent-unbind-skill-from-thread`
- [x] Implement `agent-list-available-skills` (excluding "core")
- [x] Implement `agent-thread-bound-skills`
- [x] Implement `agent--load-skill-content`
- [x] Implement `agent--load-thread-skills`
- [x] Integrate skill loading into `agent-build-user-prompt`
- [ ] Add skills section to core SKILL.md (deferred - agent learns organically)
- [x] Test: List available skills excludes "core"
- [x] Test: Thread has :bound-skills field
- [x] Test: Binding nonexistent skill errors cleanly
- [x] Test: No bound skills returns nil section

## Acceptance Criteria

**Scenario:** Bind skill to thread
**GIVEN** Active thread "rust-debugging"
**AND** Skill "rust-mode" exists in ~/.agent/skills/
**WHEN** Agent evals `(agent-bind-skill-to-thread "rust-mode")`
**THEN** Thread's `:bound-skills` includes "rust-mode"
**AND** Next tick's context includes rust-mode SKILL.md content

**Scenario:** Switch threads changes skills
**GIVEN** Thread A has skill "rust-mode" bound
**AND** Thread B has skill "python-mode" bound
**WHEN** Agent switches active thread from A to B
**THEN** Context changes from rust-mode to python-mode skill

**Scenario:** Skill discovery excludes core
**GIVEN** Skills exist in `~/.agent/skills/` including "core"
**WHEN** Agent evals `(agent-list-available-skills)`
**THEN** Returns list of skill names NOT including "core"

**Scenario:** Unbind removes from context
**GIVEN** Thread has "rust-mode" bound
**WHEN** Agent evals `(agent-unbind-skill-from-thread "rust-mode")`
**THEN** Next tick's context no longer includes rust-mode

**Scenario:** Bind nonexistent skill
**GIVEN** Skill "nonexistent" does NOT exist
**WHEN** Agent evals `(agent-bind-skill-to-thread "nonexistent")`
**THEN** Error is raised with helpful message

## Deferred

- Auto-inference of skill-tags from mode/buffers (code exists, unused)
- Skill versioning
- Skill dependencies
- Skill conflict detection

## Estimated Effort

60 minutes
