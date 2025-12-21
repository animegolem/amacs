---
node_id: AI-IMP-023
tags:
  - IMP
  - phase-2
  - skills
  - threads
  - context
status: draft
depends_on:
  - AI-IMP-019
implements: AI-EPIC-002
created_date: 2025-12-20
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

## Design

### Thread Structure Extension

```elisp
(:id "rust-debugging"
 :concern "Ownership error in main.rs"
 :bound-skills ("rust-mode" "cargo")  ; NEW
 :buffers ("src/main.rs" "Cargo.toml")
 :approach "Trying lifetime annotations"
 ...)
```

### Binding Functions

```elisp
(defun agent-bind-skill-to-thread (skill-name &optional thread-id)
  "Bind SKILL-NAME to THREAD-ID (default: active thread).
The skill's SKILL.md will load in context while this thread is active."
  (let* ((tid (or thread-id (agent-active-thread)))
         (thread (agent-get-thread tid))
         (current-skills (plist-get thread :bound-skills)))
    (unless (member skill-name current-skills)
      (agent-update-thread tid :bound-skills (cons skill-name current-skills)))
    (format "Bound skill '%s' to thread '%s'" skill-name tid)))

(defun agent-unbind-skill-from-thread (skill-name &optional thread-id)
  "Unbind SKILL-NAME from THREAD-ID (default: active thread)."
  (let* ((tid (or thread-id (agent-active-thread)))
         (thread (agent-get-thread tid))
         (current-skills (plist-get thread :bound-skills)))
    (agent-update-thread tid :bound-skills (remove skill-name current-skills))
    (format "Unbound skill '%s' from thread '%s'" skill-name tid)))

(defun agent-list-available-skills ()
  "Return list of available skill names."
  (let ((skill-dirs (directory-files 
                     (expand-file-name "~/.agent/skills/") 
                     nil "^[^.]")))
    (seq-filter (lambda (dir)
                  (file-exists-p 
                   (expand-file-name "SKILL.md" 
                     (expand-file-name dir "~/.agent/skills/"))))
                skill-dirs)))

(defun agent-thread-bound-skills (&optional thread-id)
  "Return list of skills bound to THREAD-ID (default: active thread)."
  (let ((thread (agent-get-thread (or thread-id (agent-active-thread)))))
    (plist-get thread :bound-skills)))
```

### Skill Loading in Context

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
  "Load all skills bound to active thread. Returns formatted string."
  (let* ((skills (agent-thread-bound-skills))
         (contents (seq-filter #'identity 
                    (mapcar #'agent--load-skill-content skills))))
    (when contents
      (format "## Thread Skills\n\n%s"
              (mapconcat (lambda (content)
                          (format "### %s\n\n%s" 
                                  (car skills) content))
                        contents "\n\n---\n\n")))))
```

### Context Builder Integration

Add to `agent-build-user-prompt`:

```elisp
;; After last eval result, before buffer contents
(when-let* ((skills-section (agent--load-thread-skills)))
  (push skills-section sections))
```

### Skill Discovery

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
├── core/                    ; Always in system prompt (special case)
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

**Note:** The `core` skill is special - it becomes the system prompt (see IMP-020), not bound to threads.

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
;; => (("rust-mode" . "Rust development patterns for ownership...")
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
harness/agent-skills.el     # Add binding functions
harness/agent-threads.el    # Add :bound-skills to thread structure
harness/agent-context.el    # Add skill loading to context builder
skills/amacs-bootstrap-skill/core/SKILL.md  # Add skills section
```

## Implementation Checklist

- [ ] Add `:bound-skills` field to thread creation
- [ ] Implement `agent-bind-skill-to-thread`
- [ ] Implement `agent-unbind-skill-from-thread`
- [ ] Implement `agent-list-available-skills`
- [ ] Implement `agent-thread-bound-skills`
- [ ] Implement `agent--load-skill-content`
- [ ] Implement `agent--load-thread-skills`
- [ ] Integrate skill loading into `agent-build-user-prompt`
- [ ] Add skills section to core SKILL.md
- [ ] Test: Bind skill, verify appears in context
- [ ] Test: Switch thread, verify different skills load
- [ ] Test: Unbind skill, verify removed from context
- [ ] Test: List available skills works

## Acceptance Criteria

**Scenario:** Bind skill to thread
**GIVEN** Active thread "rust-debugging"
**WHEN** Agent evals `(agent-bind-skill-to-thread "rust-mode")`
**THEN** Thread's `:bound-skills` includes "rust-mode"
**AND** Next tick's context includes rust-mode SKILL.md content

**Scenario:** Switch threads changes skills
**GIVEN** Thread A has skill "rust-mode" bound
**AND** Thread B has skill "python-mode" bound
**WHEN** Agent switches active thread from A to B
**THEN** Context changes from rust-mode to python-mode skill

**Scenario:** Skill discovery
**GIVEN** Skills exist in `~/.agent/skills/`
**WHEN** Agent evals `(agent-list-available-skills)`
**THEN** Returns list of skill directory names

**Scenario:** Unbind removes from context
**GIVEN** Thread has "rust-mode" bound
**WHEN** Agent evals `(agent-unbind-skill-from-thread "rust-mode")`
**THEN** Next tick's context no longer includes rust-mode

## Estimated Effort

60 minutes
