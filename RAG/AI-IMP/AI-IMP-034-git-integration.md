---
node_id: AI-IMP-034
tags:
  - IMP-LIST
  - Implementation
  - git
  - monologue
  - commits
  - tick
kanban_status: complete
depends_on:
  - AI-EPIC-003
confidence_score: 0.8
created_date: 2025-12-30
close_date: 2025-12-30
---

# AI-IMP-034: Git Integration (Tick Commits)

## Summary

Implement git-backed autobiographical memory for the agent. Each tick creates a commit in `~/.agent/` with a structured message encoding tick metadata.

**Current state:** No git integration. Monologue is append-only file but no commit history. Hub cannot show diffs per tick.

**Target state:** `~/.agent/` is a git repo. Each tick commits all changes with message format using `‖` delimiter. Hub can parse git log to retrieve diffs.

**Why:**
- Event-sourced history: every tick is a recoverable snapshot
- Enables monologue+diff in hub (FR-17)
- True autobiographical memory - agent can `git show` any past state
- Structured commit messages enable semantic queries ("what was I doing when confident?")

**Done when:** Tick completion triggers commit. `git log --oneline ~/.agent` shows tick history. Hub can retrieve diff for any tick.

### Out of Scope

- Bi-encoder/semantic search over commits (Phase 4)
- External project integration (symlinks, submodules)
- Push to remote (manual user choice)

### Design/Approach

**Repository structure:**
```
~/.agent/                    # Git root
├── .git/
├── .gitignore               # Excludes secrets, logs
├── config.el                # API key - NEVER COMMITTED
├── consciousness.el         # Tracked - state snapshots
├── monologue.org            # Tracked - narrative log
├── skills/                  # Tracked - installed skills
└── workspace/               # Future: agent projects
```

**Commit message format:**
```
Tick 42 ‖ rust-debugging ‖ focused ‖ 0.85 ‖ Investigating lifetime annotations
```

Fields: `Tick {N} ‖ {thread-name or "no-thread"} ‖ {mood} ‖ {confidence} ‖ {monologue-line}`

Monologue is last field so parsing can use greedy capture (handles `|` or other chars in monologue text). Delimiter `‖` (U+2016, double vertical line) is rare in natural text.

**Initialization:**
```elisp
(defun agent-git-init ()
  "Initialize git repo in ~/.agent/ if not present."
  (let ((default-directory (expand-file-name "~/.agent/")))
    (unless (file-directory-p ".git")
      (shell-command "git init")
      (agent--write-gitignore)
      (shell-command "git add -A")
      (shell-command "git commit -m 'Initial commit'"))))
```

**Gitignore contents:**
```gitignore
# Credentials - NEVER track
config.el
*.key
*.pem
credentials.*

# Logs and temp
api-log/
*.log

# Emacs artifacts
.#*
*~
\#*\#

# System
.DS_Store
Thumbs.db
```

**Commit per tick:**
```elisp
(defvar agent-commit-delimiter " ‖ "
  "Delimiter for structured commit messages. U+2016 double vertical line.")

(defun agent-commit-tick (monologue-line)
  "Commit current state with structured message.
Called at end of tick after monologue append."
  (let* ((default-directory (expand-file-name "~/.agent/"))
         (tick (agent-current-tick))
         (thread (or (alist-get 'concern (agent-active-thread)) "no-thread"))
         (mood (agent-mood))
         (confidence (agent-confidence))
         (msg (format "Tick %d%s%s%s%s%s%.2f%s%s"
                      tick agent-commit-delimiter
                      thread agent-commit-delimiter
                      mood agent-commit-delimiter
                      confidence agent-commit-delimiter
                      monologue-line)))
    (shell-command "git add -A")
    (shell-command (format "git commit -m %s --allow-empty"
                           (shell-quote-argument msg)))))
```

**Parsing commit messages:**
```elisp
(defun agent--parse-commit-message (msg)
  "Parse tick commit message into alist.
Format: Tick N ‖ thread ‖ mood ‖ confidence ‖ monologue..."
  (let ((parts (split-string msg " ‖ ")))
    (when (>= (length parts) 5)
      `((tick . ,(string-to-number (replace-regexp-in-string "^Tick " "" (nth 0 parts))))
        (thread . ,(nth 1 parts))
        (mood . ,(nth 2 parts))
        (confidence . ,(string-to-number (nth 3 parts)))
        ;; Monologue is everything from field 5 onward (rejoin if split)
        (monologue . ,(mapconcat #'identity (nthcdr 4 parts) " ‖ "))))))
```

**Retrieving tick commit:**
```elisp
(defun agent-get-tick-commit (tick-number)
  "Return commit hash for TICK-NUMBER, or nil if not found."
  (let ((default-directory (expand-file-name "~/.agent/")))
    (let ((result (string-trim
                   (shell-command-to-string
                    (format "git log --oneline --grep='^Tick %d ‖' --format='%%H' -1"
                            tick-number)))))
      (unless (string-empty-p result) result))))

(defun agent-get-tick-diff (tick-number)
  "Return diff string for TICK-NUMBER commit."
  (let ((hash (agent-get-tick-commit tick-number)))
    (when hash
      (let ((default-directory (expand-file-name "~/.agent/")))
        (shell-command-to-string
         (format "git show --stat --patch %s" hash))))))
```

**Integration with tick system:**
Hook into `agent-tick-complete` or equivalent to call `agent-commit-tick`.

### Files to Touch

- `harness/agent-git.el`: New file - git init, commit, query functions
- `harness/agent-core.el`: Call `agent-git-init` during init
- `harness/agent-tick.el`: Call `agent-commit-tick` at tick end
- `harness/agent-monologue.el`: Pass monologue line to commit function

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**?
</CRITICAL_RULE>

- [ ] Create `harness/agent-git.el` with header/requires
- [ ] Define `agent-commit-delimiter` constant (U+2016)
- [ ] Implement `agent--write-gitignore` with security exclusions
- [ ] Implement `agent-git-init` to initialize repo
- [ ] Implement `agent-git-initialized-p` predicate
- [ ] Implement `agent-commit-tick` with structured message format
- [ ] Implement `agent--parse-commit-message` to parse message into alist
- [ ] Implement `agent-get-tick-commit` to find commit by tick number
- [ ] Implement `agent-get-tick-diff` to retrieve diff for tick
- [ ] Implement `agent-get-tick-commit-info` combining hash + parsed message
- [ ] Add `agent-git-init` call to `agent-init` in agent-core.el
- [ ] Hook `agent-commit-tick` into tick completion flow
- [ ] Update `agent-append-monologue` to return line for commit
- [ ] Test: git init creates repo with .gitignore
- [ ] Test: config.el is not tracked
- [ ] Test: tick creates commit with correct message format
- [ ] Test: can retrieve commit hash by tick number
- [ ] Test: can retrieve diff for tick
- [ ] Test: handles empty tick (no changes) gracefully
- [ ] Test: handles special characters in monologue line
- [ ] Test: parse handles monologue containing regular `|` chars
- [ ] Document git integration in README

### Acceptance Criteria

**Scenario:** First agent init creates git repo
**GIVEN** fresh `~/.agent/` directory (no .git)
**WHEN** `agent-init` is called
**THEN** `~/.agent/.git/` exists
**AND** `.gitignore` excludes config.el
**AND** initial commit exists

**Scenario:** Tick creates commit
**GIVEN** agent initialized, tick 5 completes
**WHEN** monologue line "Debugging the parser" is appended
**THEN** git commit created with message "Tick 5 ‖ {thread} ‖ {mood} ‖ {confidence} ‖ Debugging the parser"
**AND** consciousness.el changes are in commit
**AND** monologue.org changes are in commit

**Scenario:** Retrieve tick diff
**GIVEN** tick 5 commit exists
**WHEN** `(agent-get-tick-diff 5)` is called
**THEN** returns diff output showing changes from that tick
**AND** diff includes file stats and patch

**Scenario:** Config.el never committed
**GIVEN** `~/.agent/config.el` contains API key
**WHEN** any tick commits
**THEN** `git status` shows config.el as untracked
**AND** `git log -p` never contains API key

**Scenario:** Parse commit with pipes in monologue
**GIVEN** tick 42 commit with message "Tick 42 ‖ rust ‖ focused ‖ 0.85 ‖ Checking A | B | C options"
**WHEN** `(agent--parse-commit-message msg)` is called
**THEN** monologue field is "Checking A | B | C options" (preserved)

### Issues Encountered

<!-- Fill during implementation -->
