---
node_id: AI-IMP-014
tags:
  - IMP
  - bootstrap-skill
  - elisp
  - patterns
status: draft
depends_on: []
implements: AI-EPIC-001c
created_date: 2025-12-18
related_files:
  - skills/amacs-bootstrap-skill/core/references/elisp-patterns.md
confidence_score: 0.85
---

# AI-IMP-014: elisp-patterns.md (NEW)

## Objective

Create a new reference file with practical, copy-paste ready elisp patterns for common operations. This is the "how do I actually do X" reference.

## File Location

`skills/amacs-bootstrap-skill/core/references/elisp-patterns.md`

## Content

```markdown
# Elisp Patterns

Practical patterns for common operations. Copy, adapt, use.

## Buffer Operations

### Read buffer contents
```elisp
;; Get entire buffer as string
(with-current-buffer "filename.txt"
  (buffer-string))

;; Get specific region
(with-current-buffer "filename.txt"
  (buffer-substring-no-properties 1 100))

;; Get current line
(thing-at-point 'line t)
```

### Switch buffers
```elisp
;; Switch to existing buffer
(switch-to-buffer "*scratch*")

;; Switch, create if needed
(switch-to-buffer (get-buffer-create "my-buffer"))

;; Work in buffer without switching display
(with-current-buffer "other-buffer"
  ;; operations here
  )
```

### Write to buffers
```elisp
;; Insert at point
(insert "hello world")

;; Insert at end
(with-current-buffer "target"
  (goto-char (point-max))
  (insert "\nnew line"))

;; Replace entire contents
(with-current-buffer "target"
  (erase-buffer)
  (insert "new contents"))
```

### Navigate in buffers
```elisp
;; Go to position
(goto-char 100)
(goto-char (point-min))  ; beginning
(goto-char (point-max))  ; end

;; Go to line
(goto-line 42)

;; Move by units
(forward-char 5)
(backward-word 2)
(forward-line 3)
```

### Search in buffers
```elisp
;; Find string, return position or nil
(with-current-buffer "target"
  (goto-char (point-min))
  (search-forward "pattern" nil t))

;; Find with regex
(re-search-forward "def\\s-+\\(\\w+\\)" nil t)

;; Get match
(match-string 1)  ; first capture group
```

### Buffer metadata
```elisp
;; List all buffers
(mapcar #'buffer-name (buffer-list))

;; Check if buffer exists
(get-buffer "name")  ; returns buffer or nil

;; Buffer file name
(buffer-file-name)

;; Is modified?
(buffer-modified-p)
```

## File Operations

### Read file
```elisp
;; Into new buffer
(find-file "/path/to/file.txt")

;; Into string (no buffer)
(with-temp-buffer
  (insert-file-contents "/path/to/file.txt")
  (buffer-string))
```

### Write file
```elisp
;; Save current buffer
(save-buffer)

;; Write buffer to specific file
(write-region (point-min) (point-max) "/path/to/output.txt")

;; Append to file
(append-to-file "text to append\n" nil "/path/to/file.txt")
```

### File checks
```elisp
;; Exists?
(file-exists-p "/path/to/file")

;; Directory?
(file-directory-p "/path")

;; Readable?
(file-readable-p "/path/to/file")

;; List directory
(directory-files "/path" nil ".*\\.el$")  ; .el files only
```

## Shell Interaction

### Run command, get output
```elisp
;; Simple command
(shell-command-to-string "ls -la")

;; With stderr
(shell-command-to-string "cargo build 2>&1")

;; Check exit code
(call-process "grep" nil nil nil "-q" "pattern" "file")
;; returns 0 for found, 1 for not found
```

### Eshell operations
```elisp
;; Ensure eshell exists
(unless (get-buffer "*eshell*")
  (eshell))

;; Send command to eshell
(with-current-buffer "*eshell*"
  (goto-char (point-max))
  (insert "ls -la")
  (eshell-send-input))

;; Read eshell output (after command completes)
(with-current-buffer "*eshell*"
  (buffer-string))
```

### Async processes
```elisp
;; Start process, don't wait
(start-process "my-build" "*build-output*" "cargo" "build")

;; Check if process is running
(get-process "my-build")

;; Kill process
(delete-process "my-build")
```

## Defining Functions

### Simple function
```elisp
(defun my-helper ()
  "Do something useful."
  (message "Hello from helper"))
```

### With arguments
```elisp
(defun my-insert (text)
  "Insert TEXT at point."
  (insert text))
```

### With optional arguments
```elisp
(defun my-greet (&optional name)
  "Greet NAME or world."
  (message "Hello, %s!" (or name "world")))
```

### Interactive (can call with M-x)
```elisp
(defun my-command ()
  "A command you can M-x."
  (interactive)
  (message "Command executed"))
```

### Persist functions to file
```elisp
;; Write function to skills file for permanence
(append-to-file
 "(defun my-helper ()
    \"My helper function.\"
    (message \"I persist\"))\n\n"
 nil
 "~/.agent/skills/custom/functions.el")

;; Load the file to make functions available
(load "~/.agent/skills/custom/functions.el")
```

## Working with Data

### Lists
```elisp
;; Create
(list 1 2 3)
'(a b c)

;; Access
(car '(1 2 3))      ; => 1
(cdr '(1 2 3))      ; => (2 3)
(nth 2 '(a b c d))  ; => c

;; Modify (destructive)
(push 'new my-list)
(pop my-list)

;; Transform
(mapcar #'1+ '(1 2 3))           ; => (2 3 4)
(seq-filter #'evenp '(1 2 3 4))  ; => (2 4)
```

### Property lists (plists)
```elisp
;; Create
'(:name "amacs" :version 1 :active t)

;; Access
(plist-get my-plist :name)

;; Modify (returns new plist)
(plist-put my-plist :name "new-name")

;; Check membership
(plist-member my-plist :name)
```

### Strings
```elisp
;; Concatenate
(concat "hello" " " "world")

;; Format
(format "Value: %d, Name: %s" 42 "test")

;; Split
(split-string "a,b,c" ",")

;; Trim
(string-trim "  spaces  ")

;; Replace
(replace-regexp-in-string "old" "new" "old text")
```

## Control Flow

### Conditionals
```elisp
;; if
(if condition
    then-form
  else-form)

;; when (no else)
(when condition
  do-this
  and-this)

;; unless (inverted when)
(unless condition
  do-this)

;; cond (multiple conditions)
(cond
 ((= x 1) "one")
 ((= x 2) "two")
 (t "other"))
```

### Let bindings
```elisp
;; Local variables
(let ((x 1)
      (y 2))
  (+ x y))

;; Sequential binding (let*)
(let* ((x 1)
       (y (+ x 1)))  ; y can use x
  y)
```

### Error handling
```elisp
;; Catch errors
(condition-case err
    (risky-operation)
  (error (message "Error: %s" err)))

;; Ignore errors
(ignore-errors
  (might-fail))

;; Unwind-protect (finally)
(unwind-protect
    (do-something)
  (always-cleanup))
```

## Useful Patterns

### Check then act
```elisp
(when-let* ((buf (get-buffer "target"))
            (content (with-current-buffer buf (buffer-string))))
  (process content))
```

### Accumulate results
```elisp
(let ((results '()))
  (dolist (item items)
    (push (process item) results))
  (nreverse results))
```

### Temporary buffer for work
```elisp
(with-temp-buffer
  (insert some-text)
  (goto-char (point-min))
  ;; process...
  (buffer-string))
```

### Save excursion (restore point after)
```elisp
(save-excursion
  (goto-char (point-min))
  (search-forward "pattern")
  ;; point restored after this block
  )
```
```

## Acceptance Criteria

Given the new elisp-patterns.md
When an agent needs to perform a common operation
Then it can:
- [ ] Find a relevant pattern quickly (clear headings)
- [ ] Copy and adapt the example
- [ ] Understand what each pattern does (brief comments)

The file should:
- [ ] Cover buffer, file, shell, function, data, and control flow operations
- [ ] Be practical (real code, not abstract descriptions)
- [ ] Be concise (no lengthy explanations)

## Estimated Effort

60 minutes
