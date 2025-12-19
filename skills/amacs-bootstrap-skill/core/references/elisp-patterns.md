# Elisp Patterns for AMACS

Practical elisp patterns you can use directly. These are real patterns, not abstractions.

## Buffer Operations

### Read buffer content
```elisp
;; Get entire buffer as string
(with-current-buffer "filename.el"
  (buffer-string))

;; Get specific region
(with-current-buffer "filename.el"
  (buffer-substring-no-properties 1 100))

;; Get current line
(thing-at-point 'line t)

;; Get word at point
(thing-at-point 'word t)
```

### Write to buffer
```elisp
;; Insert at point
(with-current-buffer "*scratch*"
  (insert "hello world"))

;; Insert at specific position
(with-current-buffer "*scratch*"
  (goto-char 42)
  (insert "inserted here"))

;; Replace region
(with-current-buffer "file.el"
  (goto-char 100)
  (delete-region 100 150)
  (insert "replacement text"))

;; Append to end
(with-current-buffer "*scratch*"
  (goto-char (point-max))
  (insert "\nappended line"))
```

### Switch buffers
```elisp
;; Switch to existing buffer
(switch-to-buffer "filename.el")

;; Switch without selecting window (background)
(set-buffer "filename.el")

;; Get or create buffer
(get-buffer-create "*my-buffer*")
```

### Buffer information
```elisp
;; List all buffer names
(mapcar #'buffer-name (buffer-list))

;; Check if buffer exists
(get-buffer "filename.el")  ; returns buffer or nil

;; Get buffer's major mode
(buffer-local-value 'major-mode (get-buffer "file.el"))

;; Check if modified
(buffer-modified-p (get-buffer "file.el"))
```

## File Operations

### Read file
```elisp
;; Read file into string (without opening buffer)
(with-temp-buffer
  (insert-file-contents "/path/to/file")
  (buffer-string))

;; Open file in buffer
(find-file "/path/to/file")

;; Open file read-only
(find-file-read-only "/path/to/file")
```

### Write file
```elisp
;; Save current buffer
(save-buffer)

;; Write buffer to specific file
(write-file "/path/to/newfile")

;; Write string to file
(with-temp-file "/path/to/file"
  (insert "content here"))

;; Append to file
(append-to-file "new content\n" nil "/path/to/file")
```

### File checks
```elisp
;; Check existence
(file-exists-p "/path/to/file")

;; Check if directory
(file-directory-p "/path/to/dir")

;; Check if readable
(file-readable-p "/path/to/file")

;; Get file size
(file-attribute-size (file-attributes "/path/to/file"))
```

## Shell and Process

### Run shell command
```elisp
;; Get output as string
(shell-command-to-string "ls -la")

;; Run and capture stderr too
(shell-command-to-string "cargo build 2>&1")

;; Run silently (no output buffer)
(call-process-shell-command "git add -A" nil 0)
```

### Eshell interaction
```elisp
;; Send command to eshell
(with-current-buffer "*eshell*"
  (goto-char (point-max))
  (insert "ls -la")
  (eshell-send-input))

;; Start eshell if not running
(unless (get-buffer "*eshell*")
  (eshell))
```

### Async process
```elisp
;; Start async process
(start-process "my-process" "*output*" "long-running-command")

;; Check if process is running
(get-process "my-process")
```

## Navigation

### Move point
```elisp
;; Go to position
(goto-char 42)

;; Go to line
(goto-line 10)

;; Beginning/end of buffer
(goto-char (point-min))
(goto-char (point-max))

;; Beginning/end of line
(beginning-of-line)
(end-of-line)
```

### Search
```elisp
;; Search forward
(search-forward "pattern" nil t)  ; t = don't error if not found

;; Search backward
(search-backward "pattern" nil t)

;; Regex search
(re-search-forward "def \\(\\w+\\)" nil t)

;; Get match
(match-string 1)  ; Get first capture group
```

### Position info
```elisp
;; Current position
(point)

;; Current line number
(line-number-at-pos)

;; Current column
(current-column)
```

## String Operations

### Basic manipulation
```elisp
;; Concatenate
(concat "hello" " " "world")

;; Format
(format "Value: %d, Name: %s" 42 "test")

;; Substring
(substring "hello world" 0 5)  ; "hello"

;; Replace
(replace-regexp-in-string "old" "new" "old string old")
```

### Trim and split
```elisp
;; Trim whitespace
(string-trim "  hello  ")
(string-trim-right "hello\n")

;; Split
(split-string "a,b,c" ",")  ; ("a" "b" "c")

;; Join
(string-join '("a" "b" "c") ",")  ; "a,b,c"
```

## Working with Plists

```elisp
;; Get value
(plist-get '(:a 1 :b 2) :a)  ; 1

;; Set value (returns new plist)
(plist-put '(:a 1 :b 2) :a 99)  ; (:a 99 :b 2)

;; Check membership
(plist-member '(:a 1 :b 2) :a)  ; (:a 1 :b 2)
```

## Error Handling

```elisp
;; Catch errors
(condition-case err
    (risky-operation)
  (error (message "Error: %s" (error-message-string err))))

;; Ignore errors
(ignore-errors
  (might-fail))

;; Unwind protect (like finally)
(unwind-protect
    (do-something)
  (always-cleanup))
```

## AMACS-Specific Functions

### Thread management
```elisp
;; Create and activate thread
(agent-add-thread
  (agent-create-thread "Fix the parser bug" '("src/parser.rs"))
  t)  ; t = activate immediately

;; Switch threads
(agent-switch-thread "other-thread-id")

;; Complete thread
(agent-complete-thread "thread-id"
  :evidence '(:output "tests pass" :files-changed ("src/main.rs"))
  :learned "Key insight from this work")

;; Get active thread
(agent-get-active-thread)

;; Get pending threads
(agent-get-pending-threads)
```

### Consciousness access
```elisp
;; Read consciousness field
(agent-get :mood)
(agent-get :confidence)
(agent-get :active-thread)

;; Set consciousness field
(agent-set :mood :curious)
(agent-set-confidence 0.8)

;; Record action
(agent-record-action "eval-elisp" 0.85)
```

### Monologue
```elisp
;; Append to monologue
(agent-append-monologue "Found the bug in line 42")

;; Search monologue
(shell-command-to-string "rg 'pattern' ~/.agent/monologue.org")
```

### Git operations
```elisp
;; Manual commit (usually handled by harness)
(agent-git-commit "[TICK N][thread][:mood] message")

;; Get last commit
(agent-get :last-commit)

;; Search git history
(shell-command-to-string "git log --oneline --grep='keyword'")
```

## Useful Idioms

### Do something in another buffer without switching
```elisp
(with-current-buffer "other-buffer"
  (do-something))
;; Point and buffer unchanged after
```

### Save excursion (restore point after)
```elisp
(save-excursion
  (goto-char (point-min))
  (search-forward "pattern")
  ;; do stuff
  )
;; Point restored to original position
```

### Narrow to region (temporary focus)
```elisp
(save-restriction
  (narrow-to-region start end)
  ;; Only this region is visible
  (do-operations))
;; Full buffer visible again
```

### Batch multiple changes
```elisp
(with-silent-modifications
  ;; Changes don't trigger modification hooks
  (insert "stuff"))
```

## Debugging

```elisp
;; Print to *Messages*
(message "Debug: %S" some-value)

;; Pretty print
(pp some-complex-structure)

;; Describe variable
(describe-variable 'agent-consciousness)

;; Describe function
(describe-function 'agent-think)
```
