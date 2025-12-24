# AMACS Elisp Style Guide

This document defines coding conventions for the AMACS harness and skills. Consistency aids both human maintainers and the agent itself when reading/writing code.

## Data Structures

### Alists for State

All persistent state uses alists with symbol keys:

```elisp
;; Good - alist with symbol keys
'((identity . "amacs-instance-1")
  (mood . "focused")
  (confidence . 0.85)
  (open-threads . ()))

;; Avoid - plist
'(:identity "amacs-instance-1" :mood "focused" ...)
```

**Why:** `alist-get` supports defaults, `setf` works cleanly, direct JSON mapping.

### Accessors

```elisp
;; Reading with default
(alist-get 'mood agent-consciousness "neutral")

;; Writing
(setf (alist-get 'mood agent-consciousness) "curious")

;; Removing
(setf agent-consciousness 
      (assoc-delete-all 'temporary-key agent-consciousness))
```

### Nested Structures

For deeply nested data, prefer flat keys or helper functions:

```elisp
;; Prefer flat when possible
'((budget-cost . 1.23)
  (budget-limit . 5.0)
  (budget-pressure . low))

;; Or use accessor functions for nested
(defun agent-budget-cost ()
  (alist-get 'cost (alist-get 'budget agent-consciousness)))
```

## Naming Conventions

### Functions

```elisp
agent-verb-noun          ; Public API
agent--private-helper    ; Internal (double dash)
agent-noun-p             ; Predicates end in -p
```

Examples:
- `agent-create-thread`
- `agent--parse-response`
- `agent-chat-pending-p`

### Variables

```elisp
agent-noun               ; Public variable
agent--internal-cache    ; Internal variable
```

### Constants

```elisp
agent-default-mood       ; Default values
agent-max-threads        ; Limits
```

## File Organization

Each file should:
1. Start with lexical-binding header
2. Have Commentary section explaining purpose
3. Group related functions with `;;; Section` comments
4. End with `(provide 'agent-module)`

```elisp
;;; agent-example.el --- Brief description -*- lexical-binding: t; -*-

;; Copyright (C) 2025 AMACS Project
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Longer explanation of what this module does.
;; Reference relevant IMPs/ADRs.

;;; Code:

(require 'agent-consciousness)

;;; Section Name

(defun agent-example-function ()
  "Docstring."
  ...)

(provide 'agent-example)
;;; agent-example.el ends here
```

## Sequence Operations

Prefer `seq-*` functions for generic operations:

```elisp
;; Good
(seq-filter #'agent-thread-active-p threads)
(seq-take items 10)
(seq-find (lambda (x) (equal (alist-get 'id x) target)) items)

;; Also fine for simple cases
(mapcar #'buffer-name buffers)
(dolist (item items) ...)
```

Avoid `cl-*` unless specifically needed (e.g., `cl-loop` for complex iteration).

## Error Handling

### Recoverable Errors

Use `condition-case` when you can handle the error:

```elisp
(defun agent-safe-read-file (path)
  "Read file, return nil on error."
  (condition-case err
      (with-temp-buffer
        (insert-file-contents path)
        (buffer-string))
    (file-error
     (message "Could not read %s: %s" path (error-message-string err))
     nil)))
```

### User-Facing Errors

Use `user-error` for problems the user should see:

```elisp
(unless (agent-api-configured-p)
  (user-error "API not configured. Set OPENROUTER_API_KEY"))
```

### Never Swallow Silently

```elisp
;; Bad - silent failure
(ignore-errors (dangerous-operation))

;; Good - at least log
(condition-case err
    (dangerous-operation)
  (error (message "Operation failed: %s" err)))
```

## Documentation

### Docstrings

Every public function needs a docstring:

```elisp
(defun agent-create-thread (concern &optional buffers)
  "Create a new thread for CONCERN with optional BUFFERS.
Returns the thread alist (not yet added to consciousness).
BUFFERS defaults to current buffer if nil."
  ...)
```

### Comments

```elisp
;; Single line comment for context

;; Multi-line comments should
;; break at reasonable points

;;; Section headers use three semicolons
```

## JSON Interop

Since alists map directly to JSON objects:

```elisp
;; Elisp alist
'((name . "test") (value . 42) (active . t))

;; Encodes to JSON
{"name": "test", "value": 42, "active": true}

;; Reading JSON
(json-parse-string str :object-type 'alist)
```

Use `json-encode` for output, `json-parse-string` for input.

## Buffer Operations

### Always Specify Buffer

```elisp
;; Good - explicit buffer
(with-current-buffer buf
  (buffer-string))

;; Risky - assumes current buffer
(buffer-string)
```

### Check Buffer Exists

```elisp
(when-let* ((buf (get-buffer name)))
  (with-current-buffer buf
    ...))
```

### Mode Checks

```elisp
;; Check if mode is active in a buffer
(buffer-local-value 'amacs-chat-mode buf)

;; Find buffers by mode
(seq-filter (lambda (b) (buffer-local-value 'target-mode b))
            (buffer-list))
```

## Testing

Tests go in `test-harness.el`. Pattern:

```elisp
(defun test-agent-feature ()
  "Test description."
  ;; Setup
  (let ((agent-consciousness (agent--default-consciousness)))
    ;; Action
    (agent-set 'mood "tested")
    ;; Assert
    (cl-assert (equal (alist-get 'mood agent-consciousness) "tested")
               nil "Mood should be updated")))
```

Run all tests: `M-x agent-run-tests`

## Common Patterns

### Safe Property Access

```elisp
(or (alist-get 'optional-field data) default-value)
```

### Transform and Update

```elisp
(setf (alist-get 'count data)
      (1+ (or (alist-get 'count data) 0)))
```

### Building Alists

```elisp
;; Simple
`((id . ,id)
  (name . ,name)
  (timestamp . ,(current-time)))

;; Conditional fields
(let ((result `((id . ,id))))
  (when name
    (push `(name . ,name) result))
  (nreverse result))
```

## What to Avoid

- **Mutation surprises:** Don't use `setcar`/`setcdr` on shared structure
- **Magic numbers:** Use named constants or consciousness fields
- **Deep nesting:** Flatten or extract helper functions
- **Silent failures:** Always log or signal errors
- **Hardcoded paths:** Use `expand-file-name` with base directories
