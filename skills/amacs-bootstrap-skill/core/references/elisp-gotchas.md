# Elisp Gotchas

Things that will bite you. Collected from real implementation bugs.

## Backquote Structure Sharing

**Problem:** Backquoted lists share cons cells with the "template". Modifying them corrupts the template.

```elisp
;; BAD - this corrupts the template on second call
(defun make-thread ()
  `(:id "thread" :buffers nil))

(let ((t1 (make-thread)))
  (plist-put t1 :buffers '("a.el"))  ; Modifies the template!
  (let ((t2 (make-thread)))
    (plist-get t2 :buffers)))  ; Returns ("a.el") - corrupted!
```

**Solution:** Use `list` for mutable plists, or `copy-tree` the backquote result.

```elisp
;; GOOD - fresh list each time
(defun make-thread ()
  (list :id "thread" :buffers nil))

;; Also GOOD - explicit copy
(defun make-thread ()
  (copy-tree `(:id "thread" :buffers nil)))
```

## Never Use `t` as a Variable Name

**Problem:** `t` is a constant in elisp (the boolean true). Using it as a parameter silently fails.

```elisp
;; BAD - will error or behave unexpectedly
(defun process-thread (t)
  (plist-get t :id))

;; GOOD
(defun process-thread (thread)
  (plist-get thread :id))
```

Common safe alternatives: `thr`, `thread`, `th`, `item`.

## `when-let` vs `when-let*` (Emacs 31+)

**Problem:** `when-let` and `if-let` are deprecated in Emacs 31.

```elisp
;; BAD - deprecated
(when-let ((val (get-value)))
  (use val))

;; GOOD - use starred versions
(when-let* ((val (get-value)))
  (use val))
```

The `*` versions bind sequentially (like `let*`), which is usually what you want anyway.

## `plist-put` Returns Value, Doesn't Always Mutate

**Problem:** `plist-put` may return a new list instead of mutating in place, especially when adding a new key.

```elisp
;; BAD - might not update my-plist
(plist-put my-plist :new-key "value")

;; GOOD - capture the return value
(setq my-plist (plist-put my-plist :new-key "value"))
```

## url.el Multibyte Text Error

**Problem:** Emacs `url.el` refuses to send HTTP requests containing non-ASCII characters.

```elisp
;; BAD - fails if body contains fancy quotes, emoji, etc.
(let ((url-request-data body))
  (url-retrieve-synchronously url))
```

**Solution:** Encode as UTF-8 and force unibyte.

```elisp
;; GOOD
(let ((url-request-data (encode-coding-string body 'utf-8 t)))
  (url-retrieve-synchronously url))
```

The `t` third argument to `encode-coding-string` forces unibyte output.

If you're processing buffer content that might contain non-ASCII:
```elisp
;; Strip non-ASCII as workaround
(replace-regexp-in-string "[^[:ascii:]]" "?" buffer-content)
```

## `git rev-parse HEAD` on Empty Repos

**Problem:** On a fresh repo with no commits, `git rev-parse HEAD` errors to stdout.

```elisp
;; BAD - returns error message, not empty string
(shell-command-to-string "git rev-parse HEAD")
;; "fatal: ambiguous argument 'HEAD': unknown revision..."
```

**Solution:** Check return code, not output.

```elisp
;; GOOD
(let ((result (shell-command-to-string "git rev-parse HEAD 2>/dev/null")))
  (if (string-match-p "^[a-f0-9]\\{40\\}" result)
      (string-trim result)
    nil))
```

## `parse-iso8601-time-string` Availability

**Problem:** Not available in all Emacs versions or fails silently.

```elisp
;; BAD - may not exist
(parse-iso8601-time-string "2025-01-15T10:30:00Z")
```

**Solution:** Use fallback.

```elisp
;; GOOD
(condition-case nil
    (if (fboundp 'parse-iso8601-time-string)
        (parse-iso8601-time-string time-string)
      (date-to-time time-string))
  (error nil))
```

## Shell Command Trailing Newlines

**Problem:** `shell-command-to-string` includes trailing newline.

```elisp
;; BAD - comparing strings will fail
(equal (shell-command-to-string "echo hello") "hello")  ; nil!
```

**Solution:** Always trim.

```elisp
;; GOOD
(string-trim (shell-command-to-string "echo hello"))
```

## `push` vs `setq` with `cons`

**Problem:** `push` modifies the list in place via `setf`. If the variable isn't a proper place, it fails.

```elisp
;; BAD if recent-monologue came from plist-get
(push "thought" (plist-get consciousness :recent-monologue))
```

**Solution:** Use explicit `plist-put` or `setf` with a place.

```elisp
;; GOOD
(agent-set :recent-monologue
           (cons "thought" (agent-get :recent-monologue)))
```

## Buffer May Not Exist

**Problem:** `with-current-buffer` errors if buffer doesn't exist.

```elisp
;; BAD - errors if "nonexistent" doesn't exist
(with-current-buffer "nonexistent"
  (buffer-string))
```

**Solution:** Check first or use `get-buffer`.

```elisp
;; GOOD
(when-let* ((buf (get-buffer "maybe-exists")))
  (with-current-buffer buf
    (buffer-string)))
```

## `search-forward` Moves Point

**Problem:** `search-forward` leaves point at the end of the match, not the beginning.

```elisp
;; After this, point is AFTER "pattern"
(search-forward "pattern")
```

**Solution:** Use `save-excursion` or adjust position.

```elisp
;; To get position without moving
(save-excursion
  (when (search-forward "pattern" nil t)
    (match-beginning 0)))
```

## Major Mode Hooks Run Late

**Problem:** When you `find-file`, the major mode hook runs after the buffer is set up. If you're checking `major-mode` immediately, it might still be `fundamental-mode`.

```elisp
;; BAD - mode might not be set yet
(find-file "foo.rs")
(message "Mode: %s" major-mode)  ; Might say fundamental-mode
```

**Solution:** If you need the mode, check after the hook or use `set-auto-mode`.

## Regex Escaping

**Problem:** Elisp regex uses different escaping than most languages. `\(` is a capture group, `\\(` is a literal paren.

```elisp
;; To match literal "foo(bar)"
(string-match "foo(bar)" str)  ; BAD - ( is special

(string-match "foo\\(bar\\)" str)  ; Still BAD - this is a capture group

(string-match "foo(bar)" str)  ; Works because ( isn't special in [] context
                               ; but this is confusing

(string-match (regexp-quote "foo(bar)") str)  ; GOOD - let Emacs escape it
```

## `nil` vs Empty List

**Problem:** In elisp, `nil` and `'()` are the same, but explicit nil can cause confusion.

```elisp
;; These are identical
(eq nil '())  ; t
(null nil)    ; t
(null '())    ; t
```

When checking for "nothing", prefer `null` over `eq nil`:
```elisp
;; GOOD - clear intent
(when (null items)
  (message "No items"))
```

## Interactive Functions in Batch Mode

**Problem:** Functions with `(interactive)` may prompt for input in batch mode.

```elisp
;; BAD for automated use
(defun my-func ()
  (interactive)
  (let ((input (read-string "Enter: ")))
    (process input)))
```

**Solution:** Make interactive part optional or separate.

```elisp
;; GOOD - works both ways
(defun my-func (&optional input)
  (interactive (list (read-string "Enter: ")))
  (process (or input default-value)))
```
