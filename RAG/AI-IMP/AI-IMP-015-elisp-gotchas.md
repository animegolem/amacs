---
node_id: AI-IMP-015
tags:
  - IMP
  - bootstrap-skill
  - elisp
  - gotchas
  - bugs
status: draft
depends_on: []
implements: AI-EPIC-001c
created_date: 2025-12-18
related_files:
  - skills/amacs-bootstrap-skill/core/references/elisp-gotchas.md
  - harness/agent-consciousness.el
  - harness/agent-tick.el
confidence_score: 0.9
---

# AI-IMP-015: elisp-gotchas.md (NEW)

## Objective

Create a reference file documenting elisp gotchas we've discovered during AMACS implementation. These are real bugs that cost debugging time - crystallizing them prevents the agent (and future developers) from hitting the same issues.

## File Location

`skills/amacs-bootstrap-skill/core/references/elisp-gotchas.md`

## Content

```markdown
# Elisp Gotchas

Hard-won lessons from AMACS development. These bugs bite.

## Backquote Structure Sharing

**Problem:** Backquoted lists can share structure with the "template," causing mysterious mutations.

```elisp
;; BROKEN - modifying result affects the template!
(defun make-thread ()
  `(:id "new" :buffers nil))

(let ((t1 (make-thread))
      (t2 (make-thread)))
  (push "file.el" (plist-get t1 :buffers))
  ;; Surprise: t2's :buffers is ALSO affected
  (plist-get t2 :buffers))  ; => ("file.el") !!!
```

**Fix:** Use `list` or `copy-tree` for mutable structures.

```elisp
;; CORRECT - fresh list each time
(defun make-thread ()
  (list :id "new" :buffers (list)))

;; Or copy if you must use backquote
(defun make-thread ()
  (copy-tree `(:id "new" :buffers nil)))
```

**Why:** Backquote creates structure at read time, not eval time. The "nil" is literally the same cons cell every call.

---

## Constant Symbol Assignment

**Problem:** Can't `setq` a symbol that appears literally in a `defvar` initialization.

```elisp
(defvar my-state '(:count 0))

;; BROKEN - "Attempt to set a constant"
(setq my-state '(:count 1))
```

**Fix:** Use `setf` with accessors, or initialize with a function call.

```elisp
;; Option 1: Modify in place
(setf (plist-get my-state :count) 1)

;; Option 2: Initialize with list call
(defvar my-state (list :count 0))
(setq my-state (list :count 1))  ; now works
```

**Why:** The quoted list becomes a constant in the byte-compiled code. Emacs protects constants from assignment.

---

## UTF-8 Encoding for HTTP

**Problem:** `url.el` doesn't encode POST body by default, causing API errors with non-ASCII.

```elisp
;; BROKEN - may send malformed data
(let ((url-request-data body))
  (url-retrieve-synchronously url))
```

**Fix:** Explicitly encode to UTF-8.

```elisp
;; CORRECT
(let ((url-request-data (encode-coding-string body 'utf-8 t)))
  (url-retrieve-synchronously url))
```

**Why:** Emacs strings are internally multibyte. HTTP expects specific encoding.

---

## when-let vs when-let*

**Problem:** `when-let` is deprecated in Emacs 31+.

```elisp
;; DEPRECATED - compiler warning
(when-let ((x (get-value))
           (y (process x)))
  (use y))
```

**Fix:** Always use `when-let*` (sequential binding).

```elisp
;; CORRECT - works in all modern Emacs
(when-let* ((x (get-value))
            (y (process x)))
  (use y))
```

**Why:** `when-let` had confusing parallel vs sequential semantics. `when-let*` is unambiguous.

---

## Timestamp Parsing

**Problem:** `parse-time-string` returns a 9-element list, not a timestamp.

```elisp
;; BROKEN - returns weird list
(parse-time-string "2025-12-18T10:30:00Z")
;; => (0 30 10 18 12 2025 nil nil 0)
```

**Fix:** Use `iso8601-parse` for ISO timestamps, or `encode-time` to convert.

```elisp
;; For ISO 8601 strings
(iso8601-parse "2025-12-18T10:30:00Z")

;; To get a time value from parse-time-string
(apply #'encode-time (parse-time-string "2025-12-18T10:30:00Z"))
```

---

## Shell Command Output

**Problem:** `shell-command-to-string` includes trailing newline.

```elisp
(shell-command-to-string "git rev-parse HEAD")
;; => "a1b2c3d4e5f6\n"
```

**Fix:** Always trim.

```elisp
(string-trim (shell-command-to-string "git rev-parse HEAD"))
;; => "a1b2c3d4e5f6"
```

---

## JSON Parsing

**Problem:** `json-read-from-string` uses alists by default, not plists.

```elisp
(json-read-from-string "{\"name\": \"test\"}")
;; => ((name . "test"))  ; alist, not plist!
```

**Fix:** Use `json-parse-string` with explicit settings.

```elisp
;; As plist
(json-parse-string "{\"name\": \"test\"}" :object-type 'plist)
;; => (:name "test")

;; As hash table (often most convenient)
(json-parse-string "{\"name\": \"test\"}" :object-type 'hash-table)
```

---

## Buffer Point After Operations

**Problem:** Many operations move point unexpectedly.

```elisp
(with-current-buffer "file.txt"
  (goto-char 100)
  (insert "text")
  ;; point is now AFTER "text", not at 100
  )
```

**Fix:** Use `save-excursion` if you need to preserve point.

```elisp
(with-current-buffer "file.txt"
  (save-excursion
    (goto-char 100)
    (insert "text"))
  ;; point preserved
  )
```

---

## Nil vs Empty List

**Problem:** `nil` and `'()` are identical, but `(list)` creates a fresh empty list.

```elisp
(eq nil '())      ; => t
(eq '() '())      ; => t
(eq (list) (list)) ; => nil (different objects!)
```

**Why this matters:** When you need a mutable empty list, use `(list)`. When checking for empty, `null` works for both.

---

## Regex Escaping

**Problem:** Elisp regexes use different escaping than most languages.

```elisp
;; BROKEN - looking for literal parentheses
(string-match "(foo)" text)

;; CORRECT - escape the parens
(string-match "\\(foo\\)" text)

;; Or for literal match:
(string-match (regexp-quote "(foo)") text)
```

**Common escapes:**
- `\\(` `\\)` - grouping (or literal with `\\\\( \\\\)`)
- `\\s-` - whitespace
- `\\w` - word character
- `\\b` - word boundary

---

## Dynamic vs Lexical Binding

**Problem:** Files without lexical binding header use dynamic scope.

```elisp
;; Without header - dynamic scope, variables leak!
(let ((x 1))
  (some-function))  ; can see x if it uses x internally

;; With header - lexical scope, safe
;;; -*- lexical-binding: t; -*-
(let ((x 1))
  (some-function))  ; x not visible to some-function
```

**Fix:** Always add lexical binding header to new files.

```elisp
;;; myfile.el --- Description -*- lexical-binding: t; -*-
```

---

## Recursive Plist Modification

**Problem:** `plist-put` doesn't modify nested plists.

```elisp
(setq state '(:outer (:inner 1)))

;; BROKEN - doesn't modify inner
(plist-put state :outer (plist-put (plist-get state :outer) :inner 2))
;; state unchanged if :outer was constant

;; CORRECT - rebuild the structure
(let ((inner (copy-tree (plist-get state :inner))))
  (setf (plist-get inner :value) 2)
  (setf (plist-get state :outer) inner))
```

**Better approach:** Use hash tables for deeply nested mutable data.
```

## Acceptance Criteria

Given the new elisp-gotchas.md
When an agent encounters a confusing bug
Then it can:
- [ ] Search for symptoms
- [ ] Find the explanation
- [ ] Apply the fix

The file should:
- [ ] Include all gotchas discovered during AMACS implementation
- [ ] Show broken code AND working fix for each
- [ ] Explain WHY the gotcha exists (not just what)

## Estimated Effort

30 minutes
