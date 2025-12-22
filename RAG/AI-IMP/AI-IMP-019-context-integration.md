---
node_id: AI-IMP-019
tags:
  - IMP
  - phase-2
  - context
  - eval
status: done
depends_on:
  - AI-IMP-018
implements: AI-EPIC-002
created_date: 2025-12-19
updated_date: 2025-12-21
close_date: 2025-12-21
related_files:
  - harness/agent-inference.el
  - harness/agent-context.el
confidence_score: 0.9
---

# AI-IMP-019: Context Integration

## Objective

Include the last eval result in the agent's context so it can see what happened when its elisp was executed.

## Naming Convention

**Elisp:** `:last-eval-result`
**JSON:** `lastEvalResult`

Serialization functions handle the translation.

## Design

### Context Addition

Add a new section to the user prompt showing last eval:

```markdown
## Last Eval Result
```json
{
  "elisp": "(+ 2 2)",
  "success": true,
  "result": "4",
  "error": null,
  "tick": 42
}
```
```

### Serialization Helper

```elisp
(defun agent--kebab-to-camel (string)
  "Convert kebab-case STRING to camelCase."
  (let ((parts (split-string string "-")))
    (concat (car parts)
            (mapconcat #'capitalize (cdr parts) ""))))

(defun agent--plist-to-json-alist (plist)
  "Convert PLIST to alist with camelCase keys for JSON encoding."
  (let ((result '()))
    (while plist
      (let* ((key (pop plist))
             (val (pop plist))
             (key-name (if (keywordp key)
                          (substring (symbol-name key) 1)  ; remove :
                        (symbol-name key)))
             (json-key (agent--kebab-to-camel key-name)))
        (push (cons json-key val) result)))
    (nreverse result)))
```

### Formatting Function

```elisp
(defun agent--format-last-eval-for-prompt ()
  "Format :last-eval-result for inclusion in user prompt."
  (when-let* ((last-eval (agent-get :last-eval-result)))
    (unless (plist-get last-eval :skipped)
      (let* ((tick (plist-get last-eval :tick))
             (json-alist (agent--plist-to-json-alist last-eval)))
        (format "## Last Eval Result (tick %d)\n```json\n%s\n```"
                tick
                (json-encode json-alist))))))
```

### Example Output

```json
{
  "elisp": "(+ 2 2)",
  "success": true,
  "result": "4",
  "error": null,
  "tick": 42
}
```

### Integration in build-user-prompt

```elisp
(defun agent-build-user-prompt ()
  "Build the user prompt from current context."
  (let* ((ctx (agent-build-context))
         (sections '()))
    
    ;; Last eval result (most immediate context)
    (when-let* ((eval-section (agent--format-last-eval-for-prompt)))
      (push eval-section sections))
    
    ;; Thread-bound skills (via IMP-023)
    (when-let* ((skills-section (agent--load-thread-skills)))
      (push skills-section sections))
    
    ;; Active thread buffers
    ;; ... rest of existing sections ...
    
    (mapconcat #'identity (nreverse sections) "\n\n")))
```

### Prompt Section Ordering

1. **Last Eval Result** - most immediate context (what just happened)
2. **Thread Skills** - domain knowledge for current work
3. **Active Thread Buffers** - current file contents
4. **Global Buffers** - chat pending, etc.
5. **Pending Threads** - other concerns
6. **Recent Monologue** - memory context
7. **Recent Actions** - action history

### Error Presentation

When eval failed, the JSON clearly shows it:

```json
{
  "elisp": "(undefined-function)",
  "success": false,
  "result": null,
  "error": "Symbol's function definition is void: undefined-function",
  "tick": 43
}
```

Agent can then:
1. Recognize the error
2. Understand what went wrong
3. Try a different approach

### First Tick Handling

On first tick, there's no last eval. The section is simply omitted - cleaner than showing "No previous eval."

## Files to Touch

```
harness/agent-inference.el   # Add formatting function
harness/agent-context.el     # Integrate in context building
```

## Implementation Checklist

- [x] Implement `agent--kebab-to-camel`
- [x] Implement `agent--plist-to-json-alist`
- [x] Implement `agent--format-last-eval-for-prompt`
- [x] Integrate into `agent-build-user-prompt`
- [x] Position as first section (most relevant)
- [x] Handle nil/skipped eval gracefully (omit section)
- [x] Include tick number for temporal context
- [x] Test: Eval result appears in prompt
- [x] Test: Keys are camelCase in JSON
- [x] Test: Error result clearly shows failure
- [x] Test: First tick (no eval) works without error
- [x] Test: Skipped eval doesn't show section

## Acceptance Criteria

**Scenario:** Successful eval in context
**GIVEN** Previous tick evaluated `(+ 2 2)` successfully
**WHEN** Building prompt for next tick
**THEN** Prompt includes "Last Eval Result" section
**AND** JSON shows `"success": true, "result": "4"`
**AND** Keys are camelCase (`elisp`, not `elisp`)

**Scenario:** Failed eval in context
**GIVEN** Previous tick evaluated `(bad-function)` and failed
**WHEN** Building prompt for next tick
**THEN** Prompt includes "Last Eval Result" section
**AND** JSON shows `"success": false, "error": "..."`

**Scenario:** No previous eval
**GIVEN** This is the first tick (cold start)
**WHEN** Building prompt
**THEN** "Last Eval Result" section is omitted
**AND** Prompt is otherwise complete

**Scenario:** Skipped eval
**GIVEN** Previous tick returned `"eval": null`
**WHEN** Building prompt for next tick
**THEN** "Last Eval Result" section is omitted

## Estimated Effort

30 minutes
