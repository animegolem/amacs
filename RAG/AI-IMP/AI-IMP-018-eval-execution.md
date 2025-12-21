---
node_id: AI-IMP-018
tags:
  - IMP
  - phase-2
  - eval
  - motor-control
status: draft
depends_on:
  - AI-IMP-017
implements: AI-EPIC-002
created_date: 2025-12-19
updated_date: 2025-12-20
related_files:
  - harness/agent-inference.el
  - harness/agent-consciousness.el
confidence_score: 0.9
---

# AI-IMP-018: Eval Execution

## Objective

Implement eval capability - taking the elisp string from the agent's response and executing it, capturing result or error.

## Design

### Core Function

The function is named `agent-eval` (not `agent-safe-eval` - we trust the eval, we just capture results).

```elisp
(defun agent-eval (elisp-string)
  "Evaluate ELISP-STRING, capturing result or error.
Returns plist with :success, :result, :error."
  (if (or (null elisp-string) 
          (and (stringp elisp-string) (string-empty-p (string-trim elisp-string))))
      (list :success t :result nil :error nil :skipped t)
    (condition-case err
        (let* ((form (read elisp-string))
               (result (eval form t)))  ; t = lexical binding
          (list :success t
                :result (prin1-to-string result)
                :error nil
                :skipped nil))
      (error
       (list :success nil
             :result nil
             :error (error-message-string err)
             :skipped nil)))))
```

### Naming Convention

**Elisp:** `:last-eval-result`
**JSON:** `lastEvalResult`

### Consciousness Update

Store eval result in consciousness:

```elisp
(defun agent-record-eval (elisp-string eval-result)
  "Record EVAL-RESULT for ELISP-STRING in consciousness."
  (agent-set :last-eval-result
             (list :elisp elisp-string
                   :success (plist-get eval-result :success)
                   :result (plist-get eval-result :result)
                   :error (plist-get eval-result :error)
                   :tick (agent-current-tick))))
```

### Monologue Logging

Log evals to monologue for git history:

```elisp
(defun agent--format-eval-for-monologue (elisp-string eval-result)
  "Format eval result for monologue entry."
  (if (plist-get eval-result :skipped)
      nil  ; Don't log skipped evals
    (if (plist-get eval-result :success)
        (format "EVAL: %s => %s"
                (truncate-string-to-width elisp-string 50 nil nil "...")
                (truncate-string-to-width 
                 (or (plist-get eval-result :result) "nil") 30 nil nil "..."))
      (format "EVAL ERROR: %s => %s"
              (truncate-string-to-width elisp-string 50 nil nil "...")
              (plist-get eval-result :error)))))
```

### Integration Point

In `agent-think` after response processing:

```elisp
;; After extracting fields from JSON response
(let ((parsed (agent-process-response response)))
  (when parsed
    ;; Execute eval if present
    (when-let* ((elisp (plist-get parsed :eval)))
      (let ((eval-result (agent-eval elisp)))
        (agent-record-eval elisp eval-result)
        (when-let* ((eval-log (agent--format-eval-for-monologue elisp eval-result)))
          (agent-append-monologue eval-log))))
    
    ;; Append thought monologue
    (when-let* ((monologue (plist-get parsed :monologue)))
      (agent-append-monologue monologue))
    
    ;; Return thought for display
    (plist-get parsed :thought)))
```

## Files to Touch

```
harness/agent-inference.el     # Add eval execution to think flow
harness/agent-consciousness.el # Add :last-eval-result field
```

## Implementation Checklist

- [ ] Implement `agent-eval` with error handling
- [ ] Implement `agent-record-eval` for consciousness update
- [ ] Implement `agent--format-eval-for-monologue`
- [ ] Integrate into `agent-think` flow
- [ ] Handle null/empty eval field (skip gracefully)
- [ ] Use lexical binding for eval (`t` second arg)
- [ ] Test: Simple expression `(+ 2 2)` returns `"4"`
- [ ] Test: Error expression `(/ 1 0)` captures error message
- [ ] Test: Multi-statement `(progn ...)` works
- [ ] Test: Null eval field skips without error
- [ ] Test: Empty string eval skips without error
- [ ] Test: Monologue contains eval log

## Acceptance Criteria

**Scenario:** Successful eval
**GIVEN** Agent returns `{"eval": "(+ 2 2)", ...}`
**WHEN** Eval is executed
**THEN** Result is `"4"`
**AND** `:last-eval-result` in consciousness shows success
**AND** Monologue contains `"EVAL: (+ 2 2) => 4"`

**Scenario:** Failed eval
**GIVEN** Agent returns `{"eval": "(undefined-function)", ...}`
**WHEN** Eval is executed
**THEN** Error is captured (not thrown)
**AND** `:last-eval-result` shows `:success nil` and `:error` message
**AND** Monologue contains `"EVAL ERROR: ..."`
**AND** Harness continues running

**Scenario:** Null eval
**GIVEN** Agent returns `{"eval": null, ...}`
**WHEN** Response is processed
**THEN** No eval is attempted
**AND** `:last-eval-result` shows `:skipped t`

**Scenario:** Progn eval
**GIVEN** Agent returns `{"eval": "(progn (setq x 1) (+ x 2))", ...}`
**WHEN** Eval is executed
**THEN** Result is `"3"`

**Scenario:** Define function
**GIVEN** Agent returns `{"eval": "(defun my-helper () 42)", ...}`
**WHEN** Eval is executed
**THEN** Function is defined
**AND** Future evals can call `(my-helper)`

## Estimated Effort

45 minutes
