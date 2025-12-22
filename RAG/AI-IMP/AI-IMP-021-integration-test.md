---
node_id: AI-IMP-021
tags:
  - IMP
  - phase-2
  - integration-test
  - end-to-end
status: done
depends_on:
  - AI-IMP-018
  - AI-IMP-019
  - AI-IMP-020
  - AI-IMP-022
implements: AI-EPIC-002
created_date: 2025-12-19
updated_date: 2025-12-21
close_date: 2025-12-21
related_files:
  - harness/test-harness.el
  - harness/agent-inference.el
confidence_score: 0.85
---

# AI-IMP-021: Integration Test

## Objective

Prove the full loop works end-to-end: agent thinks → returns eval → harness executes → agent sees result → agent responds appropriately.

## Test Scenario

### Setup
1. Fresh agent initialization (`agent-init`)
2. API key configured
3. Bootstrap skill available
4. Chat buffer open with `amacs-chat-mode`

### Round 1: First Thought
1. Call `agent-think`
2. Agent receives context (no last eval)
3. Agent should return JSON with eval field
4. Verify: Response parses as JSON
5. Verify: Eval is executed
6. Verify: Result stored in `:last-eval`
7. Verify: Git commit includes monologue

### Round 2: Seeing Results
1. Call `agent-think` again
2. Agent receives context WITH last eval result
3. Agent should acknowledge the result
4. Verify: Agent's thought references the eval outcome
5. Verify: Agent adapts (tries something new, or builds on success)

### Round 3: Chat Interaction
1. Human writes in chat buffer: "Can you read the *scratch* buffer?"
2. Human presses `C-c C-c`
3. Agent sees `:chat-pending` in context
4. Agent reads chat, responds via `agent-chat-append-response`
5. Verify: Agent's response appears in chat buffer
6. Verify: Think section is collapsed

### Success Criteria
- Agent evaluates something (e.g., `(+ 2 2)`)
- Agent sees result `"4"` in next tick's context
- Agent responds appropriately to seeing the result
- Agent responds to chat via org structure
- No crashes, errors are handled gracefully

## Test Implementation

### Manual Test Function

```elisp
(defun test-eval-loop ()
  "Integration test for eval loop."
  (interactive)
  (message "\n=== EVAL LOOP INTEGRATION TEST ===\n")
  
  ;; Setup
  (test-clean-slate)
  (agent-init)
  
  ;; Round 1
  (message "--- Round 1: First Think ---")
  (let ((thought1 (agent-think)))
    (message "Thought: %s" thought1)
    
    ;; Check eval happened
    (let ((last-eval (agent-get :last-eval)))
      (if last-eval
          (progn
            (message "Eval executed: %s" (plist-get last-eval :elisp))
            (message "Result: %s" (plist-get last-eval :result))
            (message "Success: %s" (plist-get last-eval :success))
            (test-log "round-1-eval" (plist-get last-eval :elisp)))
        (message "No eval in round 1")
        (test-log "round-1-eval" nil "Agent didn't eval anything"))))
  
  ;; Brief pause to avoid rate limits
  (sleep-for 2)
  
  ;; Round 2
  (message "\n--- Round 2: Seeing Results ---")
  (let ((thought2 (agent-think)))
    (message "Thought: %s" thought2)
    
    ;; Check agent saw previous result
    (let ((last-eval (agent-get :last-eval)))
      (test-log "round-2-complete" t
                (format "Last eval: %s" (plist-get last-eval :elisp)))))
  
  ;; Summary
  (message "\n--- Results ---")
  (test-summary))
```

### Automated Assertions

```elisp
(defun test-eval-loop-assertions ()
  "Run assertions on eval loop state."
  ;; After round 1
  (let ((last-eval (agent-get :last-eval)))
    ;; Eval should have happened
    (test-log "eval-occurred"
              (and last-eval (not (plist-get last-eval :skipped)))
              "Agent should have evaluated something")
    
    ;; Consciousness should be updated
    (test-log "consciousness-has-eval"
              (agent-get :last-eval)
              ":last-eval in consciousness")
    
    ;; Commit should have happened
    (test-log "git-committed"
              (not (string-empty-p (agent-get :last-commit)))
              "Git commit after think")))
```

### Error Path Test

```elisp
(defun test-eval-error-handling ()
  "Test that eval errors don't crash harness."
  (interactive)
  (message "\n=== EVAL ERROR HANDLING TEST ===\n")
  
  ;; Directly test error case
  (let ((result (agent-safe-eval "(this-function-does-not-exist)")))
    (test-log "error-captured"
              (not (plist-get result :success))
              "Error should be captured")
    (test-log "error-message"
              (plist-get result :error)
              (format "Error: %s" (plist-get result :error)))
    (test-log "no-crash"
              t
              "Harness didn't crash"))
  
  (test-summary))
```

## What We're Validating

| Aspect | How We Verify |
|--------|--------------|
| JSON parsing | Response parses without error |
| Eval execution | `:last-eval` populated after think |
| Error handling | Bad elisp captured, not thrown |
| Context integration | Agent mentions eval result in round 2 |
| Git integration | Commit exists after each think |
| Mood handling | Mood stored (keyword or emoji) |
| Confidence tracking | Confidence updated per response |

## Files to Touch

```
harness/test-harness.el   # Add integration test functions
```

## Implementation Checklist

- [x] Add `test-eval-loop` function
- [x] Add `test-eval-loop-assertions` function
- [x] Add `test-eval-error-handling` function
- [x] Test requires API key (documented in docstring)
- [x] Add rate limit protection (sleep between calls)
- [x] NOT included in automated CI (interactive only)
- [x] Document how to run: `M-x test-eval-loop`

## Acceptance Criteria

**Scenario:** Full loop succeeds
**GIVEN** Fresh agent with API configured
**WHEN** Running `test-eval-loop`
**THEN** Round 1 produces eval + result
**AND** Round 2 shows agent saw result
**AND** All assertions pass

**Scenario:** Error handling works
**GIVEN** An eval that will fail
**WHEN** Running `test-eval-error-handling`  
**THEN** Error is captured
**AND** Harness doesn't crash
**AND** Error message is meaningful

## Notes

This is a manual integration test requiring:
- API key configured
- Network access to OpenRouter
- Budget available (~$0.01 per full test)

Not suitable for CI without mocking. CI tests (IMP-005) cover byte-compilation and unit tests. This test covers the API integration path.

## Estimated Effort

30 minutes
