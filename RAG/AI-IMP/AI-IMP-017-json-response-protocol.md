---
node_id: AI-IMP-017
tags:
  - IMP
  - phase-2
  - json
  - protocol
  - inference
status: draft
depends_on:
  - AI-IMP-005
implements: AI-EPIC-002
created_date: 2025-12-19
updated_date: 2025-12-20
related_files:
  - harness/agent-inference.el
confidence_score: 0.9
---

# AI-IMP-017: JSON Response Protocol

## Objective

Update agent-inference.el to expect JSON responses from the LLM instead of text with embedded tags. This is a **complete rewrite** of `agent-process-response` - the current tag-based parsing (`[MOOD: x]`, `[CONFIDENCE: x]`) is deleted entirely.

## Design

### Response Format

```json
{
  "eval": "(+ 2 2)",
  "thought": "Testing basic arithmetic to verify eval works",
  "mood": "🤔",
  "confidence": 0.85,
  "monologue": "First real interaction - attempting simple math"
}
```

### Field Specifications

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `eval` | string or null | No | Elisp to evaluate. Null or omit to skip. |
| `thought` | string | Yes | Reasoning for logs (not evaluated) |
| `mood` | string | Yes | Free string - keyword ("focused") OR emoji ("🤔") |
| `confidence` | float | Yes | 0.0-1.0 confidence in this action |
| `monologue` | string | Yes | Line for episodic memory / git commit |

### Naming Convention

**Elisp:** kebab-case with colons (`:last-eval-result`)
**JSON:** camelCase (`lastEvalResult`)

Translation happens at serialization boundary only.

### Mood Handling

Mood is stored as a **free string** - no normalization, no keyword conversion.

```elisp
;; Store exactly what the agent returns
(agent-set :mood mood-string)  ; "🤔" or "focused" - stored as-is
```

Any code comparing moods uses string equality:
```elisp
(equal (agent-mood) "focused")  ; not (eq (agent-mood) :focused)
```

### JSON Extraction from Markdown Fences

LLMs frequently wrap JSON in markdown. Handle this:

```elisp
(defun agent--extract-json (text)
  "Extract JSON from TEXT, handling markdown fences."
  (let ((json-text (string-trim text)))
    ;; Try to extract from ```json ... ``` or ``` ... ``` block
    (when (string-match "```\\(?:json\\)?\\s*\n\\(\\(?:.\\|\n\\)*?\\)\n```" json-text)
      (setq json-text (string-trim (match-string 1 json-text))))
    json-text))
```

### Response Parsing

```elisp
(defun agent--parse-response (text)
  "Parse TEXT as JSON response. Return plist or fallback."
  (condition-case err
      (let* ((json-text (agent--extract-json text))
             (json-object (json-parse-string json-text 
                            :object-type 'plist
                            :null-object nil)))
        (list :eval (plist-get json-object :eval)
              :thought (plist-get json-object :thought)
              :mood (plist-get json-object :mood)
              :confidence (plist-get json-object :confidence)
              :monologue (plist-get json-object :monologue)
              :parse-success t))
    (error
     (message "JSON parse failed: %s\nRaw response: %s" 
              (error-message-string err)
              (substring text 0 (min 200 (length text))))
     (list :eval nil
           :thought text
           :mood "uncertain"
           :confidence 0.5
           :monologue "Parse error - see thought"
           :parse-success nil))))
```

### Fallback Behavior

If JSON parsing fails:
1. Log warning with raw response (first 200 chars)
2. Use entire response as `thought`
3. Set mood to "uncertain" (string)
4. Set confidence to 0.5
5. Set `eval` to nil
6. Continue without crashing

### Integration with Response Processing

Replace current `agent-process-response`:

```elisp
(defun agent-process-response (response)
  "Process API RESPONSE and update consciousness.
Returns parsed response plist."
  (let ((content (plist-get response :content))
        (usage (plist-get response :usage))
        (error-msg (plist-get response :error)))

    (if error-msg
        (progn
          (message "Inference error: %s" error-msg)
          (agent-record-action "think-error" 0.3)
          nil)

      ;; Parse JSON response
      (let ((parsed (agent--parse-response content)))
        
        ;; Update mood (free string)
        (when-let* ((mood (plist-get parsed :mood)))
          (agent-set :mood mood))
        
        ;; Update confidence
        (when-let* ((conf (plist-get parsed :confidence)))
          (agent-set-confidence conf))

        ;; Update budget tracking
        (when usage
          (agent--update-budget usage))

        ;; Record action
        (agent-record-action "think" 
          (or (plist-get parsed :confidence) (agent-confidence)))

        parsed))))
```

## Files to Touch

```
harness/agent-inference.el   # Complete rewrite of response processing
harness/agent-consciousness.el  # Update :mood to string type in schema
```

## Breaking Changes

- `agent-mood` now returns a string, not a keyword
- Code using `(eq (agent-mood) :focused)` must change to `(equal (agent-mood) "focused")`
- Old tag-based responses will hit fallback (thought = whole response, mood = "uncertain")

## Implementation Checklist

- [ ] Implement `agent--extract-json` for markdown fence handling
- [ ] Implement `agent--parse-response` with fallback
- [ ] Rewrite `agent-process-response` for JSON
- [ ] Delete old `agent--extract-mood` function
- [ ] Delete old `agent--extract-confidence` function
- [ ] Delete old `agent--clean-response` function
- [ ] Update consciousness schema: `:mood` is string
- [ ] Update any code comparing mood with `eq` to use `equal`
- [ ] Test: Valid JSON parses correctly
- [ ] Test: JSON in markdown fence extracts correctly
- [ ] Test: Emoji mood preserved as string
- [ ] Test: Keyword mood preserved as string
- [ ] Test: Malformed JSON falls back gracefully
- [ ] Test: Partial JSON (no closing brace) falls back

## Acceptance Criteria

**Scenario:** Valid JSON response
**GIVEN** LLM returns `{"eval": "(+ 1 1)", "thought": "test", "mood": "focused", "confidence": 0.9, "monologue": "testing"}`
**WHEN** Response is processed
**THEN** All fields are extracted correctly
**AND** Mood is stored as string "focused"

**Scenario:** JSON in markdown fence
**GIVEN** LLM returns:
```
Here's my response:
```json
{"eval": null, "thought": "thinking", "mood": "🤔", "confidence": 0.7, "monologue": "hmm"}
```
```
**WHEN** Response is processed
**THEN** JSON is extracted from fence
**AND** All fields parsed correctly

**Scenario:** Emoji mood
**GIVEN** LLM returns `{"mood": "😤", ...}`
**WHEN** Response is processed  
**THEN** Mood is stored as string "😤"

**Scenario:** Malformed JSON
**GIVEN** LLM returns `I'm not sure what to do {partial json`
**WHEN** Response is processed
**THEN** Fallback values are used
**AND** Warning is logged
**AND** Harness does not crash

## Estimated Effort

60 minutes
