---
node_id: AI-IMP-055
tags:
  - IMP-LIST
  - Implementation
  - EPIC-007
  - shell
  - autonomous
kanban_status: done
depends_on:
  - AI-IMP-054
confidence_score: 0.95
created_date: 2025-01-19
close_date: 2025-01-19
---

# AI-IMP-055-silent-tick-support

## Summary

Enable agent to process ticks without displaying output when no reply is provided. Currently the shell forces a synthetic `[No reply. Mood: X]` message, violating FR-2 (reply optional) from EPIC-007.

**Current state**: Shell always displays something, even when agent provides `reply: null`.

**Target state**:
- If `reply` is nil/absent: process state updates silently, no shell output
- If `reply` is present: display as normal
- Human prompt remains "pending" (unpaired in org file) until agent provides actual reply

**Done when**: Agent can return `{"mood": "focused", "confidence": 0.8, "monologue": "Working..."}` (no reply field) and shell processes silently without displaying placeholder text.

### Out of Scope

- Autonomous tick scheduling (IMP-056)
- Chat history integration (IMP-057)
- Visual indicator in hub for "agent working silently"

### Design/Approach

The fix is in `amacs-shell.el` response handling. Current code at lines 570-572:

```elisp
(let ((reply (or (plist-get parsed :reply)
                 (format "[No reply. Mood: %s]" (plist-get parsed :mood))))
```

Change to conditional:
1. Extract reply from parsed response
2. If reply is non-nil and non-empty, display it
3. If reply is nil/empty, skip display but still process state updates
4. Don't call `agent-chat-append-exchange` when no reply (keeps prompt pending)

### Files to Touch

`harness/amacs-shell.el`: Modify response handling around line 570
`harness/test-harness.el`: Add test for silent tick

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**?
</CRITICAL_RULE>

- [x] Read `amacs-shell.el` response handling section (~line 560-600)
- [x] Modify reply extraction to not use fallback synthetic message
- [x] Wrap display logic in conditional: only display if reply present
- [x] Modify `agent-chat-append-exchange` call: only append when actual reply provided
- [x] State updates (mood, confidence, scratchpad, eval) still processed regardless
- [x] Add test: response without reply processes silently
- [x] Add test: response with reply displays normally
- [x] Run CI: `./harness/ci-check.sh` - **123/123 tests pass**

### Acceptance Criteria

**Scenario:** Agent responds without reply field
**GIVEN** agent returns `{"mood": "focused", "confidence": 0.8, "monologue": "Working"}`
**WHEN** shell processes the response
**THEN** no text is displayed in the shell buffer
**AND** mood and confidence are updated in consciousness
**AND** monologue is written
**AND** human prompt remains pending in chat org file

**Scenario:** Agent responds with reply field
**GIVEN** agent returns `{"reply": "Hello!", "mood": "happy", "confidence": 0.9, "monologue": "Greeting"}`
**WHEN** shell processes the response
**THEN** "Hello!" is displayed in the shell buffer
**AND** exchange is appended to chat org file (prompt paired with response)

### Issues Encountered

**Both main and retry paths needed updates**: The retry path in `amacs-shell--do-inference-retry` had the same fallback pattern. Updated both paths for consistency.

**Added helper function**: Created `amacs-shell--clear-thinking-indicator` to clear the `[Thinking...]` line for silent ticks without inserting any response.

**Monologue append forward declaration**: Added `(declare-function agent-monologue-append "agent-monologue")` to avoid byte-compile warning for lazy-loaded function.
