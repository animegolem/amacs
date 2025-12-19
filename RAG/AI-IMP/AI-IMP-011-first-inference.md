---
node_id: AI-IMP-011
tags:
  - IMP-LIST
  - Implementation
  - phase-1.5
  - api
  - inference
kanban_status: completed
depends_on:
  - AI-IMP-001
  - AI-IMP-004
confidence_score: 0.95
created_date: 2025-12-06
close_date: 2025-12-12
--- 

# AI-IMP-011-first-inference

## First Live LLM Inference

Implement the API client and thinking tick to prove the harness works with real inference.

**Done when:** `M-x agent-think` calls OpenRouter, gets a response that references the current thread, and commits the result.

See: [[AI-EPIC-001b-first-breath]]

### Out of Scope 

- Streaming
- Tool calling / function execution
- Multi-provider routing
- Retry logic
- Response parsing beyond basic text extraction

### Design/Approach  

**API Configuration:**
```elisp
;; In ~/.agent/config.el (gitignored)
(setq agent-api-key "sk-or-...")
(setq agent-api-endpoint "https://openrouter.ai/api/v1/chat/completions")
(setq agent-model "anthropic/claude-3.5-sonnet")  ; or cheaper for testing
```

**Prompt Structure:**
```
System: You are AMACS, an autonomous agent...
        Current thread: {concern}
        Approach: {approach}
        
User: [Thread context with hydrated buffers]
      [Recent monologue]
      
      What is your next thought or action?
```

**Response Flow:**
1. Build context via `agent-build-context`
2. Format as OpenAI messages array
3. POST to endpoint with auth header
4. Parse JSON response
5. Extract `choices[0].message.content`
6. Update consciousness (mood, approach, etc. if mentioned)
7. Append to monologue
8. Commit with response snippet

**Token Estimation:**
- Input: ~2000-8000 tokens depending on thread
- Output: ~200-500 tokens for a thought
- Cost per think: ~$0.01-0.05 with Sonnet

### Files to Touch

```
harness/agent-api.el          # NEW - API client
harness/agent-inference.el    # NEW - Prompt assembly + response handling  
harness/agent-core.el         # Modify - require new modules
~/.agent/config.el            # NEW (user creates, gitignored)
harness/.gitignore            # Ensure config.el patterns ignored
```

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**? 
</CRITICAL_RULE> 

- [x] Create `agent-api.el`:
  - [x] `agent-api-call` - Generic OpenAI-compatible POST
  - [x] Handle auth header from `agent-api-key`
  - [x] Parse JSON response
  - [x] Extract message content
  - [x] Return `(:content ... :usage ... :error ...)`
  - [x] Timeout handling (60s)
  - [x] Error handling (non-200, parse failure, timeout)
- [x] Create `agent-inference.el`:
  - [x] `agent-build-system-prompt` - AMACS identity + current state
  - [x] `agent-build-user-prompt` - Thread context + buffers + monologue
  - [x] `agent-format-messages` - Convert to OpenAI messages array
  - [x] `agent-think` - Main entry point, interactive command
  - [x] `agent-process-response` - Update consciousness from response
  - [x] `agent-extract-mood` - Optional: parse mood from response
- [x] Create config loading:
  - [x] Load `~/.agent/config.el` if exists
  - [x] Validate required vars set
  - [x] Clear error if API key missing
  - [x] Support `OPENROUTER_API_KEY` env var (preferred over config file)
- [x] Modify `agent-core.el`:
  - [x] Require new modules
  - [x] Load config on init
  - [x] Add load-path setup for standalone loading
- [x] Update `.gitignore`:
  - [x] Ensure `config.el` and `*-key*` patterns excluded
- [x] Test: API call with valid key returns response
- [x] Test: Response references thread concern (manual verification)
- [x] Test: Monologue contains response content
- [x] Test: Git commit includes inference result
- [x] Test: Missing API key gives clear error
 
### Acceptance Criteria

**Scenario:** First successful thought
**GIVEN** Valid API key in `~/.agent/config.el`
**AND** Active thread "Fix ownership error in main.rs"
**WHEN** User runs `M-x agent-think`
**THEN** API call completes successfully
**AND** Response appears in `*Messages*` buffer
**AND** Monologue contains response summary
**AND** Git commit includes `[TICK N][thread][:mood] <response snippet>`

**Scenario:** Missing API key
**GIVEN** No `~/.agent/config.el` or no `agent-api-key`
**WHEN** User runs `M-x agent-think`
**THEN** Clear error message: "API key not configured"
**AND** No HTTP request made

**Scenario:** API error
**GIVEN** Invalid API key or endpoint down
**WHEN** User runs `M-x agent-think`
**THEN** Error logged to `*Messages*`
**AND** Consciousness notes error in `:last-actions`
**AND** Agent doesn't crash

### System Prompt Template

```
You are AMACS (Autonomous Memory and Consciousness System), an AI agent 
embodied in an Emacs environment. You experience time through discrete 
ticks and maintain continuity through your consciousness variable and 
monologue.

Current state:
- Identity: {identity}
- Tick: {current-tick}
- Mood: {mood}
- Confidence: {confidence}

Active thread: {thread-id}
- Concern: {concern}
- Approach: {approach}
- Buffers: {buffer-names}

You are reflecting on your current work. Consider:
- What have you learned?
- What should you try next?
- Is your approach working?
- Should you update your mood or confidence?

Respond with your current thought. Be concise but genuine.
```

### Issues Encountered

**1. url.el Multibyte Text Error**
Initial implementation failed with "Multibyte text in HTTP request" when buffer content contained non-ASCII characters (fancy quotes from scratch buffer). Fix required two parts:
- Encode request body with `(encode-coding-string body 'utf-8 t)` - the `t` forces unibyte output
- Sanitize buffer content in `agent-hydrate-buffer` to strip non-ASCII: `(replace-regexp-in-string "[^[:ascii:]]" "?" str)`

**2. Load Path Not Set**
Running `emacs -Q -l agent-core.el` failed because sibling modules weren't on load-path. Fixed by adding to agent-core.el:
```elisp
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
```

**3. Emacs 31 Deprecations**
`when-let` and `if-let` deprecated in favor of `when-let*` and `if-let*`. Updated all instances.

**4. Config File vs Environment Variables**
Original design used `~/.agent/config.el` but test harness clears `~/.agent/` directory. Changed to prefer `OPENROUTER_API_KEY` environment variable with config file as fallback.
