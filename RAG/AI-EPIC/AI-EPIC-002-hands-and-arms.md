---
node_id: AI-EPIC-002
tags:
  - EPIC
  - phase-2
  - motor-control
  - eval
  - code-mode
status: done
depends_on:
  - AI-EPIC-001c
  - AI-ADR-002
created_date: 2025-12-19
updated_date: 2025-12-21
close_date: 2025-12-21
related_files:
  - harness/agent-inference.el
  - harness/agent-tick.el
  - skills/amacs-bootstrap-skill/core/SKILL.md
  - skills/amacs-bootstrap-skill/core/references/tick-system.md
confidence_score: 0.9
---

# AI-EPIC-002: Hands and Arms

## Narrative

The agent can perceive (buffer serialization) and think (API inference), but cannot act. During Phase 1b testing, it repeatedly planned to "evaluate (+ 2 2)" but had no mechanism to do so. This EPIC gives it motor control through raw elisp evaluation.

Per ADR-002, we use "code-mode" - the agent returns elisp for evaluation rather than structured tool calls. This aligns with how LLMs are trained (massive real code corpus) vs tool-calling (synthetic training data).

**The loop becomes:**
```
perceive → think → return elisp → harness evals → agent sees result → repeat
```

## Acceptance Criteria

- [x] CI pipeline catches elisp errors before they reach agent (IMP-005)
- [x] Agent returns JSON with eval/thought/mood/confidence/monologue fields (IMP-017)
- [x] Harness parses JSON (including markdown-fenced), executes eval field, captures result/error (IMP-017, IMP-018)
- [x] Next tick includes lastEvalResult in context (IMP-019)
- [x] Human can communicate via org-mode chat buffer (IMP-022)
- [x] Agent can read chat and respond with structured output (IMP-022)
- [x] Core skill loads as system prompt (cached) (IMP-020)
- [x] Agent can bind skills to threads, skills load when thread is active (IMP-023)
- [x] End-to-end: agent successfully evaluates (+ 2 2) and sees result "4" (IMP-021)
- [x] Mood stored as free string (keyword or emoji) (IMP-017)

## Scope

### In Scope
- CI validation (byte-compile + tests)
- JSON response parsing (with markdown fence extraction)
- Eval execution with result capture
- Context integration of eval results
- Core skill as system prompt (cached)
- Skill binding to threads
- Org-mode chat interface with minor mode
- Chat skill (helper functions, pattern documentation)

### Out of Scope
- VM separation / vsock (Phase 3)
- Budget tracking changes (already implemented)
- Structured logging framework (optional enhancement)
- Intrusive thoughts / critic model (Phase 3)
- Autonomous tick interaction with chat (debounce, pause on typing)
- Agent-initiated ping (email notification to human)

## Key Design Decisions

### Naming Convention
**Elisp:** kebab-case with colons (`:last-eval-result`)
**JSON:** camelCase (`lastEvalResult`)

Translation happens at serialization boundary only. One function handles the conversion.

### Mood as Free String
Mood is stored exactly as the agent returns it - no normalization. "🤔" stays "🤔", "focused" stays "focused". Comparison uses `equal`, not `eq`.

Rationale: Emojis are information-dense mood tokens. 😤 encodes frustration+determination+intensity in one token.

### Core Skill = System Prompt
The core SKILL.md IS the system prompt. Benefits from API prompt caching (rarely changes). Single source of truth for agent orientation. Thread-bound skills load separately in user prompt.

### Skill Binding to Threads
Skills travel with threads. When a thread is active, its bound skills load into context. When you switch threads, different skills load. This enables domain-specific knowledge without constant context bloat.

### Chat as Collaboration
The chat buffer is for coordinated human-agent communication but does NOT restrict the agent to "assistant" role. The agent has full agency over its environment. Chat is one channel among many.

## Implementation Strategy

### IMP-005: CI Pipeline (Prerequisite)
Already spec'd. Byte-compile validation + test suite in batch mode.

**Deliverable:** `ci-check.sh` that returns non-zero on any failure.

### IMP-017: JSON Response Protocol
Complete rewrite of response processing. Delete tag-based parsing.

**Key changes:**
- Parse JSON with markdown fence extraction
- Mood stored as free string
- Fallback on parse failure (thought = raw text, mood = "uncertain")

### IMP-018: Eval Execution
Implement `agent-eval` (renamed from `agent-safe-eval` - we trust the eval).

**Key changes:**
- Parse elisp with `read`, eval with `condition-case`
- Store in `:last-eval-result`
- Log to monologue

### IMP-019: Context Integration
Include eval result in next tick's context.

**Key changes:**
- Use camelCase in JSON output
- Position as first section in user prompt
- Omit section if no eval or skipped

### IMP-020: System Prompt as Core Skill
Core SKILL.md becomes the system prompt (cached).

**Content:**
- Identity and orientation
- JSON response format
- Consciousness schema (brief)
- Thread model
- Skill binding
- Chat explanation
- Core principles

### IMP-023: Skill Binding System
Enable binding skills to threads.

**Functions:**
- `agent-bind-skill-to-thread`
- `agent-unbind-skill-from-thread`
- `agent-list-available-skills`

**Context integration:**
- Thread has `:bound-skills` field
- Active thread's skills load in user prompt

### IMP-022: Chat Interface
Minimal human-agent chat using org-mode.

**Design:**
- Org buffer with `* Human Input` / `* Agent Response` structure
- `** Think` collapsed by default, `** Output` visible
- `C-c C-c` queues attention + triggers think (Phase 2)
- Helper functions: `agent-chat-read-pairs`, `agent-chat-append-response`

### IMP-021: Integration Test
End-to-end test proving the loop works.

**Rounds:**
1. Agent thinks, returns eval
2. Agent sees result, adapts
3. Human chats, agent responds

## Dependencies

```
AI-EPIC-001c (bootstrap skill remediation) - COMPLETE
    ↓
AI-IMP-005 (CI pipeline) - first, catch regressions
    ↓
AI-IMP-017 (JSON protocol) + AI-IMP-020 (System prompt)
    ↓
AI-IMP-018 (Eval execution)
    ↓
AI-IMP-019 (Context integration) + AI-IMP-023 (Skill binding)
    ↓
AI-IMP-022 (Chat interface)
    ↓
AI-IMP-021 (Integration test)
```

## Effort Estimates

| IMP | Task | Estimate |
|-----|------|----------|
| 005 | CI Pipeline | 45 min |
| 017 | JSON Response Protocol | 60 min |
| 018 | Eval Execution | 45 min |
| 019 | Context Integration | 30 min |
| 020 | System Prompt as Core Skill | 45 min |
| 023 | Skill Binding System | 60 min |
| 022 | Chat Interface | 60 min |
| 021 | Integration Test | 30 min |

**Total:** ~6.25 hours

## Trust Model

Full eval access granted. Rationale:
- Phase 2 runs on local dev machine with human observing
- Logging captures all evals for review (git commits)
- Claude models have genuine safety behaviors, not just performance
- Airgap architecture (Phase 3) provides defense in depth later

## Success Metrics

1. Agent successfully executes elisp and sees results
2. Error handling works (bad elisp doesn't crash harness)
3. Agent adapts behavior based on eval results
4. Agent can bind skills and see them in context
5. Human can communicate via chat buffer
6. CI catches any regressions during development
7. Git history shows eval → result → response cycle

## Open Questions (Resolved)

1. ~~Output capture~~ → Defer. Just capture return value for MVP.
2. ~~Eval timeout~~ → No timeout for Phase 2. Add if needed.
3. ~~Multi-statement eval~~ → Allow `progn`, agent figures it out.
4. ~~Mood format~~ → Free string. Emoji or keyword.
5. ~~Naming convention~~ → Kebab in elisp, camelCase in JSON.
6. ~~System prompt size~~ → Core skill IS system prompt. ~1500 tokens.
