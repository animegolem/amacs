---
node_id: AI-EPIC-001b
tags: 
  - EPIC
  - AI
  - phase-1.5
  - api
  - inference
  - derisk
date_created: 2025-12-06
date_completed: 
kanban-status: planned
AI_IMP_spawned:
  - AI-IMP-011 (planned)
---

# AI-EPIC-001b-first-breath

## Problem Statement/Feature Scope 

We've built consciousness persistence, thread-centric context, monologue, and skills - but never actually called an LLM. All our tests use manual ticks with placeholder content. Before investing in Proxmox infrastructure (EPIC-002), we need to validate the core loop works end-to-end with real inference.

This is a de-risking checkpoint: can the agent actually think?

## Proposed Solution(s) 

Minimal viable inference integration:

1. **API client**: Simple OpenAI-compatible client (works with OpenRouter)
2. **Prompt assembly**: Convert `agent-build-context` output to API messages
3. **Response handling**: Parse response, update consciousness, append monologue
4. **Thinking tick**: New `agent-think` command that does perception → inference → action

No streaming, no tool use, no multi-provider routing. Just: assemble context → call API → handle response → commit.

OpenRouter provides budget-limited tokens and OpenAI-compatible endpoints, so we get safety rails without complexity.

## Path(s) Not Taken 

- Multi-provider support (Phase 2+)
- Streaming responses
- Tool/function calling
- Structured output parsing
- Retry logic / error recovery
- Cost optimization (prompt caching, model routing)

## Success Metrics 

1. **Single thinking tick completes**: `M-x agent-think` calls API and returns response
2. **Context flows correctly**: LLM response demonstrates awareness of thread concern
3. **State updates**: Consciousness and monologue reflect the inference
4. **Git captures thought**: Commit message includes LLM-generated content
5. **Budget respected**: Stays under $0.50 for initial testing

Timeline: 1 session

## Requirements

### Functional Requirements

- [ ] FR-1: System shall read API key from environment variable or config file
- [ ] FR-2: System shall assemble prompt from thread context using `agent-build-context`
- [ ] FR-3: System shall make HTTP POST to OpenAI-compatible endpoint
- [ ] FR-4: System shall parse JSON response and extract assistant message
- [ ] FR-5: System shall update `:last-inference-time` and increment tick
- [ ] FR-6: System shall append LLM response summary to monologue
- [ ] FR-7: System shall commit with LLM-influenced message
- [ ] FR-8: System shall handle API errors gracefully (log, don't crash)
- [ ] FR-9: System shall estimate token usage and update `:budget`

### Non-Functional Requirements 

- API timeout: 60 seconds max
- No external elisp dependencies (use built-in `url.el`)
- API key never logged or committed
- Works in both interactive and batch mode

## Implementation Breakdown 

| IMP | Title | Status | Notes |
|-----|-------|--------|-------|
| AI-IMP-011 | API client and first inference | planned | Core integration |
