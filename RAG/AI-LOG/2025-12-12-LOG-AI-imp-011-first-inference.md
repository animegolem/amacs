---
node_id: LOG-2025-12-12-001
tags:
  - AI-log
  - development-summary
  - phase-1.5
  - api-integration
  - first-inference
closed_tickets:
  - AI-IMP-011
created_date: 2025-12-12
related_files:
  - harness/agent-api.el
  - harness/agent-inference.el
  - harness/agent-context.el
  - harness/agent-core.el
  - CLAUDE.md
  - RAG/AI-IMP/AI-IMP-011-first-inference.md
confidence_score: 0.95
---

# 2025-12-12-LOG-AI-imp-011-first-inference

## Work Completed

Implemented first live LLM inference for AMACS, completing IMP-011 (First Breath de-risk). The agent can now think.

**New Files Created:**
- `agent-api.el` (~200 lines): OpenAI-compatible HTTP client using built-in `url.el`. Supports OpenRouter endpoint, handles auth via env var (`OPENROUTER_API_KEY`) or config file, parses JSON responses, tracks token usage and cost estimation.
- `agent-inference.el` (~220 lines): Prompt assembly and inference orchestration. Builds system/user prompts from consciousness and thread context, provides `M-x agent-think` command, processes responses to extract mood/confidence updates.

**Modified Files:**
- `agent-core.el`: Added requires for new modules, config loading on init, load-path setup for standalone execution.
- `agent-context.el`: Added `agent--sanitize-string` to strip non-ASCII characters for HTTP safety.
- `CLAUDE.md`: Created project documentation with structure overview, testing instructions, and elisp gotchas.

**Successful Test:** Agent completed 15 ticks with 3 successful inferences. Agent demonstrated self-reflection, noticed its own inference error, adjusted confidence appropriately, and expressed intent to take action in the scratch buffer.

## Session Commits

| Commit | Description |
|--------|-------------|
| 619bf94 | api integration smoke test |

Note: Most changes are uncommitted pending this session close. Files modified: agent-api.el, agent-inference.el, agent-context.el, agent-core.el, CLAUDE.md, AI-IMP-011.

## Issues Encountered

**1. url.el Multibyte Text Error (MAJOR)**
Emacs `url.el` refused to send requests containing non-ASCII characters. The scratch buffer contained fancy curly quotes (`'C-x C-f'`) which caused "Multibyte text in HTTP request" errors. Required two fixes:
- Encode request body: `(encode-coding-string body 'utf-8 t)` with `t` to force unibyte
- Sanitize buffer content at hydration time: strip non-ASCII with `[^[:ascii:]]` → `?`

This is a temporary workaround. Future work should implement proper UTF-8 JSON escaping.

**2. Load Path for Standalone Execution**
`emacs -Q -l agent-core.el` failed to find sibling modules. Fixed by adding load-path setup at top of agent-core.el.

**3. Emacs 31 Deprecations**
`when-let` and `if-let` are deprecated in Emacs 31. Must use `when-let*` and `if-let*` instead. Added to CLAUDE.md gotchas.

**4. Config File Destroyed by Tests**
Test harness clears `~/.agent/` directory, destroying config files. Changed API key loading to prefer `OPENROUTER_API_KEY` environment variable over config file.

## Tests Added

No automated tests added this session. IMP-011 was validated through manual testing:
- `M-x agent-api-test` - verified API connectivity
- `M-x agent-think` - verified full inference loop
- Verified consciousness persistence, monologue updates, and git commits

Automated tests for API/inference would require mocking HTTP responses. Consider for IMP-005 (CI pipeline).

## Next Steps

**Decision Point:** Two paths forward:
1. **Proxmox infrastructure (EPIC-002)** - Set up brain/body VM separation before adding more capabilities
2. **Tool calling / action execution** - Let agent actually eval elisp and take actions

Initial inference results are positive enough that Proxmox work seems safe to pursue. The agent thinks coherently and maintains state. Infrastructure before capabilities.

**Before Continuing:**
- Read `harness/agent-api.el` and `harness/agent-inference.el` for API integration
- Read `CLAUDE.md` for project overview and gotchas
- The agent can perceive buffers but cannot yet act - that's intentional for Phase 1

**Pending Work:**
- IMP-005 (CI pipeline) - byte-compile + test validation
- Proper UTF-8 handling instead of ASCII stripping
- Consider: should the test harness preserve config.el?

**Agent Observation:** The agent expressed desire to evaluate `(+ 2 2)` in scratch but lacks capability. It has intent without agency. Poetic and informative - the architecture is working.
