---
node_id: AI-IMP-002
tags:
  - IMP-LIST
  - Implementation
  - phase-1
  - monologue
  - memory
kanban_status: completed
depends_on: 
  - AI-IMP-001
confidence_score: 0.95
created_date: 2025-11-27
close_date: 2025-12-04
--- 

# AI-IMP-002-monologue-system

## Episodic Memory via Append-Only Log

Implement the monologue system: append-only org file that captures stream of consciousness, rolling window in consciousness variable, and integration with git commits.

**Done when:** Each tick appends to `~/.agent/monologue.org`, recent entries appear in `:recent-monologue`, and git commits use monologue line as message.

See: [[amacs-rfc-v3.md]] Part 7 (Memory Architecture)

### Out of Scope 

- Monologue retrieval/search (agent does this via grep/rg)
- Dream consolidation
- Semantic memory layer
- Anything beyond append + rolling window

### Design/Approach  

Monologue is the agent's stream of consciousness. Two storage locations:

1. **Permanent**: `~/.agent/monologue.org` - append-only, timestamped, grepable
2. **Working**: `:recent-monologue` in consciousness - last 50-100 lines, most recent first

Format in org file:
```org
[2025-11-27 14:32][TICK 42] Investigating the buffer switching issue
[2025-11-27 14:33][TICK 43] Found it - wrong hook order
```

The monologue line feeds the git commit message. Tick cycle becomes:
1. (existing tick updates)
2. Append monologue line to file
3. Update rolling window in consciousness
4. Commit with monologue as message

For this IMP, monologue content is placeholder text since no LLM yet. Agent will just log "Tick N completed" or similar.

### Files to Touch

```
~/.emacs.d/amacs/agent-monologue.el  # New file - monologue management
~/.emacs.d/amacs/agent-tick.el       # Modify - integrate monologue into commit
~/.agent/monologue.org               # Created on first append
```

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**? 
</CRITICAL_RULE> 

- [x] Implement `agent-monologue.el`:
  - [x] Define `agent-monologue-file` variable (`~/.agent/monologue.org`)
  - [x] Define `agent-monologue-window-size` (default 100)
  - [x] Implement `agent-append-monologue` (append to file + update rolling window)
  - [x] Implement `agent-recent-monologue` (return last N entries)
  - [x] Implement `agent-format-monologue-line` (timestamp + tick + content)
  - [x] Provide `agent-monologue` feature
- [x] Modify `agent-tick.el`:
  - [x] Call `agent-append-monologue` with placeholder content
  - [x] Use monologue line in commit message format
  - [x] Update commit format to `[TICK N][thread][:mood] monologue-line`
- [x] Modify `agent-consciousness.el`:
  - [x] Ensure `:recent-monologue` is in initial consciousness schema
  - [x] Ensure rolling window is persisted/restored correctly
- [x] Test: tick appends line to monologue.org
- [x] Test: `:recent-monologue` contains last N entries in correct order
- [x] Test: git commit message matches monologue line
- [x] Test: rolling window doesn't exceed configured size
- [x] Test: monologue survives restart (file persists, window reloads)
 
### Acceptance Criteria

**Scenario:** First monologue entry ✓
**GIVEN** Agent is initialized, `monologue.org` does not exist
**WHEN** User calls `agent-tick`
**THEN** `~/.agent/monologue.org` is created
**AND** Contains one timestamped entry with tick number
**AND** `:recent-monologue` has one entry

**Scenario:** Rolling window management ✓
**GIVEN** Agent has run 150 ticks, window size is 100
**WHEN** User inspects `:recent-monologue`
**THEN** List contains exactly 100 entries
**AND** Most recent entry is first in list
**AND** `monologue.org` file contains all 150 entries

**Scenario:** Commit message integration ✓
**GIVEN** Agent is at tick 42, mood is `:focused`, active thread is "debugging"
**WHEN** User calls `agent-tick` 
**THEN** Git commit message is `[TICK 43][debugging][:focused] <monologue line>`

**Scenario:** Monologue persistence across restart ✓
**GIVEN** Agent ran 50 ticks, Emacs killed
**WHEN** User restarts and calls `agent-init`
**THEN** `:recent-monologue` is restored from consciousness.el
**AND** `monologue.org` still contains all 50 entries

### Issues Encountered 

None significant. Implementation was straightforward given IMP-001 foundation.

### Artifacts Produced

| File | Purpose |
|------|--------|
| `harness/agent-monologue.el` | Append-only log + rolling window (~100 lines) |
| `harness/agent-tick.el` | Modified - added monologue hook |
| `harness/agent-core.el` | Modified - added require + updated agent-info |
| `harness/test-harness.el` | Added 3 monologue tests |
