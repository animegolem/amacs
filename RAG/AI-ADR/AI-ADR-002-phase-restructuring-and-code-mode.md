---
node_id: AI-ADR-002
tags:
  - ADR
  - architecture
  - phases
  - code-mode
  - motor-control
status: proposed
depends_on:
  - AI-ADR-001
created_date: 2025-12-18
related_files:
  - RAG/RFC/Part 3: Implementation Phases.org
  - RAG/AI-EPIC/AI-EPIC-001-vampire-simulator-core.md
  - RAG/AI-EPIC/AI-EPIC-001b-first-breath.md
  - skills/amacs-bootstrap-skill/core/SKILL.md
confidence_score: 0.9
---

# AI-ADR-002-phase-restructuring-and-code-mode

## Objective

With Phase 1 (Vampire Simulator) and Phase 1b (First Breath) complete, we need to define Phase 2. The original RFC bundled motor control with infrastructure (Proxmox, vsock, Gitea CI) into "Phase 2: Bicameral Mind."

Observations from Phase 1b revealed a gap: the agent can perceive (buffer serialization) and think (API inference) but cannot act. It repeatedly planned to "evaluate (+ 2 2)" but had no mechanism to do so.

Two questions emerged:
1. How should the agent's motor control work?
2. Should infrastructure deployment block motor control development?

The answer to #2 is clearly "no" - we can iterate on motor control locally before adding VM complexity.

The answer to #1 required deeper analysis of how LLMs interact with tools vs code.

## Decision

### Phase Restructuring

The implementation phases are restructured as follows:

| Phase | Name              | Focus                    | Status    |
|-------|-------------------|--------------------------|-----------|
| 1     | Vampire Simulator | Cognitive loop           | ✅ Done   |
| 1b    | First Breath      | LLM integration          | ✅ Done   |
| 2     | Hands and Arms    | Motor control (eval)     | 📋 Current|
| 3     | Bicameral Split   | Security/infrastructure  | 📋 Next   |
| 4     | Neural Memory     | Long-term retrieval      | 📋 Future |
| 5     | Ghost in Shell    | Desktop embodiment       | 📋 Future |

Key change: Infrastructure (Proxmox, vsock, Gitea) moves from Phase 2 to Phase 3. Phase 2 focuses purely on giving the agent motor control on a single machine.

### Code Mode Over Tool Calling

The core architectural decision for Phase 2:

**LLMs are better at writing code than using tool-calling interfaces.**

Evidence:
- Tool calling requires synthetic training data created by model developers
- Code generation uses massive real-world training corpus
- Cloudflare's "Code Mode" research showed agents handle more tools, more complex tools, and multi-step operations better when writing code than when using tool schemas

Reference: https://blog.cloudflare.com/code-mode/

Cloudflare's framing: "Making an LLM perform tasks with tool calling is like putting Shakespeare through a month-long class in Mandarin and then asking him to write a play in it."

**Decision:** The agent returns raw elisp, not structured action requests.

### Protocol Design

The Body-Brain protocol becomes minimal:

```
Body → Brain (JSON):
  - consciousness (mood, confidence, threads)
  - buffer contents (serialized text)  
  - last eval result (success/error + output)

Brain → Body (JSON):
  - eval: elisp string to evaluate
  - thought: reasoning for logging
  - mood: updated mood keyword
  - confidence: updated confidence float
  - monologue: line for episodic memory
```

No action vocabulary. No parameter schemas. No tool definitions.

The agent sees text (buffer state) and writes code (elisp). This is maximally aligned with how Emacs actually works - everything is a buffer, everything is eval.

### Skills as Documentation

Skills shift from "available actions" to "documentation the agent reads":

**Before (tool-calling paradigm):**
```xml
<available_actions>
  <action name="eshell-command">Execute a shell command</action>
  <action name="buffer-switch">Switch to a buffer</action>
</available_actions>
```

**After (code-mode paradigm):**
```markdown
# Useful Elisp Patterns

## Shell Interaction
(with-current-buffer "*eshell*"
  (goto-char (point-max))
  (insert "ls -la")
  (eshell-send-input))

## Buffer Operations
(switch-to-buffer "*scratch*")
(buffer-string)
(insert "hello")
```

The agent reads skills like a developer reads documentation, then writes whatever elisp accomplishes its goal. Skills teach patterns, they don't constrain actions.

### Bootstrap Skill Updates Required

The current bootstrap skill (SKILL.md and references) is misaligned with this decision:

1. **Remove:** Architecture diagrams showing Brain/Body/Gitea topology (implementation detail)
2. **Remove:** `cortex-dispatch` command vocabulary in tick-system.md
3. **Add:** `elisp-patterns.md` - practical patterns for common operations
4. **Add:** `elisp-gotchas.md` - collected bugs and workarounds
5. **Reframe:** Skills as documentation, not action constraints

### Infrastructure Deferral

Phase 3 (Bicameral Split) now contains all infrastructure work:
- Proxmox hypervisor setup
- LXC containers for Brain/Memory (with GPU bind mount - no passthrough needed)
- Body VM/LXC with airgap
- VSock communication protocol
- Gitea for commits and CI
- CI pipeline (byte-compile + test validation)

Key insight: LXC containers share host kernel, so GPU access is trivial via bind mount. GPU passthrough is only needed when Body requires a display (Phase 5/EXWM).

### Phase 4: Neural Episodic Memory

Phase 4 is updated to use the Hippocampus architecture (bi-encoder retrieval) instead of LoRA fine-tuning:

- **What changes:** What the agent remembers (retrieval)
- **What stays frozen:** How the agent thinks (base model weights)

This avoids catastrophic forgetting, hallucinated memories, and privacy/unlearning problems inherent to weight modification.

Training signal: verified success triplets (context → successful commit), not self-reported monologue.

## Consequences

### Enabled

- **Rapid iteration:** Motor control can be developed and tested on Mac without Proxmox setup
- **Simpler protocol:** No action schema maintenance, no tool registration
- **Natural fit:** Emacs is already "everything is eval" - this aligns perfectly
- **Emergent capabilities:** Agent can do anything Emacs can do, not just predefined actions
- **Skill creation:** Agent can define elisp functions that persist, creating its own vocabulary

### Requires

- **Bootstrap skill rewrite:** Current skill assumes tool-calling paradigm
- **Trust model:** Full eval access means agent can do anything in the Emacs process
- **Logging discipline:** Every eval must be logged for debugging and git history
- **Error handling:** Agent-written elisp may fail; harness must capture and report errors gracefully

### Deferred

- VM isolation (Phase 3)
- VSock protocol details (Phase 3)  
- GPU passthrough complexity (Phase 5, if ever)
- Deployment automation (Phase 3+)
