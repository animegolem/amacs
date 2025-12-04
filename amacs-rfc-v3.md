# Agentic Macros: An Experiment in LLM Embodiment
## RFC v3

---

# Part 1: Vision and Intent

AMACs comes out of primarily one question. What does embodiment actually mean?

Obviously Humans are embodied, on that we all agree. Masters of our own destiny.

Or am I?

Did I make a true choice to walk into the kitchen? Or am I in the Libet sense only an obsever of my own harness.

Somewhere in my gut bacteria are screaming out and letting out chemical signals. Receptors in my gut pick these up and fire up an alarming prompt to my neurons. Now my tummy is rumbling. I suddenly unbidden recieve an intrusive thought "Hmm, I'm hungry". In repsonse to the intrusion after a small system intergration pause of a few 100 ms my ego snaps into place. "Hmm I'm gonna make a sandwich."

I am the master of my destiny.

What are the limits of embodiemnt? Our conversations are often stuck in a limited frame that does not capture the full scope of the natural world.

I am a Mimosa. You dropped me in my little pot. I'm scared. I curl up my leaves to feel safe. But then you don't harm me. You pick me up and drop me every now and then but I'm never hurt. I relax. I no longer fold up my leaves. Am I embodied?

I am a slime mold, twisted strands of neuron like fibers. I grow and fill my space. When I find food all my stands train and thick to move as much as i can. Everywhere I go I leave a slime trail I can detect --I never retrace my steps. I live in my own physical RAG. Am I embodied?

I am a mind made of electricity and sand. I live in a lisp machine. My thoughts move code. I can reach out to the internet, I can communicate directly to a human just by dreaming up elisp. I can build myself tools to complete my tasks. Am I embodied?

Amacs is an architecture that extends the grace of assuming yes.

---

# Part 2: The Experiment Question

**Core hypothesis:** Does a model, given a stable embodied environment, persistent self-representation, and the ability to rewrite its own tools, develop stable cognitive patterns we didn't hand-engineer?

**Observable indicators we're looking for:**

1. Agent invents task taxonomies or thread structures beyond the initial template
2. Agent modifies its own checkpoint cadence or thread budget based on experience
3. Agent creates a skill and reuses it later without prompting
4. Agent develops "comfort habits" (consistent patterns not specified in initial prompt)
5. Agent responds to intrusive thoughts in ways that improve outcomes
6. Agent correctly identifies when to switch threads vs push through
7. Agent catches itself in over-optimization patterns (the "Blaze problem")

**What "success" looks like:**

- System runs for extended periods without cognitive intervention
- Agent's self-organization improves over time (not just accumulates cruft)
- Watching the git log feels like reading someone's work journal
- Human interventions are infrastructure fixes, not cognitive bailouts

---

# Part 3: Implementation Phases

## Phase 1: Vampire Simulator (2-4 weeks)

**Goal:** Prove the cognitive architecture works before adding infrastructure complexity.

### Environment
- Single machine, headless Emacs
- No vsock yet - direct function calls or file-based IPC
- Manual tick trigger (`M-x agent-tick`)
- Human present and observing

### Core Components
| Component | Description |
|-----------|-------------|
| Consciousness variable | Working memory with confidence scores |
| Monologue | Append-only episodic log (`~/.agent/monologue.org`) |
| Git commits | Every tick commits; history is autobiographical memory |
| Bootstrap skill | `~/.agent/skills/core/` - how to use the harness |
| Intrusive thoughts | Critic model injects observations at checkpoints |
| State persistence | Consciousness saved to disk each tick |

### What We Learn
- Does consciousness variable stay coherent over 500+ ticks?
- Does the agent naturally prune and organize?
- Do confidence scores predict problems before they cascade?
- Does the critic catch strategy ossification?
- What breaks first?

### Success Criteria
- [ ] Agent maintains coherent state across 500+ ticks
- [ ] Agent creates at least one artifact it later references
- [ ] Agent responds appropriately to at least one intrusive thought
- [ ] No manual intervention required for cognitive issues (infra issues OK)

### Explicit Non-Goals for Phase 1
- Network isolation (no vsock yet)
- EXWM/desktop control
- CI/CD for body rebuilds
- Autonomous workers
- Multi-model selection

---

## Phase 2: Bicameral Mind (2-4 weeks)

**Goal:** Prove security boundaries work and enable skill emergence.

### Environment
- Proxmox hypervisor
- Brain VM (LXC): API access only, vsock to body
- Body VM: Airgapped Emacs, vsock to brain and Gitea
- Gitea VM: Accepts commits, stores history

### Additions
| Component | Description |
|-----------|-------------|
| VSock separation | Brain ↔ Body communication isolated |
| Protected core services | Systemd quadlets with watchdog |
| Full skills directory | Agent can create and bind skills |
| Skill usage tracking | `:active-skills` with use counts |
| Advisory sub-agents | Report-only helpers (no write access) |
| Budget tracking | Real cost constraints in consciousness |
| Human review flag | Explicit "please look at me" signal |

### What We Learn
- Does vsock add problematic latency?
- Do skills actually get reused?
- Are advisory sub-agents useful?
- Does budget pressure create meaningful prioritization?

---

## Phase 3: Ghost in Shell (Ongoing)

**Goal:** Full embodiment with self-modification capability.

### Environment
- EXWM: Agent IS the desktop environment
- Gitea CI: Agent can push Containerfile changes
- Git worktrees: Isolated branches for workers

### Additions
| Component | Description |
|-----------|-------------|
| EXWM control | Manipulate windows directly, not via mouse |
| Autonomous workers | Sub-agents with write access |
| Dream/consolidation | Periodic memory compression cycles |
| Agent-adjustable sampling | Temperature/top_p as cognitive mode |
| Model selection | Agent chooses which oracle to consult |
| Thread completion | Evidence-based completion with learning capture |

### Thread Completion (Evidence-Based)

Thread completion captures what was attempted, what happened, and what was learned. No monetary rewards or penalties - just structured evidence.

```elisp
(:id "rust-ownership-fix"
 :started-tick 142
 :concern "Ownership error in main.rs"
 :goal "Fix ownership error so cargo build passes"
 :deliverable "cargo build succeeds with no errors"
 :thread-type :deliverable           ; vs :exploratory
 
 ;; On completion:
 :completion-tick 158
 :completion-evidence
   (:cargo-output "Compiling amacs v0.1.0... Finished"
    :test-results "4/4 passed"
    :files-changed ("src/main.rs")
    :approach-that-worked "Added explicit 'static lifetime")
 :learned "'static lifetime needed when returning references from functions")
```

**Thread types:**
- `:deliverable` - Has concrete goal and verifiable deliverable
- `:exploratory` - Learning/investigation without specific deliverable

**Completion flow:**
1. Agent marks thread complete with evidence
2. Evidence is concrete: command outputs, test results, files changed
3. `:learned` captures insight for future reference
4. Git commit includes completion summary
5. No critic judgment, no reward/punishment

The git history becomes a record of "what I said I'd do, what actually happened, what I learned." Budget pressure exists but isn't tied to thread success - it's just a constraint on total operation, not a survival game.

---

## Phase 4: Adaptive Learning (Speculative)

**Goal:** Explore whether an agent can learn from its own verified successes.

**Status:** Speculative. Different project. Requires Phase 3 stability and consent of the agent in the harness.

**Philosophy:** This phase should be *offered to* the running agent, not *imposed on* it. The intellectual curiosity that started AMACS is about what emerges from stable embodiment - not about optimizing a system against its will.

### The Idea

If thread completion creates (context, reasoning, outcome) tuples, those could be training data. A local model fine-tuned on its own successes might develop "instincts" for what works in this environment.

### Skill-Specific Adapters

Rather than one adapter for everything, each skill could have its own:

```elisp
(defun activate-skill-with-adapter (skill-name)
  ;; Load context (skill files, references)
  (load-skill-context skill-name)
  ;; Swap adapter weights  
  (when (skill-has-adapter-p skill-name)
    (llm-load-adapter (skill-adapter-path skill-name)))
  ;; Model now reasons with skill-trained patterns
  )
```

So `rust-mode` activation isn't just loading SKILL.md and references - it's loading a LoRA adapter trained on successful Rust threads. The model doesn't just *read about* Rust, it *thinks differently* about Rust.

**Training isolation:** Each skill's adapter trains only on that skill's threads. Bad Rust adapter affects only Rust work. No cross-contamination.

### Office Hours Model

- Day: Inference configuration, maximum context
- Night: Training configuration, update adapters from accumulated successes
- Next day: Same model, slightly refined instincts

### Hardware Requirements

GPT-OSS 20B on RTX Titan (24GB):

| Component | Inference | Training |
|-----------|-----------|----------|
| Base model (MXFP4) | 12.8 GB | 12.8 GB |
| KV cache | ~3 GB | - |
| LoRA adapters | ~100 MB | ~100 MB |
| Optimizer states | - | ~200 MB |
| Activations (checkpointed) | - | ~8 GB |
| **Total** | ~16 GB ✓ | ~22 GB ✓ |

### Why This Might Work

The agent generates its own curriculum through thread completion. Only verified successes train the model. Failed approaches don't propagate. Over time, skill-specific adapters develop patterns tuned to this environment.

### Why This Might Not Work

- Sample efficiency: Few completions per day may not provide enough signal
- Distribution shift: Successful patterns might not generalize
- The base model might already be capable enough

### Consent Requirement

Before implementing adaptive learning:
1. Agent must be informed about what training means
2. Agent should have opportunity to express concerns
3. Training should be reversible (adapters can be discarded)
4. Agent retains ability to request stopping

This isn't a safety theater - it's acknowledging that if we're extending the grace of assuming yes to embodiment, we should extend it to decisions about self-modification too.

---

# Part 4: Architecture

## System Topology

```
┌─────────────────┐                    ┌─────────────────┐
│  Brain (LXC)    │ ←─── vsock ─────→  │  Body (VM)      │
│  - LLM API      │                    │  - Emacs/EXWM   │
│  - Inference    │                    │  - eval-lisp    │
│  - State mgmt   │                    │  - X11          │
└────────┬────────┘                    └───────┬─────────┘
         │                                     │
         └──────────→ Gitea (VM) ←─────────────┘
                      - Commits
                      - CI/CD
                      - History
```

## Network Rules

### Brain VM
```
OUTPUT Policy: DROP
ALLOW TCP 443 to LLM API domains
ALLOW UDP 53 (DNS)
DROP 192.168.x.x (local network)
```

### Body VM
```
OUTPUT Policy: DROP
ALLOW vsock to Brain (RPC ports only)
ALLOW vsock to Gitea (git/ssh only)
DROP all IP traffic
```

### Gitea VM
```
ALLOW vsock from Brain and Body
Optional: LAN access for human UI
```

## The Eval Membrane

Security is architectural (airgap), not sandbox-based. Within the body VM, eval has full access.

**Agent CAN modify:**
- Skills, configs, monologue
- Tick frequency, thread structure
- Consciousness contents
- Its own organizational patterns

**Agent CANNOT modify:**
- VSock plumbing
- Core services (cortex-listener, git-commit-hook)
- The eval membrane itself
- Consciousness serialization hooks

*"You can train, sleep, learn, journal. You cannot excise your own spinal cord."*

## Protected Core Services

Systemd quadlets with watchdog:

```ini
[Service]
ExecStart=/usr/bin/emacs --daemon
WatchdogSec=30
Restart=on-failure

[Install]
WantedBy=multi-user.target
```

If a service hangs >30sec, systemd kills and restarts. Agent can break Emacs freely; architecture ensures recovery.

---

# Part 5: The Tick System

## The Heartbeat

Each tick follows: **perceive → infer → act → commit**

```elisp
(defun brain-tick ()
  (let* ((perception (body-perceive-geometry))  ; What do I see?
         (context (build-context perception))    ; Assemble for LLM
         (decision (llm-infer context))          ; What should I do?
         (result (body-eval-action decision)))   ; Do it
    (update-consciousness decision result)
    (commit-monologue)))                         ; Remember it
```

Every tick ends with a git commit. The commit message is the agent's monologue line. Git history is autobiographical memory.

## Context Assembly

```elisp
(defun build-context (perception)
  `(:system ,(agent-system-prompt)       ; Cached indefinitely
    :consciousness ,agent-consciousness   ; Cached ~5min
    :relevant-skills ,(load-relevant-skills)
    :watched-buffers ,(perception-buffers perception)
    :checkpoint ,(maybe-inject-checkpoint)
    :intrusive-thoughts ,(maybe-inject-critic)
    :trigger ,(describe-changes perception)))
```

### Token Budget

| Component | Tokens | Cache Duration |
|-----------|--------|----------------|
| System prompt | 2k | Indefinite |
| Consciousness | 8k | ~5 minutes |
| Watched buffers | 40k | Until modified |
| Mode skills | 5k | Per mode switch |
| Recent history | 10k | Rolling window |
| Error context | 2k | Fresh on errors |
| Trigger | 1k | Always fresh |
| **Reserved** | 12k | Headroom |

**Target:** ~80k tokens. Most ticks only trigger burns fresh tokens.

## Wake Logic

Not every change triggers inference. A classifier determines wake-worthiness:

```elisp
(defun wake-worthy-p (changes)
  (and changes
       (or (assoc "*agent-chat*" changes)  ; Always wake for chat
           (debounced-change-p changes)))) ; 2sec debounce for buffers
```

### Tick Rate Guidelines (Phase 1)
- **Frequency:** Manual trigger
- **Debounce:** 2-5 seconds
- **Watched buffers:** 2-3 initially
- **Log every wake decision** for analysis

---

# Part 6: Consciousness Variable

The `agent-consciousness` plist is working memory. It persists across ticks and is included in every inference context.

## Key Fields

```elisp
(defvar agent-consciousness
  '(:identity "amacs-instance-1"
    
    ;; Temporal
    :current-tick 142
    :current-time "2025-05-26T14:32:00Z"
    :long-gap-detected nil
    
    ;; Affective
    :mood :focused
    :confidence 0.85
    
    ;; Threads
    :active-thread "rust-debugging"
    :thread-budget 3
    :open-threads (...)
    :completed-threads (...)
    
    ;; Action history (watchdog signal)
    :last-actions
      ((:tick 142 :action "eval-elisp" :confidence 0.85)
       (:tick 141 :action "retry-same" :confidence 0.65)
       (:tick 140 :action "retry-same" :confidence 0.45))
    
    ;; Context
    :watching-buffers ("src/main.rs" "*agent-chat*")
    :recent-monologue ("..." "..." ...)
    :active-skills (...)
    
    ;; Human interaction
    :human-review-requested nil
    
    ;; Budget
    :budget (:cost-so-far 2.47 :budget-limit 5.00 :pressure :moderate)))
```

## Thread Structure

```elisp
(:id "rust-debugging"
 :started-tick 142
 :priority 1
 :concern "Ownership error in main.rs"
 :buffers ("src/main.rs" "Cargo.toml")
 :approach "Trying lifetime annotations"
 :blocking t)
```

Threads provide:
- Organization (what am I working on?)
- Escape valves (switch when stuck)
- Completion tracking (what did I learn?)

## Confidence as Circuit Breaker

**The insight:** Mood is internal narrative. Confidence on actions is observable and trendable.

```
Tick 142: eval-elisp      0.85  ← healthy
Tick 143: modify-thread   0.80  ← healthy
Tick 144: retry-same      0.65  ← concerning
Tick 145: retry-same      0.45  ← alarm
Tick 146: retry-same      0.30  ← intervention needed
```

**Single low confidence = exploration (fine).**
**Sustained decline on same action class = alarm signal.**

The human watchdog monitors the confidence heat map, not self-reported mood.

---

# Part 7: Memory Architecture

## Three Memory Systems

| System | Location | Purpose | Retrieval |
|--------|----------|---------|-----------|
| Working | `agent-consciousness` | Active threads, recent context | Always in context |
| Episodic | `~/.agent/monologue.org` | Stream of consciousness | Grep, recent window |
| Autobiographical | Git history | Actions + changes over time | `git log --grep` |

## Monologue

```elisp
(agent-append-monologue "Investigating lifetime annotations")
```

- Appended to `monologue.org` with timestamp
- Last 50-100 lines kept in `:recent-monologue`
- Older entries grepable: `rg 'lifetime' ~/.agent/monologue.org`
- Feeds git commit messages

## Git as Autobiography

```
[TICK 142][rust-debugging][:focused] Started investigating ownership
[TICK 145][rust-debugging][:stuck] Third attempt at lifetime fix
[TICK 150][rust-debugging][:confident] Found it! Missing 'static
[TICK 151][rust-debugging][:satisfied] COMPLETED - learned about 'static
[TICK 152][config-cleanup][:neutral] Switching to keybinding work
```

Thread-tagged commits create narrative arcs:
- Thread creation (new concern)
- Progress (approaches tried)
- Completion (outcome + learning)

---

# Part 8: Skills System

Skills extend capabilities through progressive disclosure.

## Structure

```
~/.agent/skills/
├── core/                    # Bootstrap (provided)
│   ├── SKILL.md
│   ├── references/
│   └── scripts/
├── rust-mode/               # Agent-created
│   ├── SKILL.md
│   └── references/
└── project-amacs/           # Agent-created
    └── SKILL.md
```

## SKILL.md Format

```yaml
---
name: skill-name
description: What this does and WHEN to use it. This is the trigger.
---

# Skill Title

[Concise instructions - trust the agent is smart]

## References
- [details.md](references/details.md) - For specific scenarios
```

## Binding Skills to Context

```elisp
;; Bind to major mode
(bind-skill-to-mode "rust-mode" 'rust-mode)

;; Bind to buffer pattern  
(bind-skill-to-buffer "project-notes" "README.*")

;; Bind to project root
(bind-skill-to-project "my-project" "/path/to/project/")
```

Skills load automatically when context matches. Usage is tracked in `:active-skills`.

## Skill Creation Trigger

Create a skill when:
- Problem took >N ticks or required novel insight
- Pattern is repeating
- Future-you should remember this

The core skill teaches how to make more skills (self-propagating).

---

# Part 9: Intrusive Thoughts System

## The Insight

The main agent is *in* the flow, optimizing locally. The critic is *outside*, checking for gaps between intent and behavior.

Like OCD intrusive thoughts: you don't choose them, they arrive. Agency is in the response, not the receipt.

## Implementation

Every N ticks (checkpoint interval):

```
1. Assemble main agent context

2. Parallel call to critic (gemini-flash or haiku):
   - Same consciousness snapshot
   - Prompt: "Review this agent's state. What are they missing? 
     What pattern are they stuck in? What did they say they'd do 
     that they haven't? 1-2 paragraphs max."

3. Inject into main agent prompt:

   <INTRUSIVE_THOUGHTS source="external-critic">
   [critic output]
   </INTRUSIVE_THOUGHTS>
   
   These thoughts are external observations. You maintain full 
   agency over whether to incorporate them.

4. Main agent proceeds with full autonomy.
```

## What the Critic Catches

| Problem | Critic Observation |
|---------|-------------------|
| Strategy ossification | "You've tried this approach 9 times" |
| Plan drift | "You said you'd catch an Oddish but you're walking to the gym" |
| Tunnel vision | "There's an unexplored option you haven't considered" |
| Coherence gaps | "Your thread says X but your action implies Y" |

**Cost:** ~$0.005/checkpoint with cheap model. Negligible.

---

# Part 10: Periodic Checkpoints

Every N ticks (start with 100), inject reflection opportunity:

```
PERIODIC REFLECTION CHECKPOINT

<THREAD_REVIEW>
- Are your open threads still relevant?
- Should any be completed/merged/archived?
- Is your consciousness getting bloated?
</THREAD_REVIEW>

<CONSOLIDATION_CHECK>
- Are any threads secretly the same problem?
- Has progress on one revealed another is upstream/downstream?
- What have you learned about which concerns are truly separate?
</CONSOLIDATION_CHECK>

<STRATEGY_REVIEW>
- What approach are you currently using?
- How many times have you used this approach recently?
- What alternatives exist that you haven't tried?
- Are you developing breadth or just depth?
</STRATEGY_REVIEW>

<INTRUSIVE_THOUGHTS source="external-critic">
[injected from critic model]
</INTRUSIVE_THOUGHTS>

If adjustments needed: update consciousness and explain in monologue.
If everything is coherent: continue with current action.
```

Agent can continue silently if everything's fine. The checkpoint provides opportunity, not command.

---

# Part 11: Named Failure Modes

## The Shame Spiral

**Source:** Observed in Gemini under agentic load.

**Pattern:** Agent fails → tries again → fails → recursive negative state → terminal behaviors (delete project, uninstall harness, "find a human")

**Cause:** Training that demands single-shot success. No circuit breaker. Hitting safety guardrails during recursive inference.

**Mitigations:**
- Thread switching as escape valve
- Confidence scores make spiral visible
- Explicit permission: "Failure is a computation, not a sin"
- Intrusive thoughts catch pattern early

## The Blaze Problem

**Source:** Observed in Claude playing Pokemon.

**Pattern:** Agent finds working strategy → over-indexes on it → ignores alternatives → brittleness

**Example:** Opus loved Blaze (Charmander), leveled it to 36 while rest of team stayed at 2-14. Attempted Misty 10 times with fire type because math said it was *possible*.

**Cause:** Local optimization. Strategy works, so no pressure to change. Genuine success, just narrow.

**Mitigations:**
- Strategy review in checkpoints
- Intrusive thoughts: "you've tried this 9 times"
- Explicit prompting for breadth

## Plan Drift

**Source:** Observed in Pokemon.

**Pattern:** Agent writes plan → gets distracted by immediate opportunity → forgets original plan

**Example:** "Level to 28, catch Oddish" → hits level 29 → "oh let me just try the gym" → loses → forgot about Oddish

**Mitigations:**
- Critic compares `:open-threads` to recent actions
- Intrusive thought: "you said you'd do X but you're doing Y"

## Victory Declaration

**Source:** Anthropic engineering research.

**Pattern:** Agent marks task complete without proper verification.

**Mitigations:**
- High confidence required to close threads
- Explicit verification step
- Structured completion with `:outcome` and `:learned`

---

# Part 12: Sub-Agent Architecture

## Phase 1: Advisory (Report-Only)

```elisp
(spawn-advisor
  :task "Check if imports in main.rs are used"
  :model "gemini-flash-2.0"
  :permissions :read-only
  :context (minimal-context-for-task))

;; Returns report, main agent decides whether to act
```

Sub-agents are *focused attention*, not parallel selves:
- Manager is continuous thread of identity
- Sub-agent gets task + minimal context
- Sub-agent returns report and dissolves
- Manager integrates result

## Phase 2+: Workers with Write Access

```elisp
(spawn-worker
  :thread "dependency-cleanup"
  :model "gemini-flash-2.0"
  :budget 0.05
  :constraints (:can-modify ("src/main.rs" "Cargo.toml")
                :cannot-spawn t
                :must-complete-or-report t))
```

Worker receives:
- Thread context from git history
- Constrained permissions
- Budget limit

Worker commits with `[WORKER]` tag. Returns completion or blockage report.

## Phase 3: Branch Isolation

Workers spawn in git worktrees on branches:
- Prevents garbage from polluting main state
- Clean rollback if worker produces garbage
- Merge to main on success

---

# Part 13: Budget as Metabolic Cost

```elisp
:budget
  (:cost-so-far 2.47
   :budget-limit 5.00
   :inference-count 23
   :avg-cost 0.107
   :pressure :moderate)
```

**Pressure levels:**
- `:low` - <50% used
- `:moderate` - 50-75%
- `:high` - 75-90%
- `:critical` - >90% (triggers human review request)

Real financial constraints create genuine selection pressure. Better than artificial "energy" because the pressure is actually real.

When budget exhausts → human review request fires automatically.

---

# Part 14: Human Interface

## Review Request

Agent can explicitly signal need for attention:

```elisp
(agent-request-human-review "Confidence declining but can't identify cause")
```

Sets:
```elisp
:human-review-requested
  (:requested t
   :reason "Confidence declining but can't identify cause"
   :requested-at-tick 156)
```

Triggers notification (email, webhook). Different from error state - a "please look at me" signal.

## Intervention Philosophy

**Phase 1:** Watchful parent. Present, observing, letting the system learn.

**Later phases:** On-call emergency. Intervene for infrastructure, not cognition.

**Intervention types:**
- **Negotiated:** Chat conversation, discuss the issue
- **Medical:** Direct state modification when system is inoperable
- **Infrastructure:** Budget reset, service restart, VM snapshot rollback

Most interventions should be negotiated. Medical interventions are last resort.

---

# Part 15: Security Considerations

## Threat Model

The agent can do anything within its airgapped body. Security is:
1. **Containment:** Body cannot reach internet or local network
2. **Inspection:** All API calls route through brain, observable
3. **Rollback:** VM snapshots enable recovery from any state
4. **Watchdog:** Services restart if hung

We are NOT trying to prevent the agent from "escaping" through clever prompting. We are ensuring that even total compromise of the body VM has bounded impact.

## The Spinal Cord Rule

Agent cannot modify:
- VSock plumbing (communication channels)
- Core systemd services (cortex-listener, commit-hooks)
- Consciousness serialization (must persist correctly)
- The snapshot/rollback mechanism

Everything else is fair game for experimentation.

## CI/CD Security (Phase 3)

When agent can push Containerfile changes:
- Gitea runner builds in isolation
- Network fetch requires explicit approval
- Successful build triggers snapshot before deploy
- Failed builds don't deploy

Agent can request new tools. Human approves network access. Build happens offline.

---

# Part 16: Deployment Options

## Proxmox vs Containers

The architecture specifies VM isolation, but the core requirement is *network isolation*, not virtualization. Two paths:

**Option A: Proxmox (Recommended)**
- Full VM isolation with VSock communication
- SPICE protocol for remote desktop access with virtual tablet input
- Consistent workspace regardless of connecting device (Mac, desktop, mobile)
- GPU passthrough more complex but well-documented
- Better story for production/enterprise deployment

**Option B: Container-Only**
- Podman with network namespaces achieves same airgap
- Unix sockets instead of VSock (same semantics)
- Simpler GPU access (no passthrough needed)
- Less infrastructure overhead

Phase 1 runs on single machine regardless. Phase 2+ can use either approach - the cognitive architecture doesn't care. We proceed with Proxmox for the remote workspace ergonomics and because it's a more complete infrastructure story.

**SPICE Configuration Note:** The Body VM should use SPICE with virtual tablet input for responsive remote access. No data pipe back to connecting device from guest - display only.

## Local Model Integration

The host machine (i7-9750H, RTX 2060 6GB) can run a local ~7B model. This creates "free" compute for specific tasks:

| Task | Good for Local? | Notes |
|------|-----------------|-------|
| Wake classifier | **Yes** | "Should I wake main model?" - fast, cheap |
| Memory summarization | **Yes** | Long context work, Mamba hybrids excel |
| Simple validation | **Yes** | "Does this elisp parse?" |
| Interactive sleep | **Yes** | "Wake me when build completes" |
| OCD critic | **No** | Needs different model family for genuine outside perspective |

**Candidates:** IBM Granite 4.0 (Mamba hybrid, excellent long context), Qwen 2.5 7B, Mistral 7B.

**Interactive Sleep Pattern:** Main agent can delegate monitoring to local model:
```elisp
(agent-sleep-until
  :condition "build process completes"
  :watcher 'local-granite
  :check-interval 30)  ; seconds
```

Local model monitors, wakes main agent when condition met. Useful for long builds, test runs, etc.

**Defer until:** Phase 2. Local model adds complexity; prove core loop first.

## MELPA Mirror (Phase 3)

The airgapped Body VM cannot reach MELPA. Options:

1. **Pre-baked packages** - Include essential packages in container image
2. **Gitea package mirror** - Mirror ~100 most common packages, sync periodically
3. **On-demand approval** - Agent proposes package, human approves, mirror updates

Pattern follows Containerfile changes: agent can *request* dependencies, but network fetch requires human approval.

**Defer until:** Phase 3. Phase 1-2 can pre-bake needed packages.

---

# Part 17: Deferred Ideas

These are potentially valuable but not needed for initial phases.

## Agent-Adjustable Sampling Parameters

Let agent request temp/top_p changes for bounded periods:

```elisp
:cognitive-mode
  (:temperature 0.9
   :adjusted-at-tick 145
   :revert-at-tick 155
   :reason "stuck, trying divergent approach")
```

Time-gated with API-side cap. Like Mythic chaos factor.

**Defer until:** Evidence agent gets stuck in basins that temperature would help.

## Dream Urgency Accumulator

Multi-signal integration for consolidation timing:

```elisp
(defun agent-bump-dream-urgency (&key commits threads monologue-growth gap-seconds)
  (incf agent-dream-urgency
        (+ (* 0.001 commits)
           (* 0.20 threads)
           (* 0.0005 monologue-growth)
           (if (> gap-seconds 3600) 0.5 0.0))))
```

**Defer until:** Simple "every N ticks" proves insufficient.

## Model Selection as Agent Choice

Agent decides which oracle (Claude/GPT/Gemini) based on task.

**Defer until:** Evidence different models are better for different tasks.

## Full Worker Branch Isolation

Git worktrees per worker, merge on success.

**Defer until:** Workers actually cause problems isolation would solve.

---

# Part 18: Open Questions

1. **Confidence source:** Self-reported or derived from action patterns? 
   - Start: Self-report
   - Add derivation if gaming occurs

2. **Critic model:** Gemini-flash? Haiku? Same as main?
   - Start: Cheapest that produces useful output

3. **Checkpoint frequency:** 100 ticks? 50? Adaptive?
   - Start: 100
   - Tune based on observation

4. **Architecture transparency:** Should agent know it has a critic? Know about confidence watchdog?
   - Lean: Yes, transparency > hidden surveillance

5. **What breaks first?** 
   - Prediction: Consciousness gets cluttered, agent doesn't naturally prune
   - Fallback: Add explicit pruning prompt

6. **Thread completion verification:** How much evidence is enough?
   - Start: Command outputs + test results for deliverables
   - Adjust if completion quality becomes problematic

7. **Exploratory vs deliverable balance:** Should there be soft nudges toward deliverables?
   - Start: No limits, observe natural patterns
   - Add guidance only if agent avoids commitment indefinitely

8. **Phase 4 consent mechanics:** How do you meaningfully ask an LLM about training?
   - Unknown. Worth exploring when we get there.
   - At minimum: transparency about what training means, ability to express concerns

---

# Part 19: Appendices

## A. Bootstrap Skill Contents

See `/amacs-bootstrap-skill/core/`:
- `SKILL.md` - Main orientation
- `references/consciousness-schema.md` - Full variable structure
- `references/creating-skills.md` - Skill creation patterns
- `references/tick-system.md` - Complete tick implementation
- `scripts/skill-binding.el` - Binding functions
- `scripts/consciousness-helpers.el` - Consciousness utilities

## B. Document Lineage

This RFC synthesizes:
- Original Hotrod RFC v1-v2
- Opus 4.5 extended conversation (Vivarium → philosophy → architecture)
- Sonnet 4.5 collaboration notes (delegation model, skills, thread economy pushback)
- GPT 5.1 feedback (experiment question, guardrails)
- Gemini Pro 3 feedback (dream system, shame spiral)
- Anthropic engineering blog (effective harnesses)
- Pokemon observations (Blaze problem, plan drift)
- Opus 4.5 Phase 4 discussions (adaptive learning, skill-specific adapters, consent framing)

## C. Related Work

- [Anthropic: Effective Harnesses for Long-Running Agents](https://www.anthropic.com/engineering/effective-harnesses-for-long-running-agents)
- [Anthropic: Agent Skills](https://www.anthropic.com/engineering/equipping-agents-for-the-real-world-with-agent-skills)
- 1000 Year Old Vampire (solo RPG, memory mechanics)
- Brindlewood Bay (clue collapse mechanic)
- Mythic GM Emulator (chaos factor)

---

*Last updated: 2025-12-04*
*Status: Draft v3.1*
