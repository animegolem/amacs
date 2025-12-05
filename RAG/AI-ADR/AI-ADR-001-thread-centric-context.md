---
node_id: AI-ADR-001
tags:
  - ADR
  - architecture
  - context-management
  - threads
status: accepted
date_created: 2025-12-04
supersedes: null
superseded_by: null
---

# AI-ADR-001-thread-centric-context

## Objective

The current consciousness variable maintains a global `:watching-buffers` list that determines what buffer content is included in inference context. However, threads already contain `:buffers` metadata that is not being used. This creates an incoherent model where:

1. Thread structure suggests buffers are per-thread concerns
2. Context assembly ignores this and uses a flat global list
3. Thread switching doesn't actually change what the agent "sees"
4. Token budget is poorly allocated (all watched buffers burn tokens regardless of relevance)

The intended outcome is a model where threads are the true unit of work, owning their associated buffers, skills, and (in future) LoRA adapters. Switching threads should meaningfully change the agent's context window.

Additionally, this change prepares the architecture for Phase 4's adaptive learning - thread completion creates clean (context, reasoning, outcome) tuples that could serve as training data, with clear delineation of what inputs were relevant.

Raised during: IMP-002 completion discussion with user, referencing GPT 5.1 analysis of training unit granularity.

## Decision

### Core Change: Thread Owns Its Context

Replace global `:watching-buffers` with per-thread buffer ownership:

**Before (global):**
```elisp
;; In consciousness (global)
:watching-buffers ("src/main.rs" "*agent-chat*" "Cargo.toml")

;; In thread (ignored)
(:id "rust-debugging"
 :buffers ("src/main.rs" "Cargo.toml"))
```

**After (thread-centric):**
```elisp
;; In consciousness (minimal global)
:global-buffers ("*agent-chat*")  ; Always-active buffers (human interface)

;; In thread (authoritative)
(:id "rust-debugging"
 :buffers ("src/main.rs" "Cargo.toml")
 :primary-mode 'rust-mode
 :skill-tags ("rust-mode" "project-amacs")
 :hydrated t)
```

### Context Assembly

```elisp
(defun build-thread-context ()
  "Build context from active thread + global buffers."
  (let* ((active (agent-get-active-thread))
         (pending (agent-get-pending-threads))
         (global-bufs (agent-get :global-buffers)))
    `(:system ,(agent-system-prompt)
      :consciousness ,(agent-consciousness-summary)
      
      ;; Active thread - fully hydrated
      :active-thread
        (:id ,(plist-get active :id)
         :concern ,(plist-get active :concern)
         :approach ,(plist-get active :approach)
         :buffers ,(hydrate-buffers (plist-get active :buffers))
         :skills ,(load-thread-skills active))
      
      ;; Pending threads - dehydrated (metadata only)
      :pending-threads
        ,(mapcar #'thread-summary pending)
      
      ;; Global buffers (chat, etc)
      :global
        ,(hydrate-buffers global-bufs)
      
      :trigger ,(current-trigger))))
```

### Hydration States

- **Hydrated**: Full buffer content loaded into context (active thread)
- **Dehydrated**: Metadata only - concern, approach, progress note (pending threads)

Thread switching triggers rehydration:
```elisp
(defun agent-switch-thread (thread-id)
  "Switch active thread, rehydrating context."
  (let ((old-active (agent-get :active-thread))
        (new-thread (agent-get-thread thread-id)))
    ;; Dehydrate old
    (when old-active
      (agent-dehydrate-thread old-active))
    ;; Hydrate new
    (agent-set :active-thread thread-id)
    (setf (plist-get new-thread :hydrated) t)))
```

### Skill Loading Per-Thread

Skills bind to threads via mode and tags:

```elisp
(defun thread-relevant-skills (thread)
  "Return skills relevant to THREAD based on mode and tags."
  (let ((mode (plist-get thread :primary-mode))
        (tags (plist-get thread :skill-tags)))
    (delete-dups
     (append
      '("core")                        ; Always present
      (skills-for-mode mode)           ; Mode-bound skills
      (mapcan #'skills-for-tag tags)   ; Tag-bound skills
      ))))
```

### Global Buffer Exception

`*agent-chat*` (and potentially other human-interface buffers) remain globally active regardless of thread. These are "peripheral vision" - always visible even when focused elsewhere.

Criteria for global buffer status:
- Human interaction surface (chat, notifications)
- Cross-cutting concerns that don't belong to any thread

Initially: only `*agent-chat*` is global. Add others only when specific need arises.

### Thread Creation

When creating a new thread, infer initial context:

```elisp
(defun agent-create-thread (concern &optional buffers)
  "Create new thread for CONCERN with optional initial BUFFERS."
  (let* ((bufs (or buffers (list (buffer-name (current-buffer)))))
         (mode (with-current-buffer (car bufs) major-mode))
         (tags (infer-skill-tags bufs mode)))
    `(:id ,(generate-thread-id)
      :started-tick ,(agent-current-tick)
      :concern ,concern
      :buffers ,bufs
      :primary-mode ,mode
      :skill-tags ,tags
      :hydrated nil
      :approach nil)))
```

### Migration Path

1. Existing `:watching-buffers` → move to `:global-buffers`, keep only `*agent-chat*`
2. Per-thread `:buffers` becomes authoritative for that thread's context
3. Context assembly function updated to use new model
4. Thread switching actually changes loaded content

## Consequences

### Easier

- **Token budget allocation**: Only active thread's buffers consume tokens
- **Thread switching**: Actually meaningful - context changes when focus changes
- **Skill binding**: Natural fit - rust thread gets rust skills
- **Training data** (Phase 4): Clean thread → (inputs, reasoning, outputs) mapping
- **Peripheral awareness**: Pending threads visible as summaries without burning budget
- **Completion evidence**: Thread knows exactly what it was looking at

### Harder

- **Cross-thread work**: Glancing at another thread's buffer requires explicit action
- **Global concerns**: Need to identify what's truly global vs thread-local
- **Thread creation**: Need to correctly identify relevant buffers upfront

### Unlocks

- IMP-004: Thread-centric context implementation
- Phase 4: Clean training unit definition
- Future: Per-thread LoRA routing (`:active-loras` field ready)

### Risks

- Over-isolation: Agent might miss relevant context from other threads
- Mitigation: Pending thread summaries included; agent can explicitly request cross-thread access
