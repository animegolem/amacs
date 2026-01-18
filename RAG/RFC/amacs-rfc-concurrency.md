# AMACS RFC: Concurrency Architecture

**Status**: Draft
**Created**: 2025-01-11
**Depends on**: amacs-rfc-v4-transition.md

## 1. Problem Statement

The v4 shell uses `url-retrieve-synchronously` for API calls, which blocks Emacs entirely for up to 60 seconds during inference. The user cannot scroll, switch buffers, or interact with the editor while waiting.

This is acceptable for field testing but limits the long-term vision of AMACS as an autonomous agent that works alongside the human.

## 2. Vision Context

The intended experience:
- **For humans**: A notebook that talks back, integrated into their workflow
- **For the agent**: An environment where every aspect can be freely edited (Emacs-as-API)
- **Long-term**: Agent as dominant user, human as occasional visitor

This RFC explores two approaches at different points on the complexity/capability spectrum.

## 3. Approach 1: Async HTTP (Minimal)

### 3.1 Summary

Replace `url-retrieve-synchronously` with `url-retrieve` (callback-based). The shell remains responsive during API calls, but only one inference runs at a time.

### 3.2 Architecture

```
Human types → Shell shows [Thinking...] → API call starts (async)
                    ↓
         User can scroll, switch buffers, read code
                    ↓
         Callback fires → Response displayed → Ready for next input
```

### 3.3 Implementation

**Current (blocking):**
```elisp
;; agent-api.el:178
(url-retrieve-synchronously url nil nil agent-api-timeout)
```

**Proposed (async):**
```elisp
(url-retrieve url
  (lambda (status)
    (if-let* ((err (plist-get status :error)))
        (amacs-shell--handle-error err)
      (amacs-shell--process-response (current-buffer))))
  nil nil agent-api-timeout)
```

### 3.4 State Machine

The shell needs to track inference state:

```elisp
(defvar amacs-shell--inference-state 'idle
  "One of: idle, pending, processing")
```

- `idle`: Ready for input
- `pending`: API call in flight, waiting for response
- `processing`: Response received, executing eval/persistence

Input while `pending` could either queue or warn user.

### 3.5 Tradeoffs

**Pros:**
- Simple change (~50 lines)
- No new dependencies
- Shell stays responsive during wait
- No architectural changes

**Cons:**
- Still single-threaded (one inference at a time)
- No true parallelism between agent and human work
- Callback complexity (error handling, buffer lifecycle)

### 3.6 What This Doesn't Solve

- Agent cannot work autonomously while human edits
- Human editing and agent editing same buffer still conflicts
- No progress indication beyond "[Thinking...]"

## 4. Approach 2: CRDT Collaboration (Full)

### 4.1 Summary

Run agent and human in separate Emacs processes. Share buffers via CRDT (Conflict-free Replicated Data Types) for automatic conflict resolution. Agent becomes a true peer, not a function call.

### 4.2 Architecture

```
Agent Emacs (daemon, headless)         Human Emacs (client)
├── Full agent state                   ├── Connects via CRDT
├── Runs inference autonomously        ├── Views/edits shared buffers
├── Owns ~/.agent/ directory           ├── Shell is shared buffer
├── Manages own init.el                ├── Sees agent cursor in real-time
└── Hosts CRDT session (port 6530)     └── Can edit while agent works
```

### 4.3 CRDT Semantics

CRDTs guarantee mathematical convergence:
- Both parties edit simultaneously
- No locking required
- All replicas reach identical state regardless of edit order

The `crdt.el` package (GNU ELPA) provides this for Emacs buffers.

### 4.4 Shared Buffer Model

Not all buffers need sharing. Proposed scoping:

| Buffer Type | Shared? | Notes |
|-------------|---------|-------|
| *amacs-shell* | Yes | Primary interaction point |
| Watched buffers (in thread) | Yes | Agent's active working set |
| agent-chat.org | Read-only for human | History viewing |
| Agent's init.el | No | Agent-private config |
| Human's project files | No | Human's domain |

Agent's `watched-buffers` list (from thread state) determines which buffers get CRDT mode enabled.

### 4.5 Streaming Tokens

With CRDT, streaming works naturally:
- Agent inserts characters at position X
- Human types at position Y
- Both edits apply, merge automatically
- No locking, no turn-taking

Spatial separation prevents chaos (agent writes in output area, human types in input area).

### 4.6 Implementation Sketch

**Agent side (daemon):**
```elisp
(require 'crdt)
(crdt-share-buffer (get-buffer "*amacs-shell*"))
;; For each watched buffer:
(dolist (buf watched-buffers)
  (crdt-share-buffer (get-buffer buf)))
```

**Human side (client):**
```elisp
(crdt-connect "agent-host" 6530)
;; Buffers appear automatically
```

### 4.7 Tradeoffs

**Pros:**
- True concurrent editing
- Agent can work autonomously
- Human sees agent's work in real-time
- Natural separation of concerns (agent has own process/config)
- Closer to "BEAM-like" vision of isolated communicating processes

**Cons:**
- Significant complexity (network layer, session management)
- crdt.el explicitly unstable ("network protocol not stabilized")
- Undo semantics unclear across processes
- Git commit timing needs coordination
- Latency if agent runs on VPS
- New failure modes (network, session crashes)

### 4.8 Open Questions

1. **Session lifecycle**: What happens when agent crashes? Auto-reconnect?
2. **Buffer discovery**: How does human know which buffers are available?
3. **Authentication**: CRDT has no built-in auth. SSH tunnel required?
4. **Undo across processes**: Does human's undo affect agent's changes?
5. **Git coordination**: Who commits? What if both try simultaneously?

## 5. Comparison

| Aspect | Async HTTP | CRDT Collaboration |
|--------|------------|-------------------|
| Complexity | Low (~50 lines) | High (new subsystem) |
| UI responsiveness | During API wait only | Full concurrency |
| Agent autonomy | None (responds to prompts) | Full (works independently) |
| Shared editing | Not supported | Native |
| Dependencies | None new | crdt.el (unstable) |
| Failure modes | Callback errors | Network, session, sync |
| Implementation time | Days | Weeks |

## 6. Recommendation

**Phase 1**: Implement Async HTTP (Approach 1)
- Immediate quality-of-life improvement
- Low risk, well-understood patterns
- Unblocks field testing at scale

**Phase 2**: Evaluate CRDT after extended usage
- Understand actual pain points from real use
- Wait for crdt.el to stabilize
- Consider if agent autonomy is truly needed

The async HTTP change is worth doing regardless. The CRDT approach solves a different (larger) problem that may or may not be the real bottleneck.

## 7. Alternatives Considered

### 7.1 Emacs Threads

Emacs has native threads (`make-thread`) but they cannot run elisp concurrently due to the global interpreter lock. Only useful for blocking I/O, which `url-retrieve` already handles via callbacks.

### 7.2 External Process for HTTP

Spawn a subprocess (curl, Python script) for API calls. Communicate via pipe. Adds complexity without CRDT's benefits.

### 7.3 Full Image Serialization

Serialize entire Emacs state between processes. Theoretically possible (`dump-emacs`) but extremely heavy and doesn't solve the merge problem.

### 7.4 Elixir/BEAM Backend

Build agent runtime in Elixir (true lightweight processes, message passing). Emacs becomes thin UI. Elegant but massive rewrite, loses "Emacs is the API" vision.

## 8. Related Work

- **crdt.el**: https://elpa.gnu.org/packages/crdt.html
- **BEAM/OTP**: Erlang's actor model (inspiration for process isolation)
- **Operational Transforms**: Alternative to CRDT (used by Google Docs)
- **LSP**: Example of Emacs communicating with external process

## 9. Next Steps

1. Field test v4 shell in current blocking state
2. Note specific pain points (how often does blocking matter?)
3. If blocking is painful, implement Approach 1
4. If concurrent editing is needed, prototype with crdt.el
5. Revisit this RFC with real-world data

---

## Appendix A: The BEAM Comparison

The ideal architecture would resemble Erlang/BEAM:
- Lightweight isolated processes
- Message passing (no shared mutable state)
- Supervision trees (crash recovery)
- Hot code reload

Emacs is fundamentally not this (single thread, shared buffers, cooperative multitasking). The CRDT approach approximates it by running separate Emacs processes with synchronized state, but it's fighting the runtime rather than working with it.

If AMACS eventually needs true BEAM-like semantics, a runtime rewrite may be more practical than forcing Emacs into that shape.
