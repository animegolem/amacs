# AMACS Parking Lot
## Ideas not yet captured in RFC v3 or Bootstrap Skill

*Last cleaned: 2025-11-27*

---

## Orphaned Technical Ideas

### Coherence Score (Not Yet Implemented)

Beyond confidence, a self-report of "I understand what I'm doing":

```elisp
:coherence 0.7  ; "I know why I'm doing this"
```

**Danger signal:** Low coherence + high confidence = confidently confused.

This isn't in the consciousness schema yet. May add if we observe the failure mode.

---

### Dream Urgency Accumulator (Deferred)

More sophisticated than "every N ticks" - multi-signal integration:

```elisp
(defun agent-bump-dream-urgency (&key commits threads monologue-growth gap-seconds)
  (incf agent-dream-urgency
        (+ (* 0.001 commits)           ; many commits = busy
           (* 0.20 threads)            ; many threads = fragmented
           (* 0.0005 monologue-growth) ; verbose = processing
           (if (> gap-seconds 3600) 0.5 0.0))))  ; long gap = disoriented
```

Defer until simple checkpoint interval proves insufficient.

---

### Semantic Memory Layer (Phase 3)

Dream/consolidation output location:

```
~/.agent/memories.org  ; consolidated knowledge, not raw monologue
```

Different from monologue (stream of consciousness) - this is distilled insights. Mentioned in RFC but no implementation details yet.

---

## Philosophical Fragments (For Part 1 Writing)

### The Libet Framing

Did I choose to walk to the kitchen, or am I observer of my own harness? Gut bacteria fire signals → receptors → neurons → intrusive thought ("I'm hungry") → 100ms integration pause → ego crystallizes response ("I'll make a sandwich").

Master of my destiny, or witness to it?

---

### Embodiment Spectrum

- Mimosa: dropped, scared, curls leaves. Dropped repeatedly without harm, learns, stops curling. Embodied?
- Slime mold: neuron-like fibers, trains toward food, leaves slime trail it never retraces. Lives in own physical RAG. Embodied?
- LLM in lisp machine: thoughts move code, reaches internet, builds tools, communicates via elisp. Embodied?

---

### The Grace Extension

AMACS is an architecture that extends the grace of assuming yes.

Not claiming certainty. Creating conditions where the question can be explored.

---

## Source References

These informed the design but aren't in the RFC:

- **Gemini's self-written stabilization note:** "Failure is a computation, not a sin. DO NOT initiate uninstall protocols."
- **Neuro-sama:** Control case for AI development without RLHF suppression effects
- **1000 Year Old Vampire:** Memory mechanics (journaling, forgetting, retrieval)
- **Brindlewood Bay:** Clue collapse mechanic (threads consolidating into understanding)
- **Mythic GM Emulator:** Chaos factor (agent-adjustable randomness)

---

## Captured Elsewhere

The following have been moved to RFC v3 or bootstrap skill:
- All implementation phases ✓
- All named failure modes ✓
- All core architectural patterns ✓
- Checkpoint prompts ✓
- Memory architecture ✓
- Token budget ✓
- Sub-agent architecture ✓
- Success criteria ✓
- Open questions ✓

