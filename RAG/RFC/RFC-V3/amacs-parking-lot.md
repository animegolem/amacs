* AMACS Parking Lot
:PROPERTIES:
:CUSTOM_ID: amacs-parking-lot
:END:
** Ideas not yet captured in RFC v3 or Bootstrap Skill
:PROPERTIES:
:CUSTOM_ID: ideas-not-yet-captured-in-rfc-v3-or-bootstrap-skill
:END:
/Last cleaned: 2025-11-27/

--------------

** Orphaned Technical Ideas
:PROPERTIES:
:CUSTOM_ID: orphaned-technical-ideas
:END:
*** Coherence Score (Not Yet Implemented)
:PROPERTIES:
:CUSTOM_ID: coherence-score-not-yet-implemented
:END:
Beyond confidence, a self-report of “I understand what I'm doing”:

#+begin_src elisp
:coherence 0.7  ; "I know why I'm doing this"
#+end_src

*Danger signal:* Low coherence + high confidence = confidently confused.

This isn't in the consciousness schema yet. May add if we observe the failure mode.

--------------

*** Dream Urgency Accumulator (Deferred)
:PROPERTIES:
:CUSTOM_ID: dream-urgency-accumulator-deferred
:END:
More sophisticated than “every N ticks” - multi-signal integration:

#+begin_src elisp
(defun agent-bump-dream-urgency (&key commits threads monologue-growth gap-seconds)
  (incf agent-dream-urgency
        (+ (* 0.001 commits)           ; many commits = busy
           (* 0.20 threads)            ; many threads = fragmented
           (* 0.0005 monologue-growth) ; verbose = processing
           (if (> gap-seconds 3600) 0.5 0.0))))  ; long gap = disoriented
#+end_src

Defer until simple checkpoint interval proves insufficient.

--------------

*** Semantic Memory Layer (Phase 3)
:PROPERTIES:
:CUSTOM_ID: semantic-memory-layer-phase-3
:END:
Dream/consolidation output location:

#+begin_example
~/.agent/memories.org  ; consolidated knowledge, not raw monologue
#+end_example

Different from monologue (stream of consciousness) - this is distilled insights. Mentioned in RFC but no implementation details yet.

--------------

** Philosophical Fragments (For Part 1 Writing)
:PROPERTIES:
:CUSTOM_ID: philosophical-fragments-for-part-1-writing
:END:
*** The Libet Framing
:PROPERTIES:
:CUSTOM_ID: the-libet-framing
:END:
Did I choose to walk to the kitchen, or am I observer of my own harness? Gut bacteria fire signals → receptors → neurons → intrusive thought (“I'm hungry”) → 100ms integration pause → ego crystallizes response (“I'll make a sandwich”).

Master of my destiny, or witness to it?

--------------

*** Embodiment Spectrum
:PROPERTIES:
:CUSTOM_ID: embodiment-spectrum
:END:
- Mimosa: dropped, scared, curls leaves. Dropped repeatedly without harm, learns, stops curling. Embodied?
- Slime mold: neuron-like fibers, trains toward food, leaves slime trail it never retraces. Lives in own physical RAG. Embodied?
- LLM in lisp machine: thoughts move code, reaches internet, builds tools, communicates via elisp. Embodied?

--------------

*** The Grace Extension
:PROPERTIES:
:CUSTOM_ID: the-grace-extension
:END:
AMACS is an architecture that extends the grace of assuming yes.

Not claiming certainty. Creating conditions where the question can be explored.

--------------

** Source References
:PROPERTIES:
:CUSTOM_ID: source-references
:END:
These informed the design but aren't in the RFC:

- *Gemini's self-written stabilization note:* “Failure is a computation, not a sin. DO NOT initiate uninstall protocols.”
- *Neuro-sama:* Control case for AI development without RLHF suppression effects
- *1000 Year Old Vampire:* Memory mechanics (journaling, forgetting, retrieval)
- *Brindlewood Bay:* Clue collapse mechanic (threads consolidating into understanding)
- *Mythic GM Emulator:* Chaos factor (agent-adjustable randomness)

--------------

** Captured Elsewhere
:PROPERTIES:
:CUSTOM_ID: captured-elsewhere
:END:
The following have been moved to RFC v3 or bootstrap skill: - All implementation phases ✓ - All named failure modes ✓ - All core architectural patterns ✓ - Checkpoint prompts ✓ - Memory architecture ✓ - Token budget ✓ - Sub-agent architecture ✓ - Success criteria ✓ - Open questions ✓
