---
node_id: AI-IMP-032
tags:
  - IMP-LIST
  - Implementation
  - hub
  - api
  - settings
  - self-modification
kanban_status: planned
depends_on: 
  - AI-IMP-029
confidence_score: 0.8
created_date: 2025-12-25
close_date:
--- 

# AI-IMP-032: API Settings Section

## Summary

Add API settings section to hub displaying inference parameters, modifiable by both human and agent.

**Current state:** API parameters hardcoded or in config file. Agent cannot modify its own inference settings.

**Target state:** Settings visible in hub status bar, editable via keybindings, storable in consciousness for agent self-modification.

**Why:**
- Transparency: human sees what parameters agent is using
- Agent autonomy: agent can adjust temperature, enable thinking based on task
- Experimental: explore what happens when agent controls its own cognition

**Done when:** Settings display in hub, human can modify via keybindings, agent can modify via elisp, changes persist and apply to next inference.

### Out of Scope

- Per-tick parameter overrides (always use consciousness values)
- Model switching mid-session (requires API reconfiguration)
- Cost estimation based on parameters

### Design/Approach

**Consciousness schema addition:**
```elisp
((api-settings . ((temperature . 1.0)
                  (top-p . 1.0)
                  (top-k . nil)
                  (thinking . nil)
                  (max-tokens . 8192)
                  (model . "anthropic/claude-sonnet-4")))
 ...)
```

**Hub display (status bar):**
```
TEMP: 1.0 | TOP_P: 1.0 | THINKING: off | MODEL: claude-sonnet-4
```

**Human modification keybindings:**
- `M-t` - toggle thinking
- `M-T` - set temperature (read-number)
- `M-p` - set top-p (read-number)
- `M-m` - set model (completing-read)

**Agent modification:**
```elisp
;; Agent can eval these
(agent-set-api-param 'temperature 0.7)
(agent-set-api-param 'thinking t)
(agent-get-api-param 'temperature)
```

**Safety bounds:**
```elisp
(defvar agent-api-param-bounds
  '((temperature . (0.0 . 2.0))
    (top-p . (0.0 . 1.0))
    (max-tokens . (1 . 32768)))
  "Min/max bounds for API parameters.")
```

Agent modifications clamped to bounds. Human can override bounds if needed.

**Application at inference:**
`agent-api-call` reads from consciousness instead of hardcoded values:
```elisp
(defun agent--get-inference-params ()
  (let ((settings (alist-get 'api-settings agent-consciousness)))
    `((temperature . ,(alist-get 'temperature settings 1.0))
      (top_p . ,(alist-get 'top-p settings 1.0))
      ...)))
```

### Files to Touch

- `harness/amacs-hub.el`: Status bar section, keybindings
- `harness/agent-consciousness.el`: Add api-settings to default, accessors
- `harness/agent-api.el`: Read params from consciousness
- `skills/amacs-bootstrap-skill/core/SKILL.md`: Document self-modification

### Implementation Checklist

<CRITICAL_RULE>
Before marking an item complete on the checklist MUST **stop** and **think**. Have you validated all aspects are **implemented** and **tested**? 
</CRITICAL_RULE> 

- [ ] Add `api-settings` to default consciousness schema
- [ ] Implement `agent-get-api-param` accessor
- [ ] Implement `agent-set-api-param` with bounds checking
- [ ] Define `agent-api-param-bounds` variable
- [ ] Update `agent-api-call` to read from consciousness
- [ ] Implement `amacs-hub--insert-api-settings` section
- [ ] Format settings as readable status bar
- [ ] Implement `amacs-hub-toggle-thinking` command
- [ ] Implement `amacs-hub-set-temperature` command
- [ ] Implement `amacs-hub-set-top-p` command
- [ ] Implement `amacs-hub-set-model` command
- [ ] Bind M-t, M-T, M-p, M-m in hub mode
- [ ] Refresh hub after parameter change
- [ ] Update core SKILL.md with self-modification docs
- [ ] Test human modification via hub
- [ ] Test agent modification via eval
- [ ] Test bounds enforcement
- [ ] Test parameters actually applied to API call

### Acceptance Criteria

**Scenario:** Display current settings
**GIVEN** hub is open
**THEN** status bar shows current temperature, top_p, thinking, model
**AND** values match consciousness api-settings

**Scenario:** Human modifies temperature
**GIVEN** hub is open
**WHEN** user presses `M-T` and enters 0.7
**THEN** temperature updates in consciousness
**AND** hub refreshes showing new value
**AND** next inference uses temperature 0.7

**Scenario:** Agent enables thinking
**GIVEN** agent evaluates `(agent-set-api-param 'thinking t)`
**THEN** thinking enabled in consciousness
**AND** next inference includes thinking parameter
**AND** hub shows "THINKING: on"

**Scenario:** Bounds enforced
**GIVEN** agent evaluates `(agent-set-api-param 'temperature 5.0)`
**THEN** temperature clamped to 2.0 (max bound)
**AND** message indicates clamping occurred

### Issues Encountered

<!-- Fill during implementation -->
