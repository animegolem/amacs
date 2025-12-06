;;; agent-game-agent.el --- AI agent management -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: amacs
;; Keywords: games, agents, ai

;;; Commentary:

;; Manages AI agents that play games.
;; Each agent has a provider, model, and memory.
;; Agents can be human or AI-controlled.

;;; Code:

(require 'agent-game-provider)
(require 'agent-game-memory)
(require 'cl-lib)

;;; Agent Registry

(defvar agent-game-agents (make-hash-table :test 'equal)
  "Registry of all agents.
Each agent is a plist with:
  :id - Agent ID (symbol)
  :type - 'human or 'ai
  :provider - Provider name (string, for AI agents)
  :model - Model name (string, for AI agents)
  :prompt-fn - Function to generate prompt (for AI agents)")

(defun agent-game-agent-register (id type &optional provider model prompt-fn)
  "Register an agent with ID and TYPE.
TYPE is 'human or 'ai.
For AI agents, provide PROVIDER, MODEL, and optionally PROMPT-FN."
  (let ((agent (list :id id
                    :type type
                    :provider provider
                    :model model
                    :prompt-fn prompt-fn)))
    (puthash id agent agent-game-agents)
    (message "Registered agent: %s (%s)" id type)
    agent))

(defun agent-game-agent-get (id)
  "Get agent by ID."
  (gethash id agent-game-agents))

(defun agent-game-agent-is-ai-p (id)
  "Return t if agent ID is AI-controlled."
  (let ((agent (agent-game-agent-get id)))
    (and agent (eq (plist-get agent :type) 'ai))))

(defun agent-game-agent-is-human-p (id)
  "Return t if agent ID is human-controlled."
  (let ((agent (agent-game-agent-get id)))
    (and agent (eq (plist-get agent :type) 'human))))

;;; Prompt Generation

(defun agent-game-agent-build-prompt (agent-id game-type)
  "Build prompt for AGENT-ID playing GAME-TYPE.
Uses the agent's prompt-fn if available, otherwise uses default."
  (let* ((agent (agent-game-agent-get agent-id))
         (prompt-fn (plist-get agent :prompt-fn)))
    (if prompt-fn
        (funcall prompt-fn agent-id game-type)
      ;; Default prompt builder
      (agent-game-agent-default-prompt agent-id game-type))))

(defun agent-game-agent-default-prompt (agent-id game-type)
  "Build default prompt for AGENT-ID playing GAME-TYPE."
  (let* ((state-info (agent-game-state-to-string))
         (memory (agent-game-memory-read game-type agent-id "memory/game-state.org"))
         (game-specific-prompt
          (pcase game-type
            ('candyland (agent-game-agent-candyland-prompt agent-id))
            (_ "Play the game."))))

    (format "%s

YOUR MEMORY:
%s

CURRENT GAME STATE:
%s

YOUR TURN - What do you do?"
            game-specific-prompt
            (or memory "No memory yet.")
            state-info)))

(defun agent-game-agent-candyland-prompt (agent-id)
  "Build Candyland-specific prompt for AGENT-ID."
  (require 'candyland)
  (let* ((position (candyland-agent-get-position agent-id))
         (all-positions (candyland-agent-get-all-positions))
         (last-roll (candyland-agent-get-last-roll)))
    (format "You are playing CANDYLAND as %s.

RULES:
1. On your turn, you must ROLL the die
2. The die shows a color (red, blue, yellow, green, purple, orange)
3. You move to the next space of that color
4. First player to reach position 30 wins
5. That's it - just roll!

AVAILABLE ACTIONS:
- ROLL: Roll the die and move (you should do this)

YOUR STATUS:
- Current position: %d
- Target: Reach position 30

OTHER PLAYERS:
%s

LAST ROLL: %s

INSTRUCTIONS:
Respond with exactly: ROLL

Nothing else - just the word ROLL in all caps."
            agent-id
            position
            (mapconcat (lambda (p)
                        (format "- %s: position %d" (car p) (cdr p)))
                      all-positions
                      "\n")
            (or last-roll "none yet"))))

;;; Structured State (for future sidecar/complex games)

(defun agent-game-agent-get-structured-state (agent-id)
  "Return minimal structured state for AGENT-ID.
This is what an agent actually needs to make decisions.
Separates concerns from human-readable rendering."
  (let ((game-type (agent-game-state-get 'type)))
    (pcase game-type
      ('candyland
       (require 'candyland)
       `((game . candyland)
         (agent-id . ,agent-id)
         (your-position . ,(candyland-agent-get-position agent-id))
         (target-position . ,candyland-board-length)
         (is-your-turn . ,(equal agent-id (agent-game-state-get 'current-player)))
         (all-positions . ,(candyland-agent-get-all-positions))
         (last-roll . ,(candyland-agent-get-last-roll))
         (winner . ,(agent-game-state-get 'winner))
         (turn-count . ,(agent-game-state-get 'turn-count))
         (valid-actions . (roll))))
      (_ nil))))

(defun agent-game-agent-state-to-json (agent-id)
  "Return structured state as JSON string for AGENT-ID.
Useful for sidecar communication or debugging."
  (json-encode (agent-game-agent-get-structured-state agent-id)))

;;; Agent Turn Execution

(defun agent-game-agent-take-turn (agent-id)
  "Execute turn for AGENT-ID.
If AI, queries the model and parses response.
If human, waits for user input."
  (let ((agent (agent-game-agent-get agent-id)))
    (unless agent
      (error "Unknown agent: %s" agent-id))

    (pcase (plist-get agent :type)
      ('ai (agent-game-agent-ai-turn agent-id))
      ('human (agent-game-agent-human-turn agent-id))
      (_ (error "Unknown agent type for %s" agent-id)))))

(defun agent-game-agent-ai-turn (agent-id)
  "Execute AI turn for AGENT-ID asynchronously."
  (let* ((agent (agent-game-agent-get agent-id))
         (provider (plist-get agent :provider))
         (model (plist-get agent :model))
         (game-type (agent-game-state-get 'type)))

    (message "🤖 %s (%s/%s) is thinking..." agent-id provider model)

    ;; Build prompt
    (let ((prompt (agent-game-agent-build-prompt agent-id game-type)))

      ;; Call AI provider asynchronously
      (agent-game-provider-call-async
       provider model prompt

       ;; Success callback
       (lambda (response)
         (message "🤖 %s responded: %s" agent-id response)
         ;; Parse response and execute action
         (agent-game-agent-parse-and-execute agent-id response game-type)
         ;; Refresh display
         (agent-game-render-refresh)
         ;; Continue auto-play if enabled
         (when agent-game-agent-auto-play
           (run-with-timer 0.5 nil #'agent-game-agent-auto-play-tick)))

       ;; Error callback
       (lambda (err)
         (message "❌ AI agent %s failed: %s" agent-id err)
         (agent-game-memory-log-action game-type agent-id "ai-error" err))))))

(defun agent-game-agent-parse-and-execute (agent-id response game-type)
  "Parse RESPONSE from AI and execute action for AGENT-ID in GAME-TYPE."
  (pcase game-type
    ('candyland
     ;; For Candyland, just look for "ROLL" in response
     (if (string-match-p "ROLL" (upcase response))
         (progn
           (require 'candyland)
           (let ((result (candyland-agent-roll agent-id)))
             ;; Update agent memory
             (agent-game-memory-log-action
              game-type agent-id "roll"
              (format "Rolled %s: %d → %d"
                      (alist-get 'roll result)
                      (alist-get 'old-pos result)
                      (alist-get 'new-pos result)))
             result))
       (message "⚠️  %s didn't say ROLL, trying anyway..." agent-id)
       (require 'candyland)
       (candyland-agent-roll agent-id)))

    (_ (error "Game type %s not yet supported for AI agents" game-type))))

(defun agent-game-agent-human-turn (agent-id)
  "Wait for human to take turn as AGENT-ID.
This is a no-op - humans use the UI (press 'r' to roll)."
  (message "Waiting for %s to take their turn (press 'r' to roll)..." agent-id))

;;; Auto-play Mode

(defvar agent-game-agent-auto-play nil
  "If t, AI agents automatically take their turns.
If nil, user must trigger AI turns manually.")

(defun agent-game-agent-auto-play-tick ()
  "Check if current player is AI and auto-play if enabled."
  (when agent-game-agent-auto-play
    (let ((current (agent-game-state-get 'current-player)))
      (when (and current
                 (agent-game-agent-is-ai-p current)
                 (not (agent-game-state-get 'winner)))
        (run-with-timer 0.5 nil #'agent-game-agent-take-turn current)))))

;;;###autoload
(defun agent-game-agent-toggle-auto-play ()
  "Toggle automatic AI turn-taking."
  (interactive)
  (setq agent-game-agent-auto-play (not agent-game-agent-auto-play))
  (message "AI auto-play: %s" (if agent-game-agent-auto-play "ON" "OFF"))
  (when agent-game-agent-auto-play
    (agent-game-agent-auto-play-tick)))

;;;###autoload
(defun agent-game-agent-trigger-ai-turn ()
  "Manually trigger AI turn for current player."
  (interactive)
  (let ((current (agent-game-state-get 'current-player)))
    (if (agent-game-agent-is-ai-p current)
        (agent-game-agent-take-turn current)
      (message "%s is not an AI agent" current))))

(provide 'agent-game-agent)
;;; agent-game-agent.el ends here
