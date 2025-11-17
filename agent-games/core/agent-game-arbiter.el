;;; agent-game-arbiter.el --- Turn coordination and validation -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: amacs
;; Keywords: games, agents

;;; Commentary:

;; The arbiter ensures only one agent can act at a time and validates
;; all actions against game rules. This prevents race conditions and
;; enforces fair play.

;;; Code:

(require 'agent-game-state)
(require 'agent-game-audit)

(defvar agent-game-arbiter-lock nil
  "Lock to prevent simultaneous actions.
Holds the ID of the agent currently acting, or nil if no action in progress.")

(defun agent-game-arbiter-can-act-p (agent-id)
  "Return t if AGENT-ID can take an action right now."
  (let ((current (agent-game-state-get 'current-player))
        (phase (agent-game-state-get 'phase))
        (winner (agent-game-state-get 'winner)))
    (and (null winner)                    ; game not over
         (null agent-game-arbiter-lock)   ; no action in progress
         (equal agent-id current)         ; it's this agent's turn
         (eq phase 'playing))))           ; game is active

(defun agent-game-arbiter-acquire-lock (agent-id)
  "Attempt to acquire action lock for AGENT-ID.
Returns t if successful, nil if another agent holds the lock."
  (if agent-game-arbiter-lock
      (progn
        (agent-game-audit-log 'conflict
                              `((attempted-by . ,agent-id)
                                (held-by . ,agent-game-arbiter-lock)))
        nil)
    (setq agent-game-arbiter-lock agent-id)
    t))

(defun agent-game-arbiter-release-lock (agent-id)
  "Release action lock held by AGENT-ID.
Warns if AGENT-ID doesn't hold the lock."
  (if (equal agent-game-arbiter-lock agent-id)
      (setq agent-game-arbiter-lock nil)
    (warn "Agent %s tried to release lock held by %s"
          agent-id agent-game-arbiter-lock)))

(defmacro agent-game-arbiter-with-lock (agent-id &rest body)
  "Execute BODY with lock held by AGENT-ID.
Automatically acquires and releases lock."
  (declare (indent 1))
  `(if (agent-game-arbiter-acquire-lock ,agent-id)
       (unwind-protect
           (progn ,@body)
         (agent-game-arbiter-release-lock ,agent-id))
     (error "Agent %s cannot act right now" ,agent-id)))

(defun agent-game-arbiter-validate-turn (agent-id)
  "Validate that it's AGENT-ID's turn.
Throws error if not."
  (unless (agent-game-arbiter-can-act-p agent-id)
    (let ((current (agent-game-state-get 'current-player))
          (winner (agent-game-state-get 'winner)))
      (cond
       (winner
        (error "Game is over, %s won" winner))
       ((not (equal agent-id current))
        (error "Not your turn (current player: %s)" current))
       (agent-game-arbiter-lock
        (error "Another agent is acting (%s)" agent-game-arbiter-lock))
       (t
        (error "Cannot act right now"))))))

(defun agent-game-arbiter-end-turn (agent-id)
  "End AGENT-ID's turn and advance to next player."
  (agent-game-arbiter-validate-turn agent-id)
  (let ((next (agent-game-state-next-player)))
    (agent-game-audit-log 'turn-end
                          `((agent . ,agent-id)
                            (next . ,next)))
    (message "Turn passed to %s" next)
    next))

(provide 'agent-game-arbiter)
;;; agent-game-arbiter.el ends here
