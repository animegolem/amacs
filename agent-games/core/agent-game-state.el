;;; agent-game-state.el --- Shared game state management -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: amacs
;; Keywords: games, agents

;;; Commentary:

;; This file provides the shared game state that all agents can read
;; but only the arbiter can modify. This ensures atomic updates and
;; prevents race conditions.

;;; Code:

(require 'cl-lib)

(defvar agent-game-state nil
  "The shared game state.
This is the single source of truth for the current game.
Structure:
  ((type . TYPE)           ; 'candyland, 'uno, 'fiasco
   (phase . PHASE)         ; game-specific phase
   (players . PLAYERS)     ; list of player IDs
   (current-player . ID)   ; whose turn it is
   (board . BOARD-DATA)    ; game-specific board/state
   (history . HISTORY))    ; list of actions taken")

(defun agent-game-state-get (key)
  "Get value of KEY from game state."
  (alist-get key agent-game-state))

(defun agent-game-state-set (key value)
  "Set KEY to VALUE in game state.
Should only be called by arbiter or game logic, not directly by agents."
  (setf (alist-get key agent-game-state) value))

(defun agent-game-state-init (type players)
  "Initialize a new game of TYPE with PLAYERS."
  (setq agent-game-state
        `((type . ,type)
          (phase . setup)
          (players . ,players)
          (current-player . ,(car players))
          (board . nil)
          (history . nil)
          (winner . nil)
          (turn-count . 0))))

(defun agent-game-state-add-history (entry)
  "Add ENTRY to game history."
  (let ((history (agent-game-state-get 'history)))
    (agent-game-state-set 'history (cons entry history))))

(defun agent-game-state-next-player ()
  "Advance to the next player in turn order."
  (let* ((players (agent-game-state-get 'players))
         (current (agent-game-state-get 'current-player))
         (idx (cl-position current players))
         (next-idx (mod (1+ idx) (length players)))
         (next (nth next-idx players)))
    (agent-game-state-set 'current-player next)
    (agent-game-state-set 'turn-count (1+ (agent-game-state-get 'turn-count)))
    next))

(defun agent-game-state-to-string ()
  "Return a human-readable string representation of game state."
  (let ((type (agent-game-state-get 'type))
        (phase (agent-game-state-get 'phase))
        (current (agent-game-state-get 'current-player))
        (turn (agent-game-state-get 'turn-count)))
    (format "Game: %s | Phase: %s | Turn: %d | Current Player: %s"
            type phase turn current)))

(provide 'agent-game-state)
;;; agent-game-state.el ends here
