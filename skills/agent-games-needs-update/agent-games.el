;;; agent-games.el --- Multi-agent game validation framework -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: amacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: games, agents, ai

;;; Commentary:

;; Agent Games: Validate multi-agent architecture through progressively
;; complex games (Candyland → Uno → Fiasco).
;;
;; This framework allows multiple AI agents (and humans) to play games
;; together, with each agent having isolated memory and a shared game
;; state mediated by an arbiter.
;;
;; Quick Start:
;;   (require 'agent-games)
;;   M-x candyland-start RET human agent-1 RET
;;
;; See README.org for full documentation.

;;; Code:

;; Add agent-games directories to load-path
(let ((agent-games-dir (file-name-directory
                        (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "core" agent-games-dir))
  (add-to-list 'load-path (expand-file-name "games/candyland" agent-games-dir))
  (add-to-list 'load-path (expand-file-name "games/candyland/skills" agent-games-dir)))

;; Load core modules
(require 'agent-game-state)
(require 'agent-game-arbiter)
(require 'agent-game-audit)
(require 'agent-game-render)
(require 'agent-game-memory)
(require 'agent-game-provider)
(require 'agent-game-agent)

;; Load configuration (if exists)
(let ((config-file (expand-file-name "agent-game-config.el"
                                      (file-name-directory
                                       (or load-file-name buffer-file-name)))))
  (when (file-exists-p config-file)
    (load config-file t)))

;; Load games
(require 'candyland)

;; Load skills (for Candyland)
(require 'dice-roller)
(require 'position-tracker)

;;; Utility Functions

(defun agent-games-version ()
  "Return the version of agent-games."
  (interactive)
  (message "agent-games version 0.2.0 (AI agents enabled)"))

(defun agent-games-status ()
  "Show current game status."
  (interactive)
  (if agent-game-state
      (message "%s" (agent-game-state-to-string))
    (message "No game in progress")))

;;; AI Agent Helpers

;;;###autoload
(defun candyland-start-ai (human-players ai-players &optional provider model)
  "Start Candyland with HUMAN-PLAYERS and AI-PLAYERS.
HUMAN-PLAYERS and AI-PLAYERS are lists of symbols.
PROVIDER defaults to 'openrouter', MODEL defaults to 'anthropic/claude-3.5-sonnet'.

Example:
  (candyland-start-ai '(human) '(claude gpt gemini)
                      \"openrouter\" \"anthropic/claude-3.5-sonnet\")"
  (interactive
   (list
    (let ((input (read-string "Human players (space-separated): " "human")))
      (if (string-empty-p input) nil
        (mapcar #'intern (split-string input))))
    (let ((input (read-string "AI players (space-separated): " "claude")))
      (mapcar #'intern (split-string input)))
    (read-string "Provider (openrouter/openai/ollama): " "openrouter")
    (read-string "Model: " "anthropic/claude-3.5-sonnet")))

  (let ((provider (or provider "openrouter"))
        (model (or model "anthropic/claude-3.5-sonnet"))
        (all-players (append human-players ai-players)))

    ;; Register all players
    (dolist (player human-players)
      (agent-game-agent-register player 'human))

    (dolist (player ai-players)
      (agent-game-agent-register player 'ai provider model))

    ;; Start game
    (candyland-start all-players)

    (message "Game started! Humans: %s | AI (%s/%s): %s | Press SPC for AI turn, TAB for auto-play"
             human-players provider model ai-players)))

;;; Setup

(defun agent-games-setup ()
  "Set up agent-games environment."
  (interactive)
  (let ((root agent-game-memory-root))
    (make-directory (expand-file-name "audit" root) t)
    (make-directory (expand-file-name "games" root) t)
    (message "Agent games initialized at: %s" root)))

;; Auto-setup on load
(agent-games-setup)

(provide 'agent-games)
;;; agent-games.el ends here
