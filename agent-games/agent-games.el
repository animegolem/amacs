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

;; Load games
(require 'candyland)

;; Load skills (for Candyland)
(require 'dice-roller)
(require 'position-tracker)

;;; Utility Functions

(defun agent-games-version ()
  "Return the version of agent-games."
  (interactive)
  (message "agent-games version 0.1.0"))

(defun agent-games-status ()
  "Show current game status."
  (interactive)
  (if agent-game-state
      (message "%s" (agent-game-state-to-string))
    (message "No game in progress")))

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
